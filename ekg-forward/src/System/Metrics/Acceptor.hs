{- |
Copyright: (c) 2021 Input Output (Hong Kong) Ltd.
Maintainer: Denis Shevchenko <denis.shevchenko@iohk.io>

See README for more info
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

-- | This top-level module will be used by the acceptor app
-- (the app that asks EKG metrics from the forwarder app).
--
module System.Metrics.Acceptor (
  runEKGAcceptor
  ) where

import qualified Codec.Serialise as CBOR
import           Control.Concurrent.Async (async, wait)
import           Control.Tracer (contramap, stdoutTracer)
import qualified Data.ByteString.Lazy as LBS
import           Data.Functor (void)
import qualified Data.Text as T
import           Data.Void (Void)
import qualified Network.Socket as Socket

import           Ouroboros.Network.Mux (MiniProtocol (..), MiniProtocolLimits (..),
                                        MiniProtocolNum (..), MuxMode (..),
                                        OuroborosApplication (..), MuxPeer (..),
                                        RunMiniProtocol (..),
                                        miniProtocolLimits, miniProtocolNum, miniProtocolRun)
import           Ouroboros.Network.NodeToNode (nullErrorPolicies, simpleSingletonVersions)
import           Ouroboros.Network.IOManager (withIOManager)
import           Ouroboros.Network.Snocket (localAddressFromPath, localSnocket, socketSnocket)
import           Ouroboros.Network.Socket (AcceptedConnectionsLimit (..),
                                           SomeResponderApplication (..),
                                           cleanNetworkMutableState, newNetworkMutableState,
                                           nullNetworkServerTracers, withServerNode)
import           Ouroboros.Network.Codec (Codec)
import           Ouroboros.Network.Protocol.Handshake.Codec (cborTermVersionDataCodec)
import           Ouroboros.Network.Protocol.Handshake.Unversioned (UnversionedProtocol (..),
                                                                   UnversionedProtocolData (..),
                                                                   unversionedHandshakeCodec,
                                                                   unversionedProtocolDataCodec)
import           Ouroboros.Network.Protocol.Handshake.Version (acceptableVersion)

import qualified System.Metrics.Internal.Protocol.Acceptor as Acceptor
import qualified System.Metrics.Internal.Protocol.Codec as Acceptor
import qualified System.Metrics.Internal.Protocol.Type as Acceptor
import           System.Metrics.Internal.Request (Request (..))
import           System.Metrics.Internal.Response (Response (..))
import           System.Metrics.Configuration (AcceptorConfiguration (..), HowToConnect (..))

-- | Please note that acceptor is a server from the __networking__ point of view:
-- the forwarder establishes network connection with the acceptor.
--
runEKGAcceptor :: AcceptorConfiguration -> IO ()
runEKGAcceptor = void . runEKGAcceptor'

runEKGAcceptor' :: AcceptorConfiguration -> IO Void
runEKGAcceptor' AcceptorConfiguration {..} = withIOManager $ \iocp ->
  case listenToForwarder of
    LocalPipe localPipe -> do
      networkState <- newNetworkMutableState
      _ <- async $ cleanNetworkMutableState networkState
      withServerNode
        (localSnocket iocp localPipe)
        nullNetworkServerTracers
        networkState
        (AcceptedConnectionsLimit maxBound maxBound 0)
        (localAddressFromPath localPipe)
        unversionedHandshakeCodec
        (cborTermVersionDataCodec unversionedProtocolDataCodec)
        acceptableVersion
        (simpleSingletonVersions
          UnversionedProtocol
          UnversionedProtocolData
          (SomeResponderApplication app))
        nullErrorPolicies
        $ \_ serverAsync -> wait serverAsync -- Block until async exception.
    RemoteSocket host port -> do
      listenAddress:_ <- Socket.getAddrInfo Nothing (Just $ T.unpack host) (Just $ show port)
      networkState <- newNetworkMutableState
      _ <- async $ cleanNetworkMutableState networkState
      withServerNode
        (socketSnocket iocp)
        nullNetworkServerTracers
        networkState
        (AcceptedConnectionsLimit maxBound maxBound 0)
        (Socket.addrAddress listenAddress)
        unversionedHandshakeCodec
        (cborTermVersionDataCodec unversionedProtocolDataCodec)
        acceptableVersion
        (simpleSingletonVersions
          UnversionedProtocol
          UnversionedProtocolData
          (SomeResponderApplication app))
        nullErrorPolicies
        $ \_ serverAsync -> wait serverAsync -- Block until async exception.
 where
  app :: OuroborosApplication 'ResponderMode addr LBS.ByteString IO Void ()
  app =
    OuroborosApplication $ \_connectionId _shouldStopSTM -> [
      MiniProtocol {
        miniProtocolNum    = MiniProtocolNum 2,
        miniProtocolLimits = maximumMiniProtocolLimits,
        miniProtocolRun    = acceptEKGMetrics
      }
    ]

  acceptEKGMetrics :: RunMiniProtocol 'ResponderMode LBS.ByteString IO Void ()
  acceptEKGMetrics =
    ResponderProtocolOnly $
      MuxPeer
        (contramap show stdoutTracer)
        codecEKGForward
        (Acceptor.ekgAcceptorPeer (ekgAcceptorActions 0))

codecEKGForward
  :: ( CBOR.Serialise req
     , CBOR.Serialise resp
     )
  => Codec (Acceptor.EKGForward req resp)
           CBOR.DeserialiseFailure
           IO LBS.ByteString
codecEKGForward =
  Acceptor.codecEKGForward
    CBOR.encode CBOR.decode
    CBOR.encode CBOR.decode

ekgAcceptorActions :: Int -> Acceptor.EKGAcceptor Request Response IO ()
ekgAcceptorActions 0 = Acceptor.SendMsgReq GetAllMetrics (\resp -> do
                                                         print resp
                                                         return (ekgAcceptorActions 1))
ekgAcceptorActions _ = Acceptor.SendMsgDone (return ())

maximumMiniProtocolLimits :: MiniProtocolLimits
maximumMiniProtocolLimits =
    MiniProtocolLimits {
      maximumIngressQueue = maxBound
    }
