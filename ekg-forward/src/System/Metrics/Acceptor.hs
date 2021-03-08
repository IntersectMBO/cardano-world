{- |
Copyright: (c) 2021 Input Output (Hong Kong) Ltd.
Maintainer: Denis Shevchenko <denis.shevchenko@iohk.io>

See README for more info
-}

{-# LANGUAGE DataKinds #-}

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
import           Data.Void (Void)

import           Ouroboros.Network.Mux (MiniProtocol (..), MiniProtocolLimits (..),
                                        MiniProtocolNum (..), MuxMode (..),
                                        OuroborosApplication (..), MuxPeer (..),
                                        RunMiniProtocol (..),
                                        miniProtocolLimits, miniProtocolNum, miniProtocolRun)
import           Ouroboros.Network.NodeToNode (nullErrorPolicies, simpleSingletonVersions)
import           Ouroboros.Network.IOManager (withIOManager)
import           Ouroboros.Network.Snocket (localAddressFromPath, localSnocket)
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

-- | Please note that acceptor is a server from the __networking__ point of view:
-- the forwarder establishes network connection with the acceptor.
--
runEKGAcceptor :: FilePath -> IO Void
runEKGAcceptor sockAddr = withIOManager $ \iocp -> do
    networkState <- newNetworkMutableState
    _ <- async $ cleanNetworkMutableState networkState
    withServerNode
      (localSnocket iocp defaultLocalSocketAddrPath)
      nullNetworkServerTracers
      networkState
      (AcceptedConnectionsLimit maxBound maxBound 0)
      (localAddressFromPath sockAddr)
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

defaultLocalSocketAddrPath :: FilePath
defaultLocalSocketAddrPath =  "./demo-ekg-forward.sock"

maximumMiniProtocolLimits :: MiniProtocolLimits
maximumMiniProtocolLimits =
    MiniProtocolLimits {
      maximumIngressQueue = maxBound
    }
