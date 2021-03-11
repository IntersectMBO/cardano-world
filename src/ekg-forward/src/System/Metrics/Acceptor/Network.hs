
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

module System.Metrics.Acceptor.Network
  ( listenToForwarder
  ) where

import qualified Codec.Serialise as CBOR
import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async (async, wait)
import           Control.Tracer (contramap, stdoutTracer)
import qualified Data.ByteString.Lazy as LBS
import           Data.IORef (IORef)
import qualified Data.Text as T
import           Data.Void (Void)
import qualified Network.Socket as Socket
import qualified System.Metrics as EKG

import           Ouroboros.Network.Mux (MiniProtocol (..), MiniProtocolLimits (..),
                                        MiniProtocolNum (..), MuxMode (..),
                                        OuroborosApplication (..), MuxPeer (..),
                                        RunMiniProtocol (..),
                                        miniProtocolLimits, miniProtocolNum, miniProtocolRun)
import           Ouroboros.Network.ErrorPolicy (nullErrorPolicies)
import           Ouroboros.Network.IOManager (withIOManager)
import           Ouroboros.Network.Snocket (localAddressFromPath, localSnocket, socketSnocket)
import           Ouroboros.Network.Socket (AcceptedConnectionsLimit (..),
                                           SomeResponderApplication (..),
                                           cleanNetworkMutableState, newNetworkMutableState,
                                           nullNetworkServerTracers, withServerNode)
import           Ouroboros.Network.Protocol.Handshake.Codec (cborTermVersionDataCodec)
import           Ouroboros.Network.Protocol.Handshake.Unversioned (UnversionedProtocol (..),
                                                                   UnversionedProtocolData (..),
                                                                   unversionedHandshakeCodec,
                                                                   unversionedProtocolDataCodec)
import           Ouroboros.Network.Protocol.Handshake.Version (acceptableVersion, simpleSingletonVersions)

import qualified System.Metrics.Internal.Protocol.Acceptor as Acceptor
import qualified System.Metrics.Internal.Protocol.Codec as Acceptor
import           System.Metrics.Acceptor.Store (MetricsLocalStore (..), storeMetrics)
import           System.Metrics.Request (Request (..))
import           System.Metrics.Response (Response (..))
import           System.Metrics.Configuration (AcceptorConfiguration (..), HowToConnect (..),
                                               Frequency (..), TimePeriod (..))

listenToForwarder
  :: AcceptorConfiguration
  -> EKG.Store
  -> IORef MetricsLocalStore
  -> IO Void
listenToForwarder config ekgStore metricsStore = withIOManager $ \iocp ->
  case forwarderEndpoint config of
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
          (SomeResponderApplication $ acceptorApp config ekgStore metricsStore))
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
          (SomeResponderApplication $ acceptorApp config ekgStore metricsStore))
        nullErrorPolicies
        $ \_ serverAsync -> wait serverAsync -- Block until async exception.

acceptorApp
  :: AcceptorConfiguration
  -> EKG.Store
  -> IORef MetricsLocalStore
  -> OuroborosApplication 'ResponderMode addr LBS.ByteString IO Void ()
acceptorApp config ekgStore metricsStore =
  OuroborosApplication $ \_connectionId _shouldStopSTM -> [
    MiniProtocol
      { miniProtocolNum    = MiniProtocolNum 2
      , miniProtocolLimits = MiniProtocolLimits {
                               maximumIngressQueue = maxBound
                             }
      , miniProtocolRun    = acceptEKGMetrics config ekgStore metricsStore
      }
  ]

acceptEKGMetrics
  :: AcceptorConfiguration
  -> EKG.Store
  -> IORef MetricsLocalStore
  -> RunMiniProtocol 'ResponderMode LBS.ByteString IO Void ()
acceptEKGMetrics config ekgStore metricsStore =
  ResponderProtocolOnly $
    MuxPeer
      (contramap show stdoutTracer)
      (Acceptor.codecEKGForward CBOR.encode CBOR.decode
                                CBOR.encode CBOR.decode)
      (Acceptor.ekgAcceptorPeer $ acceptorActions config ekgStore metricsStore)

acceptorActions
  :: AcceptorConfiguration
  -> EKG.Store
  -> IORef MetricsLocalStore
  -> Acceptor.EKGAcceptor Request Response IO ()
acceptorActions config@AcceptorConfiguration {..} ekgStore metricsStore =
  Acceptor.SendMsgReq whatToRequest $ \response -> do
    storeMetrics response ekgStore metricsStore
    actionOnResponse response
    threadDelay $ mkDelay requestFrequency
    return $ acceptorActions config ekgStore metricsStore
 where
  -- TODO: temporary function, should be rewritten
  -- (we have to take into account actual time of 'actionOnResponse'
  -- as well as actual time of getting the response from the forwarder).
  mkDelay (Every delay Seconds)      = fromIntegral delay * 1000000
  mkDelay (Every delay MilliSeconds) = fromIntegral delay * 1000
