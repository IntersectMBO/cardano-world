{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

module System.Metrics.Network.Acceptor
  ( listenToForwarder
  -- | Export this function for Mux purpose.
  , acceptEKGMetrics
  , acceptEKGMetricsInit
  ) where

import           Codec.CBOR.Term (Term)
import qualified Codec.Serialise as CBOR
import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async (async, wait)
import qualified Data.ByteString.Lazy as LBS
import           Data.IORef (IORef, readIORef)
import qualified Data.Text as T
import           Data.Time.Clock (NominalDiffTime)
import           Data.Void (Void)
import qualified Network.Socket as Socket
import           Ouroboros.Network.Driver.Limits (ProtocolTimeLimits)
import           Ouroboros.Network.ErrorPolicy (nullErrorPolicies)
import           Ouroboros.Network.IOManager (withIOManager)
import           Ouroboros.Network.Mux (MiniProtocol (..), MiniProtocolLimits (..),
                                        MiniProtocolNum (..), MuxMode (..),
                                        OuroborosApplication (..), MuxPeer (..),
                                        RunMiniProtocol (..),
                                        miniProtocolLimits, miniProtocolNum, miniProtocolRun)
import           Ouroboros.Network.Snocket (Snocket, localAddressFromPath, localSnocket, socketSnocket)
import           Ouroboros.Network.Socket (AcceptedConnectionsLimit (..),
                                           SomeResponderApplication (..),
                                           cleanNetworkMutableState, newNetworkMutableState,
                                           nullNetworkServerTracers, withServerNode)
import           Ouroboros.Network.Protocol.Handshake.Codec (cborTermVersionDataCodec,
                                                             noTimeLimitsHandshake,
                                                             timeLimitsHandshake)
import           Ouroboros.Network.Protocol.Handshake.Type (Handshake)
import           Ouroboros.Network.Protocol.Handshake.Unversioned (UnversionedProtocol (..),
                                                                   UnversionedProtocolData (..),
                                                                   unversionedHandshakeCodec,
                                                                   unversionedProtocolDataCodec)
import           Ouroboros.Network.Protocol.Handshake.Version (acceptableVersion, simpleSingletonVersions)
import qualified System.Metrics as EKG

import qualified System.Metrics.Protocol.Acceptor as Acceptor
import qualified System.Metrics.Protocol.Codec as Acceptor
import           System.Metrics.Store.Acceptor (MetricsLocalStore (..), storeMetrics)
import           System.Metrics.ReqResp (Request (..), Response (..))
import           System.Metrics.Configuration (AcceptorConfiguration (..), HowToConnect (..))

listenToForwarder
  :: AcceptorConfiguration
  -> EKG.Store
  -> IORef MetricsLocalStore
  -> IO Void
listenToForwarder config ekgStore metricsStore = withIOManager $ \iocp -> do
  let app = acceptorApp config ekgStore metricsStore
  case forwarderEndpoint config of
    LocalPipe localPipe -> do
      let snocket = localSnocket iocp localPipe
          address = localAddressFromPath localPipe
      doListenToForwarder snocket address noTimeLimitsHandshake app
    RemoteSocket host port -> do
      listenAddress:_ <- Socket.getAddrInfo Nothing (Just $ T.unpack host) (Just $ show port)
      let snocket = socketSnocket iocp
          address = Socket.addrAddress listenAddress
      doListenToForwarder snocket address timeLimitsHandshake app

doListenToForwarder
  :: Ord addr
  => Snocket IO fd addr
  -> addr
  -> ProtocolTimeLimits (Handshake UnversionedProtocol Term)
  -> OuroborosApplication 'ResponderMode addr LBS.ByteString IO Void ()
  -> IO Void
doListenToForwarder snocket address timeLimits app = do
  networkState <- newNetworkMutableState
  _ <- async $ cleanNetworkMutableState networkState
  withServerNode
    snocket
    nullNetworkServerTracers
    networkState
    (AcceptedConnectionsLimit maxBound maxBound 0)
    address
    unversionedHandshakeCodec
    timeLimits
    (cborTermVersionDataCodec unversionedProtocolDataCodec)
    acceptableVersion
    (simpleSingletonVersions
      UnversionedProtocol
      UnversionedProtocolData
      (SomeResponderApplication app))
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
      (acceptorTracer config)
      (Acceptor.codecEKGForward CBOR.encode CBOR.decode
                                CBOR.encode CBOR.decode)
      (Acceptor.ekgAcceptorPeer $ acceptorActions True config ekgStore metricsStore)

acceptEKGMetricsInit
  :: AcceptorConfiguration
  -> EKG.Store
  -> IORef MetricsLocalStore
  -> RunMiniProtocol 'InitiatorMode LBS.ByteString IO () Void
acceptEKGMetricsInit config ekgStore metricsStore =
  InitiatorProtocolOnly $
    MuxPeer
      (acceptorTracer config)
      (Acceptor.codecEKGForward CBOR.encode CBOR.decode
                                CBOR.encode CBOR.decode)
      (Acceptor.ekgAcceptorPeer $ acceptorActions True config ekgStore metricsStore)

acceptorActions
  :: Bool
  -> AcceptorConfiguration
  -> EKG.Store
  -> IORef MetricsLocalStore
  -> Acceptor.EKGAcceptor Request Response IO ()
acceptorActions True config@AcceptorConfiguration{..} ekgStore metricsStore =
  Acceptor.SendMsgReq whatToRequest $ \response -> do
    storeMetrics response ekgStore metricsStore
    actionOnResponse response
    threadDelay $ toMicroSecs requestFrequency
    weAreDone <- readIORef shouldWeStop
    if weAreDone
      then return $ acceptorActions False config ekgStore metricsStore
      else return $ acceptorActions True  config ekgStore metricsStore
 where
  -- TODO: temporary function, should be rewritten
  -- (we have to take into account actual time of 'actionOnResponse'
  -- as well as actual time of getting the response from the forwarder).
  toMicroSecs :: NominalDiffTime -> Int
  toMicroSecs dt = fromEnum dt `div` 1000000
acceptorActions False AcceptorConfiguration{..} _ _ =
  Acceptor.SendMsgDone $
    actionOnDone
