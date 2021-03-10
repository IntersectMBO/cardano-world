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
import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async (async, wait)
import           Control.Monad (forM_, when)
import           Control.Tracer (contramap, stdoutTracer)
import qualified Data.ByteString.Lazy as LBS
import           Data.Functor (void)
import           Data.HashMap.Strict (HashMap, (!))
import qualified Data.HashMap.Strict as HM
import           Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import qualified Data.Text as T
import           Data.Void (Void)
import qualified Network.Socket as Socket

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
import           Ouroboros.Network.Codec (Codec)
import           Ouroboros.Network.Protocol.Handshake.Codec (cborTermVersionDataCodec)
import           Ouroboros.Network.Protocol.Handshake.Unversioned (UnversionedProtocol (..),
                                                                   UnversionedProtocolData (..),
                                                                   unversionedHandshakeCodec,
                                                                   unversionedProtocolDataCodec)
import           Ouroboros.Network.Protocol.Handshake.Version (acceptableVersion, simpleSingletonVersions)

import qualified System.Metrics as EKG
import qualified System.Metrics.Counter as C
import qualified System.Metrics.Gauge as G
import qualified System.Metrics.Label as L

import qualified System.Metrics.Internal.Protocol.Acceptor as Acceptor
import qualified System.Metrics.Internal.Protocol.Codec as Acceptor
import qualified System.Metrics.Internal.Protocol.Type as Acceptor
import           System.Metrics.Request (Request (..), MetricName)
import           System.Metrics.Response (Response (..), MetricValue (..))
import           System.Metrics.Configuration (AcceptorConfiguration (..), HowToConnect (..),
                                               RequestFrequency (..), TimePeriod (..),
                                               WhatToRequest (..))

-- | Please note that acceptor is a server from the __networking__ point of view:
-- the forwarder establishes network connection with the acceptor.
--
runEKGAcceptor
  :: AcceptorConfiguration
  -> (Response -> IO ())
  -> EKG.Store
  -> IO ()
runEKGAcceptor config actionOnResponse ekgStore = do
  metricsStore <- newIORef emptyMetricsLocalStore
  void $ runEKGAcceptor' config actionOnResponse ekgStore metricsStore

runEKGAcceptor'
  :: AcceptorConfiguration
  -> (Response -> IO ())
  -> EKG.Store
  -> IORef MetricsLocalStore
  -> IO Void
runEKGAcceptor' config actionOnResponse ekgStore metricsStore = withIOManager $ \iocp ->
  case listenToForwarder config of
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
      MiniProtocol
        { miniProtocolNum    = MiniProtocolNum 2
        , miniProtocolLimits = MiniProtocolLimits {
                                 maximumIngressQueue = maxBound
                               }
        , miniProtocolRun    = acceptEKGMetrics
        }
    ]

  acceptEKGMetrics :: RunMiniProtocol 'ResponderMode LBS.ByteString IO Void ()
  acceptEKGMetrics =
    ResponderProtocolOnly $
      MuxPeer
        (contramap show stdoutTracer)
        codecEKGForward
        (Acceptor.ekgAcceptorPeer $
          ekgAcceptorActions config actionOnResponse ekgStore metricsStore)

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

ekgAcceptorActions
  :: AcceptorConfiguration
  -> (Response -> IO ())
  -> EKG.Store
  -> IORef MetricsLocalStore
  -> Acceptor.EKGAcceptor Request Response IO ()
ekgAcceptorActions config@AcceptorConfiguration {..}
                   actionOnResponse
                   ekgStore
                   metricsStore =
  Acceptor.SendMsgReq request $ \response -> do
    storeResponseInEKG response ekgStore metricsStore
    actionOnResponse response
    threadDelay $ mkDelay requestFrequency
    return $ ekgAcceptorActions config actionOnResponse ekgStore metricsStore
 where
  request =
    case whatToRequest of
      AllMetrics -> GetAllMetrics
      TheseMetrics mNames -> GetMetrics mNames

  -- TODO: temporary function, should be rewritten
  -- (we have to take into account actual time of 'actionOnResponse'
  -- as well as actual time of getting the response from the forwarder).
  mkDelay (AskMetricsEvery delay Seconds)      = fromIntegral delay * 1000000
  mkDelay (AskMetricsEvery delay MilliSeconds) = fromIntegral delay * 1000

storeResponseInEKG
  :: Response
  -> EKG.Store
  -> IORef MetricsLocalStore
  -> IO ()
storeResponseInEKG (ResponseMetrics []) _ _ = return ()
storeResponseInEKG (ResponseMetrics newMetrics) ekgStore metricsStore =
  forM_ newMetrics $ \(mName, mValue) -> do
    storedMetrics <- readIORef metricsStore
    case mValue of
      CounterValue c -> addOrUpdate mName storedMetrics c checkCounter addCounter updateCounter
      GaugeValue g   -> addOrUpdate mName storedMetrics g checkGauge   addGauge   updateGauge
      LabelValue l   -> addOrUpdate mName storedMetrics l checkLabel   addLabel   updateLabel
 where
  addOrUpdate mName storedMetrics v checkIt addIt updateIt =
    if checkIt mName storedMetrics
      then updateIt mName storedMetrics v
      else addIt v mName

  checkCounter mName = HM.member mName . ekgCounters
  checkGauge mName   = HM.member mName . ekgGauges
  checkLabel mName   = HM.member mName . ekgLabels

  updateCounter mName storedMetrics newValue = do
    let counter = ekgCounters storedMetrics ! mName
    currentValue <- C.read counter
    let difference = newValue - currentValue
    when (difference > 0) $
      C.add counter difference

  updateGauge mName storedMetrics newValue = do
    let gauge = ekgGauges storedMetrics ! mName
    currentValue <- G.read gauge
    when (currentValue /= newValue) $
      G.set gauge newValue

  updateLabel mName storedMetrics newValue = do
    let label = ekgLabels storedMetrics ! mName
    currentValue <- L.read label
    when (currentValue /= newValue) $
      L.set label newValue

  addCounter c mName = do
    newCounter <- EKG.createCounter mName ekgStore
    C.add newCounter c
    atomicModifyIORef' metricsStore $ \currentMetricStore ->
      let updatedCounters = HM.insert mName newCounter $ ekgCounters currentMetricStore
      in (currentMetricStore { ekgCounters = updatedCounters }, ())

  addGauge g mName = do
    newGauge <- EKG.createGauge mName ekgStore
    G.set newGauge g
    atomicModifyIORef' metricsStore $ \currentMetricStore ->
      let updatedGauges = HM.insert mName newGauge $ ekgGauges currentMetricStore
      in (currentMetricStore { ekgGauges = updatedGauges }, ())

  addLabel l mName = do
    newLabel <- EKG.createLabel mName ekgStore
    L.set newLabel l
    atomicModifyIORef' metricsStore $ \currentMetricStore ->
      let updatedLabels = HM.insert mName newLabel $ ekgLabels currentMetricStore
      in (currentMetricStore { ekgLabels = updatedLabels }, ())

-- Local storage for metrics. We need it be able to update metrics in EKG.Store
-- by new values we receive from the forwarder.

data MetricsLocalStore = MetricsLocalStore
  { ekgCounters :: !(HashMap MetricName C.Counter)
  , ekgGauges   :: !(HashMap MetricName G.Gauge)
  , ekgLabels   :: !(HashMap MetricName L.Label)
  }

emptyMetricsLocalStore :: MetricsLocalStore
emptyMetricsLocalStore =
  MetricsLocalStore
    { ekgCounters = HM.empty
    , ekgGauges   = HM.empty
    , ekgLabels   = HM.empty
    }
