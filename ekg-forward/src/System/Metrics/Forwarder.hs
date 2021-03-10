{- |
Copyright: (c) 2021 Input Output (Hong Kong) Ltd.
Maintainer: Denis Shevchenko <denis.shevchenko@iohk.io>

See README for more info
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- This top-level module will be used by the forwarder app
-- (the app that collects EKG metrics and sends them to the acceptor).
--
module System.Metrics.Forwarder (
  runEKGForwarder
  ) where

import qualified Codec.Serialise as CBOR
import           Control.Tracer (contramap, stdoutTracer)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Strict as HM
import qualified Data.List.NonEmpty as NE
import           Data.Maybe (mapMaybe)
import qualified Data.Text as T
import           Data.Void (Void)
import qualified Network.Socket as Socket

import           Ouroboros.Network.Mux (MiniProtocol (..), MiniProtocolLimits (..),
                                        MiniProtocolNum (..), MuxMode (..),
                                        OuroborosApplication (..), MuxPeer (..),
                                        RunMiniProtocol (..),
                                        miniProtocolLimits, miniProtocolNum, miniProtocolRun)
import           Ouroboros.Network.IOManager (withIOManager)
import           Ouroboros.Network.Snocket (localAddressFromPath, localSnocket, socketSnocket)
import           Ouroboros.Network.Socket (connectToNode, nullNetworkConnectTracers)
import           Ouroboros.Network.Codec (Codec)
import           Ouroboros.Network.Protocol.Handshake.Codec (cborTermVersionDataCodec)
import           Ouroboros.Network.Protocol.Handshake.Unversioned (UnversionedProtocol (..),
                                                                   UnversionedProtocolData (..),
                                                                   unversionedHandshakeCodec,
                                                                   unversionedProtocolDataCodec)
import           Ouroboros.Network.Protocol.Handshake.Version (acceptableVersion, simpleSingletonVersions)

import qualified System.Metrics as EKG

import qualified System.Metrics.Internal.Protocol.Forwarder as Forwarder
import qualified System.Metrics.Internal.Protocol.Codec as Forwarder
import qualified System.Metrics.Internal.Protocol.Type as Forwarder
import           System.Metrics.Request (Request (..))
import           System.Metrics.Response (Response (..), MetricValue (..))
import           System.Metrics.Configuration (ForwarderConfiguration (..), HowToConnect (..))

runEKGForwarder
  :: ForwarderConfiguration
  -> EKG.Store
  -> IO ()
runEKGForwarder ForwarderConfiguration{..} ekgStore = withIOManager $ \iocp ->
  case connectToAcceptor of
    LocalPipe localPipe ->
      connectToNode
        (localSnocket iocp localPipe)
        unversionedHandshakeCodec
        (cborTermVersionDataCodec unversionedProtocolDataCodec)
        nullNetworkConnectTracers
        acceptableVersion
        (simpleSingletonVersions
           UnversionedProtocol
           UnversionedProtocolData
           app)
        Nothing
        (localAddressFromPath localPipe)
    RemoteSocket host port -> do
      acceptorAddress:_ <- Socket.getAddrInfo Nothing (Just $ T.unpack host) (Just $ show port)
      connectToNode
        (socketSnocket iocp)
        unversionedHandshakeCodec
        (cborTermVersionDataCodec unversionedProtocolDataCodec)
        nullNetworkConnectTracers
        acceptableVersion
        (simpleSingletonVersions
           UnversionedProtocol
           UnversionedProtocolData
           app)
        Nothing
        (Socket.addrAddress acceptorAddress)
 where
  app :: OuroborosApplication 'InitiatorMode addr LBS.ByteString IO () Void
  app =
    OuroborosApplication $ \_connectionId _shouldStopSTM -> [
        MiniProtocol
          { miniProtocolNum    = MiniProtocolNum 2
          , miniProtocolLimits = MiniProtocolLimits
                                   { maximumIngressQueue = maxBound
                                   }
          , miniProtocolRun    = forwardEKGMetrics
        }
      ]

  forwardEKGMetrics :: RunMiniProtocol 'InitiatorMode LBS.ByteString IO () Void
  forwardEKGMetrics =
    InitiatorProtocolOnly $
      MuxPeer
        (contramap show stdoutTracer)
        codecEKGForward
        (Forwarder.ekgForwarderPeer (ekgForwarderActions ekgStore))

codecEKGForward
  :: ( CBOR.Serialise req
     , CBOR.Serialise resp
     )
  => Codec (Forwarder.EKGForward req resp)
           CBOR.DeserialiseFailure
           IO LBS.ByteString
codecEKGForward =
  Forwarder.codecEKGForward
    CBOR.encode CBOR.decode
    CBOR.encode CBOR.decode

ekgForwarderActions
  :: EKG.Store
  -> Forwarder.EKGForwarder Request Response IO ()
ekgForwarderActions ekgStore =
  Forwarder.EKGForwarder {
    Forwarder.recvMsgReq = \request -> do
      allMetrics <- HM.toList <$> EKG.sampleAll ekgStore
      case request of
        GetAllMetrics -> do
          let supportedMetrics = mapMaybe filterMetrics allMetrics
          return ( ResponseMetrics supportedMetrics
                 , ekgForwarderActions ekgStore
                 )
        GetMetrics metricsNames -> do
          let mNames = NE.toList metricsNames
              metricsWeNeed = mapMaybe (filterMetricsWeNeed mNames) allMetrics
          return ( ResponseMetrics metricsWeNeed
                 , ekgForwarderActions ekgStore
                 )
  , Forwarder.recvMsgDone = return ()
  }
 where
  -- TODO: temporary functions. We have to remove unsupported metrics.
  filterMetrics (_,     EKG.Distribution _) = Nothing
  filterMetrics (mName, EKG.Counter c)      = Just (mName, CounterValue c)
  filterMetrics (mName, EKG.Gauge g)        = Just (mName, GaugeValue g)
  filterMetrics (mName, EKG.Label l)        = Just (mName, LabelValue l)

  filterMetricsWeNeed _      (_,     EKG.Distribution _) = Nothing
  filterMetricsWeNeed mNames (mName, EKG.Counter c)      = onlyIfNeeded mNames mName (CounterValue c)
  filterMetricsWeNeed mNames (mName, EKG.Gauge g)        = onlyIfNeeded mNames mName (GaugeValue g)
  filterMetricsWeNeed mNames (mName, EKG.Label l)        = onlyIfNeeded mNames mName (LabelValue l)

  onlyIfNeeded mNames mName value =
    if mName `elem` mNames then Just (mName, value) else Nothing
