{-# LANGUAGE ViewPatterns #-}

module System.Metrics.Forwarder.Store
  ( mkResponse
  ) where

import qualified Data.HashMap.Strict as HM
import qualified Data.List.NonEmpty as NE
import           Data.Maybe (mapMaybe)

import qualified System.Metrics as EKG

import qualified System.Metrics.Internal.Protocol.Forwarder as Forwarder
import           System.Metrics.Request (Request (..), MetricName)
import           System.Metrics.Response (Response (..), MetricValue (..))

mkResponse
  :: EKG.Store
  -> Forwarder.EKGForwarder Request Response IO ()
mkResponse ekgStore =
  Forwarder.EKGForwarder
    { Forwarder.recvMsgReq = \request -> do
        allMetrics <- HM.toList <$> EKG.sampleAll ekgStore
        case request of
          GetAllMetrics -> do
            let supportedMetrics = mapMaybe filterMetrics allMetrics
            return ( ResponseMetrics supportedMetrics
                   , mkResponse ekgStore
                   )
          GetMetrics (NE.toList -> mNames) -> do
            let metricsWeNeed = mapMaybe (filterMetricsWeNeed mNames) allMetrics
            return ( ResponseMetrics metricsWeNeed
                   , mkResponse ekgStore
                   )
    , Forwarder.recvMsgDone = return ()
    }

filterMetrics
  :: (MetricName, EKG.Value)
  -> Maybe (MetricName, MetricValue)
filterMetrics (mName, ekgValue) =
  case ekgValue of
    EKG.Distribution _ -> Nothing  -- Distribution does not supported yet.
    EKG.Counter c      -> Just (mName, CounterValue c)
    EKG.Gauge g        -> Just (mName, GaugeValue g)
    EKG.Label l        -> Just (mName, LabelValue l)

filterMetricsWeNeed
  :: [MetricName]
  -> (MetricName, EKG.Value)
  -> Maybe (MetricName, MetricValue)
filterMetricsWeNeed mNames (mName, ekgValue) =
  case ekgValue of
    EKG.Distribution _ -> Nothing  -- Distribution does not supported yet.
    EKG.Counter c      -> onlyIfNeeded $ CounterValue c
    EKG.Gauge g        -> onlyIfNeeded $ GaugeValue g
    EKG.Label l        -> onlyIfNeeded $ LabelValue l
 where
  onlyIfNeeded value =
    if mName `elem` mNames then Just (mName, value) else Nothing
