{-# LANGUAGE DeriveGeneric #-}

module System.Metrics.Response 
  ( Response (..)
  ) where

import           Codec.Serialise (Serialise)
import           GHC.Generics (Generic)
import           Ouroboros.Network.Util.ShowProxy (ShowProxy(..))

import           System.Metrics.Metric (MetricName, MetricValue)

-- | The response with the metrics.
-- The forwarder will send it to the acceptor as a reply for the request.
-- Please note that the list of metrics can be empty (for example, if the
-- forwarder's local store is empty).
newtype Response = ResponseMetrics [(MetricName, MetricValue)]
  deriving (Generic, Show)

instance ShowProxy Response

instance Serialise Response
