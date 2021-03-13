{-# LANGUAGE DeriveGeneric #-}

module System.Metrics.Request
  ( Request (..)
  ) where

import           Codec.Serialise (Serialise)
import           Data.List.NonEmpty (NonEmpty)
import           GHC.Generics (Generic)
import           Ouroboros.Network.Util.ShowProxy (ShowProxy(..))

import           System.Metrics.Metric (MetricName)

-- | The request for the new metrics.
-- The acceptor will send this request to the forwarder.
data Request
  = GetAllMetrics                      -- ^ Get all metrics from the forwarder's local store.
  | GetMetrics !(NonEmpty MetricName)  -- ^ Get specific metrics only.
  deriving (Eq, Generic, Show)

instance ShowProxy Request

instance Serialise Request
