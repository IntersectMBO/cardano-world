{-# LANGUAGE DeriveGeneric #-}

module System.Metrics.Metric
  ( MetricName
  , MetricValue (..)
  ) where

import           Codec.Serialise (Serialise)
import           Data.Int (Int64)
import           Data.Text (Text)
import           GHC.Generics (Generic)
import           Ouroboros.Network.Util.ShowProxy (ShowProxy(..))

-- | Each EKG metric has a unique name.
type MetricName = Text

-- | Metric value that will be transmitted from the forwarder to the acceptor.
-- Please note that EKG.Distribution is not supported yet.
data MetricValue
  = CounterValue !Int64  -- ^ Counter value.
  | GaugeValue   !Int64  -- ^ Gauge value.
  | LabelValue   !Text   -- ^ Text label.
  deriving (Eq, Show, Generic)

instance ShowProxy MetricValue

instance Serialise MetricValue
