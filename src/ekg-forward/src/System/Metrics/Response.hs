{- |
Copyright: (c) 2021 Input Output (Hong Kong) Ltd.
Maintainer: Denis Shevchenko <denis.shevchenko@iohk.io>

See README for more info
-}

{-# LANGUAGE DeriveGeneric #-}

--
module System.Metrics.Response ( 
    MetricName
  , MetricValue (..)
  , Response (..)
  ) where

import           Codec.Serialise (Serialise)
import           Data.Int (Int64)
import           Data.Text (Text)
import           GHC.Generics (Generic)

import           Ouroboros.Network.Util.ShowProxy (ShowProxy(..))

type MetricName = Text

data MetricValue
  = CounterValue !Int64
  | GaugeValue   !Int64
  | LabelValue   !Text
  deriving (Eq, Show, Generic)

instance ShowProxy MetricValue

instance Serialise MetricValue

newtype Response = ResponseMetrics [(MetricName, MetricValue)]
  deriving (Generic, Show)

instance ShowProxy Response

instance Serialise Response
