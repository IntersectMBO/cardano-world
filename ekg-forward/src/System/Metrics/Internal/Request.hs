{- |
Copyright: (c) 2021 Input Output (Hong Kong) Ltd.
Maintainer: Denis Shevchenko <denis.shevchenko@iohk.io>

See README for more info
-}

{-# LANGUAGE DeriveGeneric #-}

--
module System.Metrics.Internal.Request (
    MetricName
  , Request (..)
  ) where

import           Codec.Serialise (Serialise)
import           Data.List.NonEmpty (NonEmpty)
import           Data.Text (Text)
import           GHC.Generics (Generic)

import           Ouroboros.Network.Util.ShowProxy (ShowProxy(..))

type MetricName = Text

data Request
  = GetAllMetrics
  | GetMetrics !(NonEmpty MetricName)
  deriving (Eq, Generic, Show)

instance ShowProxy Request

instance Serialise Request
