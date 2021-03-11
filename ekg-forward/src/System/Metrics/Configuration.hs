{- |
Copyright: (c) 2021 Input Output (Hong Kong) Ltd.
Maintainer: Denis Shevchenko <denis.shevchenko@iohk.io>

See README for more info
-}

--
module System.Metrics.Configuration (
    AcceptorConfiguration (..)
  , ForwarderConfiguration (..)
  , HowToConnect (..)
  , Host
  , MetricName
  , Port
  , Frequency (..)
  , TimePeriod (..)
  , WhatToRequest (..)
  ) where

import           Data.List.NonEmpty (NonEmpty)
import           Data.Text (Text)
import           Data.Word (Word16, Word64)

type Host = Text
type Port = Word16

data HowToConnect
  = LocalPipe    !FilePath
  | RemoteSocket !Host !Port
  deriving (Eq, Show)

data TimePeriod = Seconds | MilliSeconds
  deriving (Eq, Show)

data Frequency = Every !Word64 !TimePeriod
  deriving (Eq, Show)

type MetricName = Text

data WhatToRequest
  = AllMetrics
  | TheseMetrics !(NonEmpty MetricName)
  deriving (Eq, Show)

data AcceptorConfiguration = AcceptorConfiguration
  { listenToForwarder :: !HowToConnect
  , requestFrequency  :: !Frequency
  , whatToRequest     :: !WhatToRequest
  } deriving (Eq, Show)

data ForwarderConfiguration = ForwarderConfiguration
  { acceptorEndpoint   :: !HowToConnect
  , reConnectFrequency :: !Frequency
  } deriving (Eq, Show)
