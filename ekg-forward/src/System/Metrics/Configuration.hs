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
  , Port
  , RequestFrequency (..)
  , TimePeriod (..)
  ) where

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

data RequestFrequency = AskMetricsEvery !Word64 !TimePeriod
  deriving (Eq, Show)

data AcceptorConfiguration = AcceptorConfiguration
  { listenToForwarder :: !HowToConnect
  , requestFrequency  :: !RequestFrequency
  } deriving (Eq, Show)

data ForwarderConfiguration = ForwarderConfiguration
  { connectToAcceptor :: !HowToConnect
  } deriving (Eq, Show)
