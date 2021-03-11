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
  ) where

import           Data.Text (Text)
import           Data.Word (Word16, Word64)

import           System.Metrics.Response (Response)
import           System.Metrics.Request (Request)

type Host = Text
type Port = Word16

data HowToConnect
  = LocalPipe    !FilePath
  | RemoteSocket !Host !Port

data TimePeriod = Seconds | MilliSeconds

data Frequency = Every !Word64 !TimePeriod

type MetricName = Text

data AcceptorConfiguration = AcceptorConfiguration
  { forwarderEndpoint :: !HowToConnect
  , requestFrequency  :: !Frequency
  , whatToRequest     :: !Request
  , actionOnResponse  :: Response -> IO ()
  }

data ForwarderConfiguration = ForwarderConfiguration
  { acceptorEndpoint   :: !HowToConnect
  , reConnectFrequency :: !Frequency
  }
