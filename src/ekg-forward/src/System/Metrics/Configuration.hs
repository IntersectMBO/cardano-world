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
  ) where

import           Control.Tracer (Tracer)
import           Data.Text (Text)
import           Data.Time.Clock (NominalDiffTime)
import           Data.Word (Word16, Word64)

import           Ouroboros.Network.Driver (TraceSendRecv)

import           System.Metrics.Protocol.Type (EKGForward)
import           System.Metrics.Response (Response)
import           System.Metrics.Request (Request)

type Host = Text
type Port = Word16
type MetricName = Text

data HowToConnect
  = LocalPipe    !FilePath
  | RemoteSocket !Host !Port

data AcceptorConfiguration = AcceptorConfiguration
  { acceptorTracer    :: !(Tracer IO (TraceSendRecv (EKGForward Request Response)))
  , forwarderEndpoint :: !HowToConnect
  , requestFrequency  :: !NominalDiffTime
  , whatToRequest     :: !Request
  , actionOnResponse  :: Response -> IO ()
  }

data ForwarderConfiguration = ForwarderConfiguration
  { forwarderTracer    :: !(Tracer IO (TraceSendRecv (EKGForward Request Response)))
  , acceptorEndpoint   :: !HowToConnect
  , reConnectFrequency :: !NominalDiffTime
  }
