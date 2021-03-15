module Test.MkConfig
  ( mkAcceptorConfig
  , mkForwarderConfig
  ) where

import           Control.Tracer (nullTracer)
import           Data.IORef (IORef)
import           Data.Time.Clock (secondsToNominalDiffTime)

import           System.Metrics.Configuration (AcceptorConfiguration (..),
                                               ForwarderConfiguration (..),
                                               HowToConnect (..))
import           System.Metrics.ReqResp (Request (..))

mkAcceptorConfig
  :: HowToConnect
  -> IORef Bool
  -> Request
  -> AcceptorConfiguration
mkAcceptorConfig endpoint weAreDone request =
  AcceptorConfiguration
    { acceptorTracer    = nullTracer
    , forwarderEndpoint = endpoint
    , requestFrequency  = secondsToNominalDiffTime 1
    , whatToRequest     = request
    , actionOnResponse  = \_ -> return ()
    , shouldWeStop      = weAreDone
    , actionOnDone      = putStrLn "Acceptor: we are done!"
    }

mkForwarderConfig
  :: HowToConnect
  -> ForwarderConfiguration
mkForwarderConfig endpoint =
  ForwarderConfiguration
    { forwarderTracer    = nullTracer
    , acceptorEndpoint   = endpoint
    , reConnectFrequency = secondsToNominalDiffTime 1
    , actionOnRequest    = \_ -> return ()
    }
