module Test.MkConfig
  ( mkAcceptorConfig
  , mkForwarderConfig
  ) where

import           Control.Tracer (nullTracer)
import           Data.Time.Clock (secondsToNominalDiffTime)

import           System.Metrics.Configuration (AcceptorConfiguration (..),
                                               ForwarderConfiguration (..),
                                               HowToConnect (..))
import           System.Metrics.Request (Request (..))

mkAcceptorConfig
  :: HowToConnect
  -> Request
  -> AcceptorConfiguration
mkAcceptorConfig endpoint request =
  AcceptorConfiguration
    { acceptorTracer    = nullTracer
    , forwarderEndpoint = endpoint
    , requestFrequency  = secondsToNominalDiffTime 1
    , whatToRequest     = request
    , actionOnResponse  = \_ -> return ()
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
