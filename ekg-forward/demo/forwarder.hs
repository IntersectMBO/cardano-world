{-# LANGUAGE LambdaCase #-}

import           Control.Tracer (contramap, stdoutTracer)
import           Data.Text (pack)
import           Data.Word (Word64)
import           System.Environment (getArgs)
import           System.Exit (die)

import qualified System.Metrics as EKG

import           System.Metrics.Forwarder (runEKGForwarder)
import           System.Metrics.Configuration (ForwarderConfiguration (..),
                                               Frequency (..), HowToConnect (..),
                                               Port, TimePeriod (..))

main :: IO ()
main = do
  -- Prepare the forwarder's configuration.
  (howToConnect, freq) <- getArgs >>= \case
    [path, freq]       -> return (LocalPipe path, freq)
    [host, port, freq] -> return (RemoteSocket (pack host) (read port :: Port), freq)
    _                  -> die "Usage: demo-forwarder (pathToLocalPipe | host port) freqInMilliSecs"
  let freqAsNum = read freq :: Word64
      config =
        ForwarderConfiguration
          { forwarderTracer    = contramap show stdoutTracer
          , acceptorEndpoint   = howToConnect
          , reConnectFrequency = Every freqAsNum MilliSeconds
          }

  -- Create an empty EKG store and register predefined GC metrics in it.
  store <- EKG.newStore
  EKG.registerGcMetrics store

  -- Run the forwarder. It will establish the connection with the acceptor,
  -- then the acceptor will periodically ask for the metrics, the forwarder
  -- will take them from the 'store' and send them back.
  runEKGForwarder config store
