{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
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
  -- Prepare the configuration.
  (connectTo, freq) <- getArgs >>= \case
    [path, freq]       -> return (LocalPipe path, freq)
    [host, port, freq] -> return (RemoteSocket (T.pack host) (read port :: Port), freq)
    _                  -> die "Usage: demo-forwarder (pathToLocalPipe | host port) freqInMilliSecs"
  let freqAsNum = read freq :: Word64
      config =
        ForwarderConfiguration
          { connectToAcceptor  = connectTo
          , reConnectFrequency = Every freqAsNum MilliSeconds
          }

  -- Create an empty EKG store and register predefined GC metrics in it.
  store <- EKG.newStore
  EKG.registerGcMetrics store

  -- Run the forwarder. All the metrics (or the metrics with specified names)
  -- from the store will be forwarded to the acceptor.
  runEKGForwarder config store
