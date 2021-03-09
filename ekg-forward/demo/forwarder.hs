{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import           System.Environment (getArgs)
import           System.Exit (die)

import qualified System.Metrics as EKG

import           System.Metrics.Forwarder (runEKGForwarder)
import           System.Metrics.Configuration (ForwarderConfiguration (..),
                                               HowToConnect (..), Port)

main :: IO ()
main = do
  -- Prepare the configuration.
  connectTo <- getArgs >>= \case
    [path]       -> return $ LocalPipe path
    [host, port] -> return $ RemoteSocket (T.pack host) (read port :: Port)
    _            -> die "Usage: demo-forwarder (pathToLocalPipe | host port)"
  let config =
        ForwarderConfiguration
          { connectToAcceptor = connectTo
          }

  -- Create an empty EKG store and register predefined GC metrics in it.
  store <- EKG.newStore
  EKG.registerGcMetrics store

  -- Run the forwarder.
  runEKGForwarder config store
