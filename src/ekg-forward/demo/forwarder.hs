{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import           System.Environment (getArgs)
import           System.Exit (die)

import           System.Metrics.Forwarder (runEKGForwarder)
import           System.Metrics.Configuration (ForwarderConfiguration (..),
                                               HowToConnect (..), Port)

main :: IO ()
main = do
  connectTo <- getArgs >>= \case
    [path]       -> return $ LocalPipe path
    [host, port] -> return $ RemoteSocket (T.pack host) (read port :: Port)
    _            -> die "Usage: demo-forwarder (pathToLocalPipe | host port)"
  let config =
        ForwarderConfiguration
          { connectToAcceptor = connectTo
          }
  runEKGForwarder config
