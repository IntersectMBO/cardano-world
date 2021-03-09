{-# LANGUAGE OverloadedStrings #-}

import           System.Metrics.Forwarder (runEKGForwarder)
import           System.Metrics.Configuration (ForwarderConfiguration (..), HowToConnect (..))

main :: IO ()
main = do
  runEKGForwarder config
 where
  config =
    ForwarderConfiguration
      { connectToAcceptor = RemoteSocket "127.0.0.1" 3010 -- LocalPipe "./demo-ekg-forward.sock"
      }
