{-# LANGUAGE OverloadedStrings #-}

import           System.Metrics.Acceptor (runEKGAcceptor)
import           System.Metrics.Configuration (AcceptorConfiguration (..), HowToConnect (..),
                                               RequestFrequency (..), TimePeriod (..))

main :: IO ()
main = do
  runEKGAcceptor config
 where
  config =
    AcceptorConfiguration
      { listenToForwarder = RemoteSocket "127.0.0.1" 3010 -- LocalPipe "./demo-ekg-forward.sock"
      , requestFrequency  = AskMetricsEvery 1000 MilliSeconds
      }
