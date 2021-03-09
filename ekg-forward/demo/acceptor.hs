import           System.Metrics.Acceptor (runEKGAcceptor)
import           System.Metrics.Configuration (AcceptorConfiguration (..), HowToConnect (..),
                                               RequestFrequency (..), TimePeriod (..))

main :: IO ()
main = do
  runEKGAcceptor config
 where
  config =
    AcceptorConfiguration
      { listenToForwarder = LocalPipe "./demo-ekg-forward.sock"
      , requestFrequency  = AskMetricsEvery 1000 MilliSeconds
      }
