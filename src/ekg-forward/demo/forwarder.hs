import           System.Metrics.Forwarder (runEKGForwarder)
import           System.Metrics.Configuration (ForwarderConfiguration (..), HowToConnect (..))

main :: IO ()
main = do
  runEKGForwarder config
 where
  config =
    ForwarderConfiguration {
      connectToAcceptor = LocalPipe "./demo-ekg-forward.sock"
    }
