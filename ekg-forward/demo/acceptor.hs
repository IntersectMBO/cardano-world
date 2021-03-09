import           Data.Functor (void)

import           System.Metrics.Acceptor (runEKGAcceptor)
import           System.Metrics.Configuration (AcceptorConfiguration (..), HowToConnect (..))

main :: IO ()
main = do
  void $ runEKGAcceptor config
 where
  config =
    AcceptorConfiguration {
      listenToForwarder = LocalPipe "./demo-ekg-forward.sock"
    }
