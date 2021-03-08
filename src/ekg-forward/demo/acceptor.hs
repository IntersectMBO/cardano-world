import           Data.Functor (void)

import           System.Metrics.Acceptor (runEKGAcceptor)

main :: IO ()
main = do
  void $ runEKGAcceptor "./demo-ekg-forward.sock"
