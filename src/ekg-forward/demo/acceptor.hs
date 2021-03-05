import           Data.Functor (void)

import           System.Metrics.Acceptor

main :: IO ()
main = do
  void $ ekgAcceptor "./demo-ekg-forward.sock"
