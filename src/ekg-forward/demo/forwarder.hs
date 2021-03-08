import           System.Metrics.Forwarder (runEKGForwarder)

main :: IO ()
main = do
  runEKGForwarder "./demo-ekg-forward.sock"
