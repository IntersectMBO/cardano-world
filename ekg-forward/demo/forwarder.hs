import           System.Metrics.Forwarder

main :: IO ()
main = do
  ekgForwarder "./demo-ekg-forward.sock"
