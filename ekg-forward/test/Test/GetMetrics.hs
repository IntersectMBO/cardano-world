module Test.GetMetrics
  ( getMetricsViaPipe
  , getMetricsViaSocket
  ) where

import Test.Hspec

import           Control.Concurrent (forkIO, killThread, threadDelay)
import qualified Data.HashMap.Strict as HM
import           Data.IORef (newIORef)
import qualified Data.List.NonEmpty as NE
import qualified System.Metrics as EKG
import qualified System.Metrics.Gauge as G
import qualified System.Metrics.Label as L
import qualified System.Metrics.Counter as C

import           System.Metrics.Acceptor (runEKGAcceptor)
import           System.Metrics.Forwarder (runEKGForwarder)
import           System.Metrics.Configuration (HowToConnect (..))
import           System.Metrics.Request (Request (..))

import           Test.MkConfig (mkAcceptorConfig, mkForwarderConfig)

getMetricsViaPipe, getMetricsViaSocket :: IO ()
getMetricsViaPipe   = getMetrics $ LocalPipe "/tmp/ekg-forward-test-2.sock"
getMetricsViaSocket = getMetrics $ RemoteSocket "127.0.0.1" 3020

getMetrics :: HowToConnect -> IO ()
getMetrics endpoint = do
  forwarderStore <- EKG.newStore
  acceptorStore  <- EKG.newStore
  weAreDone <- newIORef False

  let acceptorConfig = mkAcceptorConfig endpoint weAreDone $
        GetMetrics $ NE.fromList ["test2.gauge.1", "test2.label.2"]
      forwarderConfig = mkForwarderConfig endpoint

  acceptorThr <- forkIO $ runEKGAcceptor acceptorConfig acceptorStore

  EKG.createGauge   "test2.gauge.1" forwarderStore >>= flip G.set 123
  EKG.createGauge   "test2.gauge.2" forwarderStore >>= flip G.set 456
  EKG.createLabel   "test2.label.1" forwarderStore >>= flip L.set "TestLabel_1"
  EKG.createLabel   "test2.label.2" forwarderStore >>= flip L.set "TestLabel_2"
  EKG.createCounter "test2.cntr.1"  forwarderStore >>= flip C.add 10

  forwarderThr <- forkIO $ runEKGForwarder forwarderConfig forwarderStore

  threadDelay 2000000

  killThread forwarderThr
  killThread acceptorThr

  acceptorMetrics  <- HM.toList <$> EKG.sampleAll acceptorStore

  acceptorMetrics `shouldBe` [ ("test2.gauge.1", EKG.Gauge 123)
                             , ("test2.label.2", EKG.Label "TestLabel_2")
                             ]
