module Test.GetAllMetrics
  ( getAllMetricsViaPipe
  , getAllMetricsViaSocket
  ) where

import Test.Hspec

import           Control.Concurrent (forkIO, killThread, threadDelay)
import qualified System.Metrics as EKG
import qualified System.Metrics.Gauge as G
import qualified System.Metrics.Label as L
import qualified System.Metrics.Counter as C

import           System.Metrics.Acceptor (runEKGAcceptor)
import           System.Metrics.Forwarder (runEKGForwarder)
import           System.Metrics.Configuration (HowToConnect (..))
import           System.Metrics.Request (Request (..))

import           Test.MkConfig (mkAcceptorConfig, mkForwarderConfig)

getAllMetricsViaPipe, getAllMetricsViaSocket :: IO ()
getAllMetricsViaPipe   = getAllMetrics $ LocalPipe "/tmp/ekg-forward-test-1.sock"
getAllMetricsViaSocket = getAllMetrics $ RemoteSocket "127.0.0.1" 3010

getAllMetrics :: HowToConnect -> IO ()
getAllMetrics endpoint = do
  forwarderStore <- EKG.newStore
  acceptorStore  <- EKG.newStore
  let acceptorConfig = mkAcceptorConfig endpoint GetAllMetrics
      forwarderConfig = mkForwarderConfig endpoint

  acceptorThr <- forkIO $ runEKGAcceptor acceptorConfig acceptorStore

  EKG.createGauge   "test1.gauge.1" forwarderStore >>= flip G.set 123
  EKG.createGauge   "test1.gauge.2" forwarderStore >>= flip G.set 456
  EKG.createLabel   "test1.label.1" forwarderStore >>= flip L.set "TestLabel_1"
  EKG.createLabel   "test1.label.2" forwarderStore >>= flip L.set "TestLabel_2"
  EKG.createCounter "test1.cntr.1"  forwarderStore >>= flip C.add 10

  forwarderThr <- forkIO $ runEKGForwarder forwarderConfig forwarderStore

  threadDelay 2000000

  killThread forwarderThr
  killThread acceptorThr

  forwarderMetrics <- EKG.sampleAll forwarderStore
  acceptorMetrics  <- EKG.sampleAll acceptorStore

  acceptorMetrics `shouldBe` forwarderMetrics
