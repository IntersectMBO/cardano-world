{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- This top-level module will be used by the forwarder app
-- (the app that collects EKG metrics and sends them to the acceptor).
--
module System.Metrics.Forwarder
  ( runEKGForwarder
  ) where

import           Control.Exception (SomeException, try)
import           Control.Concurrent (threadDelay)
import           Control.Monad (forever)
import qualified System.Metrics as EKG

import           System.Metrics.Configuration (ForwarderConfiguration (..), Frequency (..),
                                               TimePeriod (..))
import           System.Metrics.Forwarder.Network (connectToAcceptor)

runEKGForwarder
  :: ForwarderConfiguration
  -> EKG.Store
  -> IO ()
runEKGForwarder config@ForwarderConfiguration{..} ekgStore =
  forever $
    try (connectToAcceptor config ekgStore) >>= \case
      Left (_e :: SomeException) -> do
        threadDelay $ mkDelay reConnectFrequency
        runEKGForwarder config ekgStore
      Right _ -> return ()
 where
  mkDelay (Every delay Seconds)      = fromIntegral delay * 1000000
  mkDelay (Every delay MilliSeconds) = fromIntegral delay * 1000
