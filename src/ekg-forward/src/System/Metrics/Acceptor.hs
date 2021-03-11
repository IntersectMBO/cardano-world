{- |
Copyright: (c) 2021 Input Output (Hong Kong) Ltd.
Maintainer: Denis Shevchenko <denis.shevchenko@iohk.io>

See README for more info
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This top-level module will be used by the acceptor app
-- (the app that asks EKG metrics from the forwarder app).
--
module System.Metrics.Acceptor (
  runEKGAcceptor
  ) where

import           Control.Exception (SomeException, try)
import           Control.Monad (void)
import           Data.IORef (newIORef)
import qualified System.Metrics as EKG

import           System.Metrics.Acceptor.Network (listenToForwarder)
import           System.Metrics.Acceptor.Store (emptyMetricsLocalStore)
import           System.Metrics.Configuration (AcceptorConfiguration (..))

-- | Please note that acceptor is a server from the __networking__ point of view:
-- the forwarder establishes network connection with the acceptor.
--
runEKGAcceptor
  :: AcceptorConfiguration
  -> EKG.Store
  -> IO ()
runEKGAcceptor config ekgStore = do
  metricsStore <- newIORef emptyMetricsLocalStore
  try (void $ listenToForwarder config ekgStore metricsStore) >>= \case
    Left (_e :: SomeException) ->
      runEKGAcceptor config ekgStore
    Right _ -> return ()
