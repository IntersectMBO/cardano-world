{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import           System.Environment (getArgs)
import           System.Exit (die)

import qualified System.Metrics as EKG

import           System.Metrics.Acceptor (runEKGAcceptor)
import           System.Metrics.Configuration (AcceptorConfiguration (..), HowToConnect (..),
                                               RequestFrequency (..), TimePeriod (..), Port,
                                               WhatToRequest (..))

main :: IO ()
main = do
  listenIt <- getArgs >>= \case
    [path]       -> return $ LocalPipe path
    [host, port] -> return $ RemoteSocket (T.pack host) (read port :: Port)
    _            -> die "Usage: demo-acceptor (pathToLocalPipe | host port)"
  let config =
        AcceptorConfiguration
          { listenToForwarder = listenIt
          , requestFrequency  = AskMetricsEvery 1000 MilliSeconds
          , whatToRequest     = AllMetrics
          }
      actionOnResponse = print
  store <- EKG.newStore
  runEKGAcceptor config actionOnResponse store
