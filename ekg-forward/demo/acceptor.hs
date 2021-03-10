{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import           Data.Word (Word64)
import           System.Environment (getArgs)
import           System.Exit (die)

import qualified System.Metrics as EKG

import           System.Metrics.Acceptor (runEKGAcceptor)
import           System.Metrics.Configuration (AcceptorConfiguration (..), HowToConnect (..),
                                               RequestFrequency (..), TimePeriod (..), Port,
                                               WhatToRequest (..))

main :: IO ()
main = do
  (listenIt, freq) <- getArgs >>= \case
    [path, freq]       -> return (LocalPipe path, freq)
    [host, port, freq] -> return (RemoteSocket (T.pack host) (read port :: Port), freq)
    _                  -> die "Usage: demo-acceptor (pathToLocalPipe | host port) freqInMilliSecs"
  let freqAsNum = read freq :: Word64
      config =
        AcceptorConfiguration
          { listenToForwarder = listenIt
          , requestFrequency  = AskMetricsEvery freqAsNum MilliSeconds
          , whatToRequest     = AllMetrics
          }
      actionOnResponse = print
  store <- EKG.newStore
  runEKGAcceptor config actionOnResponse store
