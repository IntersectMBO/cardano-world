{-# LANGUAGE GADTs               #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.KeepAlive
  ( KeepAliveInterval (..)
  , keepAliveClient
  , keepAliveServer

  , TraceKeepAliveClient
  ) where

import           Control.Exception (assert)
import qualified Control.Monad.Class.MonadSTM as Lazy
import           Control.Monad.Class.MonadSTM.Strict
import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadTimer
import           Control.Tracer (Tracer, traceWith)
import           Data.Maybe (fromJust)
import qualified Data.Map.Strict as M

import           Ouroboros.Network.Mux (RunOrStop (..), ScheduledStop)
import           Ouroboros.Network.DeltaQ
import           Ouroboros.Network.Protocol.KeepAlive.Client
import           Ouroboros.Network.Protocol.KeepAlive.Server


newtype KeepAliveInterval = KeepAliveInterval { keepAliveInterval :: DiffTime }

data TraceKeepAliveClient peer =
    AddSample peer DiffTime PeerGSV

instance Show peer => Show (TraceKeepAliveClient peer) where
    show (AddSample peer rtt gsv) = "AddSample " ++ show peer ++ " sample: " ++ show rtt
        ++ " gsv: " ++ show gsv

keepAliveClient
    :: forall m peer.
       ( MonadSTM   m
       , MonadMonotonicTime m
       , MonadTimer m
       , Ord peer
       )
    => Tracer m (TraceKeepAliveClient peer)
    -> ScheduledStop m
    -> peer
    -> (StrictTVar m (M.Map peer PeerGSV))
    -> KeepAliveInterval
    -> KeepAliveClient m ()
keepAliveClient tracer shouldStopSTM peer dqCtx KeepAliveInterval { keepAliveInterval } =
    SendMsgKeepAlive (go Nothing)
  where
    payloadSize = 2

    decisionSTM :: Lazy.TVar m Bool
                -> STM  m RunOrStop
    decisionSTM delayVar = do
       shouldStop <- shouldStopSTM
       case shouldStop of
            Stop -> return Stop
            Run  -> do
              done <- Lazy.readTVar delayVar
              if done
                 then return Run
                 else retry

    go :: Maybe Time -> m (KeepAliveClient m ())
    go startTime_m = do
      endTime <- getMonotonicTime
      case startTime_m of
           Just startTime -> do
               let rtt = diffTime endTime startTime
                   sample = fromSample startTime endTime payloadSize
               gsv' <- atomically $ do
                   m <- readTVar dqCtx
                   assert (peer `M.member` m) $ do
                     let (gsv', m') = M.updateLookupWithKey (\_ a -> Just $ sample <> a) peer m
                     writeTVar dqCtx m'
                     return $ fromJust gsv'
               traceWith tracer $ AddSample peer rtt gsv'

           Nothing        -> return ()

      let keepAliveInterval' = case startTime_m of
                                    Just _  -> keepAliveInterval
                                    Nothing -> 0 -- The first time we send a packet directly.

      delayVar <- registerDelay keepAliveInterval'
      decision <- atomically (decisionSTM delayVar)
      now <- getMonotonicTime
      case decision of
        Run  -> pure (SendMsgKeepAlive $ go $ Just now)
        Stop -> pure (SendMsgDone (pure ()))


keepAliveServer
  :: forall m.  Applicative m
  => KeepAliveServer m ()
keepAliveServer = KeepAliveServer {
    recvMsgKeepAlive = pure keepAliveServer,
    recvMsgDone      = pure ()
  }
