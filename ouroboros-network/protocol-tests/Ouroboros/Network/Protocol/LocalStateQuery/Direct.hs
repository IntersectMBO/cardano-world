{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ouroboros.Network.Protocol.LocalStateQuery.Direct (
    direct
  ) where

import           Ouroboros.Network.Protocol.LocalStateQuery.Client
import           Ouroboros.Network.Protocol.LocalStateQuery.Server

direct
  :: forall block query result m a b.
     Monad m
  => LocalStateQueryClient block query result m a
  -> LocalStateQueryServer block query result m b
  -> m (a, b)
direct (LocalStateQueryClient client) (LocalStateQueryServer mserver) =
    mserver >>= directIdle client
  where
    directIdle
      :: ClientStIdle block query result m a
      -> ServerStIdle block query result m b
      -> m (a, b)
    directIdle (SendMsgAcquire pt client') ServerStIdle{recvMsgAcquire} = do
      server' <- recvMsgAcquire pt
      directAcquiring client' server'
    directIdle (SendMsgDone a) ServerStIdle{recvMsgDone} = do
      b <- recvMsgDone
      return (a, b)

    directAcquiring
      :: ClientStAcquiring block query result m a
      -> ServerStAcquiring block query result m b
      -> m (a, b)
    directAcquiring ClientStAcquiring{recvMsgAcquired} (SendMsgAcquired server') =
      let client' = recvMsgAcquired
      in directAcquired client' server'
    directAcquiring ClientStAcquiring{recvMsgFailure} (SendMsgFailure failure server') = do
      client' <- recvMsgFailure failure
      directIdle client' server'

    directAcquired
      :: ClientStAcquired block query result m a
      -> ServerStAcquired block query result m b
      -> m (a, b)
    directAcquired (SendMsgQuery query client') ServerStAcquired{recvMsgQuery} = do
      server' <- recvMsgQuery query
      directQuerying client' server'
    directAcquired (SendMsgReAcquire pt client') ServerStAcquired{recvMsgReAcquire} = do
      server' <- recvMsgReAcquire pt
      directAcquiring client' server'
    directAcquired (SendMsgRelease client') ServerStAcquired{recvMsgRelease} = do
      server' <- recvMsgRelease
      directIdle client' server'

    directQuerying
      :: ClientStQuerying block query result m a
      -> ServerStQuerying block query result m b
      -> m (a, b)
    directQuerying ClientStQuerying{recvMsgResult} (SendMsgResult result server') = do
      client' <- recvMsgResult result
      directAcquired client' server'
