{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.Driver.ByteLimit
  ( runPeerWithByteLimit
  , DecoderFailureOrTooMuchInput (..)
  ) where

import           Control.Exception
import           Data.Int (Int64)

import           Control.Monad.Class.MonadThrow (MonadThrow (..))
import           Control.Tracer (Tracer, traceWith)


import           Network.TypedProtocol.Core
import           Network.TypedProtocol.Driver

import           Ouroboros.Network.Channel
import           Ouroboros.Network.Codec
import           Ouroboros.Network.Driver.Simple (TraceSendRecv(..))


data DecoderFailureOrTooMuchInput failure
  = DecoderFailure !failure
  | TooMuchInput
  deriving (Show)

instance Exception failure => Exception (DecoderFailureOrTooMuchInput failure)

runDecoderWithByteLimit
    :: forall m bytes failure a. Monad m
    => Int64
    -- ^ message size limit
    -> (bytes -> Int64)
    -- ^ byte size
    -> Channel m bytes
    -> Maybe bytes
    -> DecodeStep bytes failure m a
    -> m (Either (DecoderFailureOrTooMuchInput failure) (a, Maybe bytes))
runDecoderWithByteLimit limit byteLength Channel{recv} mbytes = go 0 mbytes
    where
      go :: Int64
         -- ^ length of consumed input
         -> Maybe bytes
         -> DecodeStep bytes failure m a
         -> m (Either (DecoderFailureOrTooMuchInput failure) (a, Maybe bytes))
      -- we decoded the data, but we might be over the limit
      go !l _  (DecodeDone x trailing) | l - maybe 0 byteLength trailing > limit = return (Left TooMuchInput)
                                       | otherwise                       = return (Right (x, trailing))
      -- we run over the limit, return @TooMuchInput@ error
      go !l _  _                       | l > limit = return (Left TooMuchInput)
      go !_ _  (DecodeFail failure)    = return (Left (DecoderFailure failure))

      go !l Nothing         (DecodePartial k) =
        recv >>= (\mbs -> k mbs >>= go (l + maybe 0 byteLength mbs) Nothing)

      go !l (Just trailing) (DecodePartial k) =
        k (Just trailing) >>= go (l + byteLength trailing) Nothing


-- |
-- Like @'runPeer'@, but require that each inbound message is smaller than
-- @limit@ bytes; if the limit is exceeded throw @'TooMuchInput'@ exception.
--
runPeerWithByteLimit
  :: forall ps (st :: ps) pr bytes failure m a .
     (MonadThrow m, Exception failure)
  => Int64
  -- ^ message size limit
  -> (bytes -> Int64)
  -- ^ byte size
  -> Tracer m (TraceSendRecv ps)
  -> Codec ps failure m bytes
  -> Channel m bytes
  -> Peer ps pr st m a
  -> m a

runPeerWithByteLimit limit byteLength tracer
                     Codec{encode, decode} channel@Channel{send} =
    go Nothing
  where
    go :: forall st'.
          Maybe bytes
       -> Peer ps pr st' m a
       -> m a
    go trailing (Effect k) = k >>= go trailing
    go _        (Done _ x) = return x

    go trailing (Yield stok msg k) = do
      send (encode stok msg)
      traceWith tracer (TraceSendMsg (AnyMessage msg))
      go trailing k

    go trailing (Await stok k) = do
      decoder <- decode stok
      res <- runDecoderWithByteLimit limit byteLength channel trailing decoder
      case res of
        Right (SomeMessage msg, trailing') -> do
          traceWith tracer (TraceRecvMsg (AnyMessage msg))
          go trailing' (k msg)
        Left failure ->
          throwM failure
