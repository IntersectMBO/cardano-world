{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Mux.Bearer.Queues
  ( queuesAsMuxBearer
  , runMuxWithQueues
  ) where

import qualified Data.ByteString.Lazy as BL
import           Data.Word (Word16)

import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadSTM.Strict
import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadTimer
import           Control.Monad.Class.MonadThrow
import           Control.Tracer

import qualified Network.Mux as Mx
import           Network.Mux.Types (MuxBearer)
import qualified Network.Mux.Types as Mx
import qualified Network.Mux.Codec as Mx
import           Network.Mux.Time as Mx


queuesAsMuxBearer
  :: forall m.
     ( MonadSTM   m
     , MonadTime  m
     , MonadThrow m
     , Eq  (Async m ())
     )
  => Tracer m Mx.MuxTrace
  -> TBQueue m BL.ByteString
  -> TBQueue m BL.ByteString
  -> Word16
  -> MuxBearer m
queuesAsMuxBearer tracer writeQueue readQueue sduSize = do
      Mx.MuxBearer {
        Mx.read    = readMux,
        Mx.write   = writeMux,
        Mx.sduSize = sduSize
      }
    where
      readMux :: m (Mx.MuxSDU, Time)
      readMux = do
          traceWith tracer $ Mx.MuxTraceRecvHeaderStart
          buf <- atomically $ readTBQueue readQueue
          let (hbuf, payload) = BL.splitAt 8 buf
          case Mx.decodeMuxSDU hbuf of
              Left  e      -> throwM e
              Right header -> do
                  traceWith tracer $ Mx.MuxTraceRecvHeaderEnd header
                  traceWith tracer $ Mx.MuxTraceRecvPayloadStart $ fromIntegral $ BL.length payload
                  ts <- getMonotonicTime
                  traceWith tracer $ Mx.MuxTraceRecvDeltaQObservation header ts
                  traceWith tracer $ Mx.MuxTraceRecvPayloadEnd payload
                  return (header {Mx.msBlob = payload}, ts)

      writeMux :: Mx.MuxSDU -> m Time
      writeMux sdu = do
          ts <- getMonotonicTime
          let ts32 = Mx.timestampMicrosecondsLow32Bits ts
              sdu' = sdu { Mx.msTimestamp = Mx.RemoteClockModel ts32 }
              buf  = Mx.encodeMuxSDU sdu'
          traceWith tracer $ Mx.MuxTraceSendStart sdu'
          atomically $ writeTBQueue writeQueue buf
          traceWith tracer $ Mx.MuxTraceSendEnd
          return ts

runMuxWithQueues
  :: ( MonadAsync m
     , MonadCatch m
     , MonadMask m
     , MonadSTM m
     , MonadThrow m
     , MonadThrow (STM m)
     , MonadTime m
     , MonadTimer m
     , Eq  (Async m ())
     )
  => Tracer m Mx.MuxTrace
  -> Mx.MuxApplication appType m a b
  -> TBQueue m BL.ByteString
  -> TBQueue m BL.ByteString
  -> Word16
  -> m (Maybe SomeException)
runMuxWithQueues tracer app wq rq mtu = do
    let bearer = queuesAsMuxBearer tracer wq rq mtu
    res_e <- try $ Mx.muxStart tracer app bearer
    case res_e of
         Left  e -> return (Just e)
         Right _ -> return Nothing
