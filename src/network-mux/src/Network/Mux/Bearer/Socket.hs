{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Mux.Bearer.Socket
  ( socketAsMuxBearer
  , hexDump
  ) where

import           Control.Monad (when)
import           Control.Tracer
import qualified Data.ByteString.Lazy as BL
import           Data.Int
import           Text.Printf

import           GHC.Stack

import           Control.Monad.Class.MonadSay
import           Control.Monad.Class.MonadSTM.Strict
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadTimer

import qualified Network.Socket as Socket hiding (recv)
import qualified Network.Socket.ByteString.Lazy as Socket (recv, sendAll)

import qualified Network.Mux as Mx
import           Network.Mux.Types (MuxBearer)
import qualified Network.Mux.Types as Mx
import qualified Network.Mux.Codec as Mx
import qualified Network.Mux.Time as Mx

hexDump :: BL.ByteString -> String -> IO ()
hexDump buf out | BL.empty == buf = say out
hexDump buf out = hexDump (BL.tail buf) (out ++ printf "0x%02x " (BL.head buf))


-- |
-- Create @'MuxBearer'@ from a socket.
--
-- Note: 'IOException's thrown by 'sendAll' and 'recv' are wrapped in
-- 'MuxError'.
--
socketAsMuxBearer
  :: forall ptcl.
     Mx.ProtocolEnum ptcl
  => Tracer IO (Mx.MuxTrace ptcl)
  -> Socket.Socket
  -> IO (MuxBearer ptcl IO)
socketAsMuxBearer tracer sd = do
      mxState <- atomically $ newTVar Mx.Larval
      return $ Mx.MuxBearer {
          Mx.read    = readSocket,
          Mx.write   = writeSocket,
          Mx.sduSize = 12288,
          Mx.state   = mxState
        }
    where
      readSocket :: (HasCallStack) => IO (Mx.MuxSDU ptcl, Time)
      readSocket = do
          traceWith tracer $ Mx.MuxTraceRecvHeaderStart
          hbuf <- recvLen' True 8 []
          --say "read"
          --hexDump hbuf ""
          case Mx.decodeMuxSDU hbuf of
              Left  e      -> throwM e
              Right header -> do
                  -- say $ printf "decoded mux header, goint to read %d bytes" (Mx.msLength header)
                  traceWith tracer $ Mx.MuxTraceRecvHeaderEnd header
                  traceWith tracer $ Mx.MuxTraceRecvPayloadStart (fromIntegral $ Mx.msLength header)
                  blob <- recvLen' False (fromIntegral $ Mx.msLength header) []
                  ts <- getMonotonicTime
                  traceWith tracer (Mx.MuxTraceRecvDeltaQObservation header ts)
                  traceWith tracer $ Mx.MuxTraceRecvPayloadEnd blob
                  --hexDump blob ""
                  return (header {Mx.msBlob = blob}, ts)

      recvLen' :: Bool -> Int64 -> [BL.ByteString] -> IO BL.ByteString
      recvLen' _ 0 bufs = return (BL.concat $ reverse bufs)
      recvLen' waitingOnNxtHeader l bufs = do
          traceWith tracer $ Mx.MuxTraceRecvStart $ fromIntegral l
          buf <- Socket.recv sd l
                    `catch` Mx.handleIOException "recv errored"
          if BL.null buf
              then do
                  when (waitingOnNxtHeader) $
                      {- This may not be an error, but could be an orderly shutdown.
                       - We wait 1 seconds to give the mux protocols time to perform
                       - a clean up and exit.
                       -}
                      threadDelay 1
                  throwM $ Mx.MuxError Mx.MuxBearerClosed (show sd ++
                      " closed when reading data, waiting on next header " ++
                      show waitingOnNxtHeader) callStack
              else do
                  traceWith tracer $ Mx.MuxTraceRecvEnd buf
                  recvLen' False (l - fromIntegral (BL.length buf)) (buf : bufs)

      writeSocket :: Mx.MuxSDU ptcl -> IO Time
      writeSocket sdu = do
          --say "write"
          ts <- getMonotonicTime
          let ts32 = Mx.timestampMicrosecondsLow32Bits ts
              sdu' = sdu { Mx.msTimestamp = Mx.RemoteClockModel ts32 }
              buf  = Mx.encodeMuxSDU sdu'
          --hexDump buf ""
          traceWith tracer $ Mx.MuxTraceSendStart sdu'
          Socket.sendAll sd buf
            `catch` Mx.handleIOException "sendAll errored"
          traceWith tracer $ Mx.MuxTraceSendEnd
          return ts

