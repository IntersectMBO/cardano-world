{- |
Copyright: (c) 2021 Input Output (Hong Kong) Ltd.
Maintainer: Denis Shevchenko <denis.shevchenko@iohk.io>

See README for more info
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module System.Metrics.Internal.Protocol.Codec where

import           Control.Monad.Class.MonadST
import qualified Data.ByteString.Lazy as LBS
import           Data.Word (Word8)
import           Data.Char (ord)
import           GHC.Base (unsafeChr)
import           Text.Printf (printf)
import           Text.Read (readMaybe)

import           Codec.CBOR.Decoding (decodeListLen, decodeWord)
import qualified Codec.CBOR.Decoding as CBOR
import           Codec.CBOR.Encoding (encodeListLen, encodeWord)
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Read as CBOR

import           Network.TypedProtocol.Core
import           Ouroboros.Network.Codec

import           System.Metrics.Internal.Protocol.Type

codecEKGForward
  :: forall req resp m.
     (MonadST m)
  => (req -> CBOR.Encoding)
  -> (forall s . CBOR.Decoder s req)
  -> (resp -> CBOR.Encoding)
  -> (forall s . CBOR.Decoder s resp)
  -> Codec (EKGForward req resp)
           CBOR.DeserialiseFailure m LBS.ByteString
codecEKGForward encodeReq  decodeReq
                encodeResp decodeResp =
  mkCodecCborLazyBS encode decode
 where
  -- Encode messages.
  encode :: forall (pr  :: PeerRole)
                   (st  :: EKGForward req resp)
                   (st' :: EKGForward req resp).
            PeerHasAgency pr st
         -> Message (EKGForward req resp) st st'
         -> CBOR.Encoding

  encode (ClientAgency TokIdle) (MsgReq req) =
    encodeListLen 2
      <> encodeWord 0
      <> encodeReq req

  encode (ClientAgency TokIdle) MsgDone =
    encodeListLen 1
      <> encodeWord 1

  encode (ServerAgency TokBusy) (MsgResp resp) =
    encodeListLen 2
      <> encodeWord 1
      <> encodeResp resp

  -- Decode messages
  decode :: forall (pr :: PeerRole)
                   (st :: EKGForward req resp) s.
            PeerHasAgency pr st
         -> CBOR.Decoder s (SomeMessage st)
  decode stok = do
    len <- decodeListLen
    key <- decodeWord
    case (key, len, stok) of
      (0, 2, ClientAgency TokIdle) -> do
        req <- decodeReq
        return (SomeMessage (MsgReq req))

      (1, 1, ClientAgency TokIdle) ->
        return (SomeMessage MsgDone)

      (1, 2, ServerAgency TokBusy) -> do
        resp <- decodeResp
        return (SomeMessage (MsgResp resp))

      -- Failures per protocol state
      (_, _, ClientAgency TokIdle) ->
        fail (printf "codecEKGForward (%s) unexpected key (%d, %d)" (show stok) key len)
      (_, _, ServerAgency TokBusy) ->
        fail (printf "codecEKGForward (%s) unexpected key (%d, %d)" (show stok) key len)
