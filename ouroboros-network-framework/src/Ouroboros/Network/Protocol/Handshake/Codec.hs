{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Ouroboros.Network.Protocol.Handshake.Codec
  ( codecHandshake

  , byteLimitsHandshake
  , timeLimitsHandshake
  ) where

import           Control.Monad.Class.MonadST
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTime
import           Control.Monad (unless)
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import           Data.Map (Map)
import qualified Data.Map as Map

import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Read     as CBOR
import qualified Codec.CBOR.Term     as CBOR
import           Codec.Serialise (Serialise)
import qualified Codec.Serialise     as CBOR

import           Ouroboros.Network.Codec hiding (encode, decode)
import           Ouroboros.Network.Driver.Limits
import           Ouroboros.Network.Protocol.Handshake.Type
import           Ouroboros.Network.Protocol.Handshake.Version
import           Ouroboros.Network.Protocol.Limits

-- |
-- We assume that a TCP segment size of 1440 bytes with initial window of size
-- 4.  This sets upper limit of 5760 bytes on each message of handshake
-- protocol.
--
maxTransmissionUnit :: Word
maxTransmissionUnit = 4 * 1440

-- | Byte limits
byteLimitsHandshake :: ProtocolSizeLimits (Handshake vNumber CBOR.Term) ByteString
byteLimitsHandshake = ProtocolSizeLimits stateToLimit (fromIntegral . BL.length)
  where
    stateToLimit :: forall (pr :: PeerRole) (st  :: Handshake vNumber CBOR.Term).
                    PeerHasAgency pr st -> Word
    stateToLimit (ClientAgency TokPropose) = maxTransmissionUnit
    stateToLimit (ServerAgency TokConfirm) = maxTransmissionUnit

-- Time limits
-- We don't use a per-state timeout, instead there is an overall timeout
-- for the complete handshake protocol exchange.
timeLimitsHandshake :: ProtocolTimeLimits (Handshake vNumber CBOR.Term)
timeLimitsHandshake = ProtocolTimeLimits stateToLimit
  where
    stateToLimit :: forall (pr :: PeerRole) (st  :: Handshake vNumber CBOR.Term).
                    PeerHasAgency pr st -> Maybe DiffTime
    stateToLimit _ = Nothing


-- |
-- @'Handshake'@ codec.  The @'MsgProposeVersions'@ encodes proposed map in
-- ascending order and it expects to receive them in this order.  This allows
-- to construct the map in linear time.  There is also another limiting factor
-- to the number of versions on can present: the whole message must fit into
-- a single TCP segment.
--
codecHandshake
  :: forall vNumber m failure.
     ( Monad m
     , MonadThrow m
     , MonadST m
     , Ord vNumber
     , Enum vNumber
     , Serialise vNumber
     , Show vNumber
     , Show failure
     )
  => CodecCBORTerm (failure, Maybe Int) vNumber
  -> Codec (Handshake vNumber CBOR.Term) CBOR.DeserialiseFailure m ByteString
codecHandshake versionNumberCodec = mkCodecCborLazyBS encode decode
    where
      encode :: forall (pr :: PeerRole) st st'.
                PeerHasAgency pr st
             -> Message (Handshake vNumber CBOR.Term) st st'
             -> CBOR.Encoding

      encode (ClientAgency TokPropose) (MsgProposeVersions vs) =
        let vs' = Map.toAscList vs
        in
           CBOR.encodeListLen 2
        <> CBOR.encodeWord 0
        <> CBOR.encodeMapLen (fromIntegral $ length vs')
        <> mconcat [    CBOR.encodeTerm (encodeTerm versionNumberCodec vNumber)
                     <> CBOR.encodeTerm vParams
                   | (vNumber, vParams) <- vs'
                   ]

      encode (ServerAgency TokConfirm) (MsgAcceptVersion vNumber vParams) =
           CBOR.encodeListLen 3
        <> CBOR.encodeWord 1
        <> CBOR.encodeTerm (encodeTerm versionNumberCodec vNumber)
        <> CBOR.encodeTerm vParams

      encode (ServerAgency TokConfirm) (MsgRefuse vReason) =
           CBOR.encodeListLen 2
        <> CBOR.encodeWord 2
        <> CBOR.encode vReason

      -- decode a map checking the assumption that
      --  * keys are different
      --  * keys are encoded in ascending order
      -- fail when one of these assumptions is not met
      decodeMap :: Int
                -> Maybe vNumber
                -> [(vNumber, CBOR.Term)]
                -> CBOR.Decoder s (Map vNumber CBOR.Term)
      decodeMap 0  _     !vs = return $ Map.fromDistinctAscList $ reverse vs
      decodeMap !l !prev !vs = do
        vNumberTerm <- CBOR.decodeTerm
        vParams <- CBOR.decodeTerm
        case decodeTerm versionNumberCodec vNumberTerm of
          -- error when decoding un-recognized version; skip the version
          -- TODO: include error in the dictionary
          Left _        -> decodeMap (pred l) prev vs

          Right vNumber -> do
            let next = Just vNumber
            unless (next > prev)
              $ fail "codecHandshake.Propose: unordered version"
            decodeMap (pred l) next ((vNumber, vParams) : vs)

      decode :: forall (pr :: PeerRole) s (st :: Handshake vNumber CBOR.Term).
                PeerHasAgency pr st
             -> CBOR.Decoder s (SomeMessage st)
      decode stok = do
        _ <- CBOR.decodeListLen
        key <- CBOR.decodeWord
        case (stok, key) of
          (ClientAgency TokPropose, 0) -> do
            l  <- CBOR.decodeMapLen
            vMap <- decodeMap l Nothing []
            pure $ SomeMessage $ MsgProposeVersions vMap
          (ServerAgency TokConfirm, 1) -> do
            v <- decodeTerm versionNumberCodec <$> CBOR.decodeTerm
            case v of
              -- at this stage we can throw exception when decoding
              -- version number: 'MsgAcceptVersion' must send us back
              -- version which we know how to decode
              Left e -> fail ("codecHandshake.MsgAcceptVersion: not recognized version: " ++ show e)
              Right vNumber ->
                SomeMessage . MsgAcceptVersion vNumber <$> CBOR.decodeTerm
          (ServerAgency TokConfirm, 2) -> SomeMessage . MsgRefuse <$> CBOR.decode

          (ClientAgency TokPropose, _) -> fail "codecHandshake.Propose: unexpected key"
          (ServerAgency TokConfirm, _) -> fail "codecHandshake.Confirm: unexpected key"
