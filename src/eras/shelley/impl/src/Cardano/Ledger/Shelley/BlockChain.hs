{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Shelley.BlockChain
  ( TxSeq (TxSeq, txSeqTxns', TxSeq'),
    constructMetadata,
    txSeqTxns,
    bbHash,
    bBodySize,
    slotToNonce,
    --
    incrBlocks,
    coreAuxDataBytes,
    txSeqDecoder,
  )
where

import Cardano.Binary
  ( Annotator (..),
    Decoder,
    FromCBOR (fromCBOR),
    ToCBOR (..),
    encodePreEncoded,
    serializeEncoding,
    serializeEncoding',
    withSlice,
  )
import qualified Cardano.Crypto.Hash.Class as Hash
import Cardano.Ledger.BaseTypes
  ( BlocksMade (..),
    Nonce (..),
    StrictMaybe (..),
    mkNonceFromNumber,
    strictMaybeToMaybe,
  )
import Cardano.Ledger.Block (BlockAnn)
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era (Crypto, Era)
import Cardano.Ledger.Hashes (EraIndependentBlockBody)
import Cardano.Ledger.Keys (Hash, KeyHash, KeyRole (..))
import Cardano.Ledger.SafeHash (SafeToHash (..))
import Cardano.Ledger.Serialization
  ( ToCBORGroup (..),
    decodeMap,
    decodeSeq,
    encodeFoldableEncoder,
    encodeFoldableMapEncoder,
  )
import Cardano.Ledger.Shelley.Tx (Tx, segwitTx)
import Cardano.Ledger.Slot (SlotNo (..))
import Control.Monad (unless)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Coerce (coerce)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Sequence.Strict (StrictSeq)
import qualified Data.Sequence.Strict as StrictSeq
import Data.Typeable
import GHC.Generics (Generic)
import GHC.Records (HasField (..))
import NoThunks.Class (AllowThunksIn (..), NoThunks (..))

data TxSeq era = TxSeq'
  { txSeqTxns' :: !(StrictSeq (Tx era)),
    txSeqBodyBytes :: BSL.ByteString,
    txSeqWitsBytes :: BSL.ByteString,
    txSeqMetadataBytes :: BSL.ByteString
    -- bytes representing a (Map index metadata). Missing indices have SNothing for metadata
  }
  deriving (Generic)

deriving via
  AllowThunksIn
    '[ "txSeqBodyBytes",
       "txSeqWitsBytes",
       "txSeqMetadataBytes"
     ]
    (TxSeq era)
  instance
    (Typeable era, NoThunks (Tx era)) => NoThunks (TxSeq era)

deriving stock instance
  Show (Tx era) =>
  Show (TxSeq era)

deriving stock instance
  Eq (Tx era) =>
  Eq (TxSeq era)

-- ===========================
-- Getting bytes from pieces of a Core.Tx

coreWitnessBytes ::
  forall era.
  ( SafeToHash (Core.Witnesses era)
  ) =>
  Tx era ->
  ByteString
coreWitnessBytes coretx =
  originalBytes @(Core.Witnesses era) $
    getField @"wits" coretx

coreBodyBytes ::
  forall era.
  ( SafeToHash (Core.TxBody era)
  ) =>
  Tx era ->
  ByteString
coreBodyBytes coretx =
  originalBytes @(Core.TxBody era) $
    getField @"body" coretx

coreAuxDataBytes ::
  forall era.
  ( SafeToHash (Core.AuxiliaryData era)
  ) =>
  Tx era ->
  StrictMaybe ByteString
coreAuxDataBytes coretx = getbytes <$> getField @"auxiliaryData" coretx
  where
    getbytes auxdata = originalBytes @(Core.AuxiliaryData era) auxdata

-- ===========================

-- | Constuct a TxSeq (with all it bytes) from just Core.Tx's
pattern TxSeq ::
  forall era.
  ( Era era,
    SafeToHash (Core.Witnesses era)
  ) =>
  StrictSeq (Tx era) ->
  TxSeq era
pattern TxSeq xs <-
  TxSeq' xs _ _ _
  where
    TxSeq txns =
      let serializeFoldable x =
            serializeEncoding $
              encodeFoldableEncoder encodePreEncoded x
          metaChunk index m = encodePair <$> strictMaybeToMaybe m
            where
              encodePair metadata = toCBOR index <> encodePreEncoded metadata
       in TxSeq'
            { txSeqTxns' = txns,
              -- bytes encoding Seq(Core.TxBody era)
              txSeqBodyBytes = serializeFoldable $ coreBodyBytes @era <$> txns,
              -- bytes encoding Seq(Core.Witnesses era)
              txSeqWitsBytes = serializeFoldable $ coreWitnessBytes @era <$> txns,
              -- bytes encoding a (Map Int (Core.AuxiliaryData))
              txSeqMetadataBytes =
                serializeEncoding . encodeFoldableMapEncoder metaChunk $
                  coreAuxDataBytes @era <$> txns
            }

{-# COMPLETE TxSeq #-}

txSeqTxns :: TxSeq era -> StrictSeq (Tx era)
txSeqTxns (TxSeq' ts _ _ _) = ts

instance
  forall era.
  (Era era) =>
  ToCBORGroup (TxSeq era)
  where
  toCBORGroup (TxSeq' _ bodyBytes witsBytes metadataBytes) =
    encodePreEncoded $
      BSL.toStrict $
        bodyBytes <> witsBytes <> metadataBytes
  encodedGroupSizeExpr size _proxy =
    encodedSizeExpr size (Proxy :: Proxy ByteString)
      + encodedSizeExpr size (Proxy :: Proxy ByteString)
      + encodedSizeExpr size (Proxy :: Proxy ByteString)
  listLen _ = 3
  listLenBound _ = 3

-- | Hash a given block body
bbHash ::
  forall era.
  (Era era) =>
  TxSeq era ->
  Hash (Crypto era) EraIndependentBlockBody
bbHash (TxSeq' _ bodies wits md) =
  coerce $
    hashStrict
      ( hashPart bodies
          <> hashPart wits
          <> hashPart md
      )
  where
    hashStrict :: ByteString -> Hash (Crypto era) ByteString
    hashStrict = Hash.hashWith id
    hashPart = Hash.hashToBytes . hashStrict . BSL.toStrict

-- | Given a size and a mapping from indices to maybe metadata,
--  return a sequence whose size is the size paramater and
--  whose non-Nothing values correspond to the values in the mapping.
constructMetadata ::
  forall era.
  Int ->
  Map Int (Annotator (Core.AuxiliaryData era)) ->
  Seq (Maybe (Annotator (Core.AuxiliaryData era)))
constructMetadata n md = fmap (`Map.lookup` md) (Seq.fromList [0 .. n - 1])

-- | The parts of the Tx in Blocks that have to have FromCBOR(Annotator x) instances.
--   These are exactly the parts that are SafeToHash.
-- | Decode a TxSeq, used in decoding a Block.
txSeqDecoder ::
  forall era.
  BlockAnn era =>
  Bool ->
  forall s. Decoder s (Annotator (TxSeq era))
txSeqDecoder lax = do
  (bodies, bodiesAnn) <- withSlice $ decodeSeq fromCBOR
  (wits, witsAnn) <- withSlice $ decodeSeq fromCBOR
  let b = length bodies
      inRange x = (0 <= x) && (x <= (b - 1))
      w = length wits
  (metadata, metadataAnn) <- withSlice $
    do
      m <- decodeMap fromCBOR fromCBOR
      unless -- TODO this PR introduces this new test, That didn't used to run in the Shelley
        (lax || all inRange (Map.keysSet m)) -- Era,  Is it possible there might be some blocks, that should have been caught on the chain?
        (fail ("Some Auxiliarydata index is not in the range: 0 .. " ++ show (b - 1)))
      pure (constructMetadata @era b m)

  unless
    (lax || b == w)
    ( fail $
        "different number of transaction bodies ("
          <> show b
          <> ") and witness sets ("
          <> show w
          <> ")"
    )

  let txns =
        sequenceA $
          StrictSeq.forceToStrict $
            Seq.zipWith3 segwitTx bodies wits metadata
  pure $ TxSeq' <$> txns <*> bodiesAnn <*> witsAnn <*> metadataAnn

instance
  (BlockAnn era, Typeable era) =>
  FromCBOR (Annotator (TxSeq era))
  where
  fromCBOR = txSeqDecoder False

bBodySize ::
  ToCBORGroup txSeq => txSeq -> Int
bBodySize = BS.length . serializeEncoding' . toCBORGroup

slotToNonce :: SlotNo -> Nonce
slotToNonce (SlotNo s) = mkNonceFromNumber s

incrBlocks ::
  Bool ->
  KeyHash 'StakePool crypto ->
  BlocksMade crypto ->
  BlocksMade crypto
incrBlocks isOverlay hk b'@(BlocksMade b)
  | isOverlay = b'
  | otherwise = BlocksMade $ case hkVal of
      Nothing -> Map.insert hk 1 b
      Just n -> Map.insert hk (n + 1) b
  where
    hkVal = Map.lookup hk b
