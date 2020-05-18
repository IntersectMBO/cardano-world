{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.Byron.Ledger.HeaderValidation (
    ByronOtherHeaderEnvelopeError(..)
  ) where

import           Control.Monad.Except
import           Data.Word
import           GHC.Generics (Generic)

import           Cardano.Prelude (NoUnexpectedThunks)

import qualified Cardano.Chain.Slotting as CC

import           Ouroboros.Network.Block

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.HeaderValidation

import           Ouroboros.Consensus.Byron.Ledger.Block
import           Ouroboros.Consensus.Byron.Ledger.Config
import           Ouroboros.Consensus.Byron.Ledger.Orphans ()
import           Ouroboros.Consensus.Byron.Ledger.PBFT ()

{-------------------------------------------------------------------------------
  Envelope
-------------------------------------------------------------------------------}

type ByronTipInfo = (HeaderHash ByronBlock, IsEBB)

instance HasAnnTip ByronBlock where
  type TipInfo ByronBlock = ByronTipInfo
  tipInfoHash _ = fst
  getTipInfo b  = (blockHash b, byronHeaderIsEBB b)

data ByronOtherHeaderEnvelopeError =
    UnexpectedEBBInSlot !SlotNo
  deriving (Eq, Show, Generic, NoUnexpectedThunks)

instance BasicEnvelopeValidation ByronBlock where
  expectedFirstBlockNo  _ = BlockNo 0
  minimumPossibleSlotNo _ = SlotNo 0

  -- EBB shares its block number with its predecessor
  expectedNextBlockNo _ (_, prevIsEBB) (_, curIsEBB) b =
     case (prevIsEBB, curIsEBB) of
       (IsNotEBB, IsEBB) -> b
       _otherwise        -> succ b

  -- EBB shares its slot number with its successor
  minimumNextSlotNo _ (_, prevIsEBB) (_, curIsEBB) s =
      case (prevIsEBB, curIsEBB) of
        (IsEBB, IsNotEBB) -> s
        _otherwise        -> succ s

instance ValidateEnvelope ByronBlock where
  type OtherHeaderEnvelopeError ByronBlock = ByronOtherHeaderEnvelopeError

  additionalEnvelopeChecks cfg _ledgerView hdr =
      when (fromIsEBB newIsEBB && not (canBeEBB actualSlotNo)) $
        throwError $ UnexpectedEBBInSlot actualSlotNo
    where
      actualSlotNo :: SlotNo
      actualSlotNo = blockSlot hdr

      newIsEBB :: IsEBB
      newIsEBB = byronHeaderIsEBB hdr

      canBeEBB :: SlotNo -> Bool
      canBeEBB (SlotNo s) = s `mod` epochSlots == 0

      epochSlots :: Word64
      epochSlots = CC.unEpochSlots $ byronEpochSlots $ configCodec cfg
