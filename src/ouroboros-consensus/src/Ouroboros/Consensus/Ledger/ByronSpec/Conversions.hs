-- | Conversions from ouroboros-consensus types to the Byron spec types
--
-- Intended for unqualified import.
module Ouroboros.Consensus.Ledger.ByronSpec.Conversions (
    -- * Spec to consensus
    fromByronSpecPrevHash
  , fromByronSpecSlotNo
    -- * Consensus to spec
  , toByronSpecSlotNo
  ) where

import           Cardano.Slotting.Slot (SlotNo (..))

import           Ouroboros.Network.Block (ChainHash (..), HeaderHash)

import qualified Cardano.Spec.Chain.STS.Block as Spec
import qualified Ledger.Core as Spec

{-------------------------------------------------------------------------------
  Spec to consensus
-------------------------------------------------------------------------------}

fromByronSpecPrevHash :: (Spec.Hash -> HeaderHash b)
                      -> Spec.Hash -> ChainHash b
fromByronSpecPrevHash f h
  | h == Spec.genesisHash = GenesisHash
  | otherwise             = BlockHash (f h)

fromByronSpecSlotNo :: Spec.Slot -> SlotNo
fromByronSpecSlotNo (Spec.Slot slot) = SlotNo slot

{-------------------------------------------------------------------------------
  Consensus to spec
-------------------------------------------------------------------------------}

toByronSpecSlotNo :: SlotNo -> Spec.Slot
toByronSpecSlotNo (SlotNo slot) = Spec.Slot slot
