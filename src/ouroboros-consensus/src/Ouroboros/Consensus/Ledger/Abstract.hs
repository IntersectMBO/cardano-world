{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | Interface to the ledger layer
module Ouroboros.Consensus.Ledger.Abstract (
    -- * Definition of a ledger independent of a choice of block
    IsLedger(..)
  , LedgerCfg
  , Ticked(..)
  , ApplyBlock(..)
    -- ** Derived
  , tickThenApply
  , tickThenReapply
  , foldLedger
  , refoldLedger
    -- * Link block to its ledger
  , LedgerState
  , UpdateLedger
    -- ** Short-hand
  , LedgerConfig
  , LedgerError
  , TickedLedgerState
  , ledgerTipHash
  , ledgerTipPoint'
  , ledgerTipSlot
    -- * Queries
  , QueryLedger(..)
  , ShowQuery(..)
  ) where

import           Control.Monad.Except
import           Data.Maybe (isJust)
import           Data.Proxy
import           Data.Type.Equality ((:~:))
import           GHC.Generics (Generic)
import           GHC.Stack (HasCallStack)

import           Cardano.Prelude (NoUnexpectedThunks)

import           Ouroboros.Network.Block
import           Ouroboros.Network.Point (WithOrigin)
import           Ouroboros.Network.Protocol.LocalStateQuery.Codec (Some (..))
import           Ouroboros.Network.Protocol.LocalStateQuery.Type
                     (ShowQuery (..))

import           Ouroboros.Consensus.Block.Abstract
import           Ouroboros.Consensus.Util (repeatedly, repeatedlyM)

{-------------------------------------------------------------------------------
  Definition of a ledger independent of a choice of block
-------------------------------------------------------------------------------}

-- | Static environment required for the ledger
type family LedgerCfg l :: *

class ( -- Requirements on the ledger state itself
        Show               l
      , Eq                 l
      , NoUnexpectedThunks l
        -- Requirements on 'LedgerCfg'
      , NoUnexpectedThunks (LedgerCfg l)
        -- Requirements on 'LedgerErr'
      , Show               (LedgerErr l)
      , Eq                 (LedgerErr l)
      , NoUnexpectedThunks (LedgerErr l)
      ) => IsLedger l where
  -- | Errors that can arise when updating the ledger
  --
  -- This is defined here rather than in 'ApplyBlock', since the /type/ of
  -- these errors does not depend on the type of the block.
  type family LedgerErr l :: *

  -- | Apply "slot based" state transformations
  --
  -- When a block is applied to the ledger state, a number of things happen
  -- purely based on the slot number of that block. For example:
  --
  -- * In Byron, scheduled updates are applied, and the update system state is
  --   updated.
  -- * In Shelley, delegation state is updated (on epoch boundaries).
  --
  -- The consensus layer must be able to apply such a "chain tick" function,
  -- primarily when validating transactions in the mempool (which, conceptually,
  -- live in "some block in the future") or when extracting valid transactions
  -- from the mempool to insert into a new block to be produced.
  --
  -- This is not allowed to throw any errors. After all, if this could fail,
  -- it would mean a /previous/ block set up the ledger state in such a way
  -- that as soon as a certain slot was reached, /any/ block would be invalid.
  --
  -- PRECONDITION: The slot number must be strictly greater than the slot at
  -- the tip of the ledger (except for EBBs, obviously..).
  applyChainTick :: LedgerCfg l -> SlotNo -> l -> Ticked l

-- | Mark a ledger state or ledger view as " ticked "
--
-- Ticking refers to the passage of time (the ticking of the clock). When a
-- ledger state or ledger view is marked as ticked, it means  that time-related
-- changes have been applied to ledger state, or the right view has been
-- forecast for that slot.
--
-- Some examples of time related changes:
--
-- * Scheduled delegations might have been applied in Byron
-- * New leader schedule computed for Shelley
-- * Transition from Byron to Shelley activated in the hard fork combinator.
--
-- When applying a block, the ledger must first have been advanced to the slot
-- of the block.
data Ticked l = Ticked {
      -- | The slot number supplied to 'applyChainTick'
      tickedSlotNo      :: !SlotNo

      -- | The underlying ledger state
      --
      -- NOTE: 'applyChainTick' should /not/ change the tip of the underlying
      -- ledger state, which should still refer to the most recent applied
      -- /block/. In other words, we should have
      --
      -- >    ledgerTipPoint (tickedLedgerState (applyChainTick cfg slot st)
      -- > == ledgerTipPoint st
    , tickedLedgerState :: !l
    }
  deriving stock    (Generic, Functor)
  deriving anyclass (NoUnexpectedThunks)

{-------------------------------------------------------------------------------
  Apply block to ledger state
-------------------------------------------------------------------------------}

class ( IsLedger l
      , HasHeader blk
      , HasHeader (Header blk)
      ) => ApplyBlock l blk where
  -- | Apply a block to the ledger state.
  --
  -- This is passed the ledger state ticked with the slot of the given block,
  -- so 'applyChainTick' has already been called.
  applyLedgerBlock :: HasCallStack
                   => LedgerCfg l -> blk -> Ticked l -> Except (LedgerErr l) l

  -- | Re-apply a block to the very same ledger state it was applied in before.
  --
  -- Since a block can only be applied to a single, specific, ledger state,
  -- if we apply a previously applied block again it will be applied in the
  -- very same ledger state, and therefore can't possibly fail.
  --
  -- It is worth noting that since we already know that the block is valid in
  -- the provided ledger state, the ledger layer should not perform /any/
  -- validation checks.
  reapplyLedgerBlock :: HasCallStack
                     => LedgerCfg l -> blk -> Ticked l -> l

  -- | Point of the most recently applied block
  --
  -- Should be 'genesisPoint' when no blocks have been applied yet
  ledgerTipPoint :: l -> Point blk

{-------------------------------------------------------------------------------
  Derived functionality
-------------------------------------------------------------------------------}

tickThenApply :: ApplyBlock l blk
              => LedgerCfg l -> blk -> l -> Except (LedgerErr l) l
tickThenApply cfg blk =
      applyLedgerBlock cfg blk
    . applyChainTick   cfg (blockSlot blk)

tickThenReapply :: ApplyBlock l blk  => LedgerCfg l -> blk -> l -> l
tickThenReapply cfg blk =
      reapplyLedgerBlock cfg blk
    . applyChainTick     cfg (blockSlot blk)

foldLedger :: ApplyBlock l blk
           => LedgerCfg l -> [blk] -> l -> Except (LedgerErr l) l
foldLedger = repeatedlyM . tickThenApply

refoldLedger :: ApplyBlock l blk => LedgerCfg l -> [blk] -> l -> l
refoldLedger = repeatedly . tickThenReapply

{-------------------------------------------------------------------------------
  Link block to its ledger
-------------------------------------------------------------------------------}

-- | Ledger state associated with a block
data family LedgerState blk :: *

-- | Interaction with the ledger layer
class ApplyBlock (LedgerState blk) blk => UpdateLedger blk

{-------------------------------------------------------------------------------
  Short-hand
-------------------------------------------------------------------------------}

type LedgerConfig      blk = LedgerCfg (LedgerState blk)
type LedgerError       blk = LedgerErr (LedgerState blk)
type TickedLedgerState blk = Ticked    (LedgerState blk)

-- | Wrapper around 'ledgerTipPoint' that uses a proxy to fix @blk@
--
-- This is occassionally useful to guide type inference
ledgerTipPoint' :: UpdateLedger blk => Proxy blk -> LedgerState blk -> Point blk
ledgerTipPoint' _ = ledgerTipPoint

ledgerTipHash :: forall blk. UpdateLedger blk => LedgerState blk -> ChainHash blk
ledgerTipHash = pointHash . (ledgerTipPoint' (Proxy @blk))

ledgerTipSlot :: forall blk. UpdateLedger blk => LedgerState blk -> WithOrigin SlotNo
ledgerTipSlot = pointSlot . (ledgerTipPoint' (Proxy @blk))

{-------------------------------------------------------------------------------
  Queries
-------------------------------------------------------------------------------}

-- | Query the ledger state.
--
-- Used by the LocalStateQuery protocol to allow clients to query the ledger
-- state.
class (UpdateLedger blk, ShowQuery (Query blk)) => QueryLedger blk where

  -- | Different queries supported by the ledger, indexed by the result type.
  data family Query  blk :: * -> *

  -- | Answer the given query about the ledger state.
  answerQuery :: LedgerConfig blk -> Query blk result -> LedgerState blk -> result

  -- | Generalisation of value-level equality of two queries.
  eqQuery :: Query blk result1 -> Query blk result2
          -> Maybe (result1 :~: result2)

instance QueryLedger blk => Eq (Some (Query blk)) where
  Some qry == Some qry' = isJust (eqQuery qry qry')

deriving instance (forall result. Show (Query blk result)) => Show (Some (Query blk))
