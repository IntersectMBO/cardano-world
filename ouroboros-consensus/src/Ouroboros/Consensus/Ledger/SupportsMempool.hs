{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}
module Ouroboros.Consensus.Ledger.SupportsMempool (
    LedgerSupportsMempool (..)
  , HasTxId (..)
  , GenTxId
  , HasTxs (..)
  ) where

import           Control.Monad.Except
import           Data.Word (Word32)
import           GHC.Stack (HasCallStack)

import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Util.IOLike

class ( UpdateLedger blk
      , NoUnexpectedThunks (GenTx blk)
      , Show (GenTx blk)
      , Show (ApplyTxErr blk)
      ) => LedgerSupportsMempool blk where
  -- | Generalized transaction
  --
  -- The mempool (and, accordingly, blocks) consist of "generalized
  -- transactions"; this could be "proper" transactions (transferring funds) but
  -- also other kinds of things such as update proposals, delegations, etc.
  data family GenTx blk :: *

  -- | Check whether the internal invariants of the transaction hold.
  txInvariant :: GenTx blk -> Bool
  txInvariant = const True

  -- | Updating the ledger with a single transaction may result in a different
  -- error type as when updating it with a block
  type family ApplyTxErr blk :: *

  -- | Apply transaction we have not previously seen before
  applyTx :: LedgerConfig blk
          -> GenTx blk
          -> TickedLedgerState blk
          -> Except (ApplyTxErr blk) (TickedLedgerState blk)

  -- | Re-apply a transaction
  --
  -- When we re-apply a transaction to a potentially different ledger state
  -- expensive checks such as cryptographic hashes can be skipped, but other
  -- checks (such as checking for double spending) must still be done.
  reapplyTx :: HasCallStack
            => LedgerConfig blk
            -> GenTx blk
            -> TickedLedgerState blk
            -> Except (ApplyTxErr blk) (TickedLedgerState blk)

  -- | The maximum number of bytes worth of transactions that can be put into
  -- a block according to the currently adopted protocol parameters of the
  -- ledger state.
  --
  -- This is (conservatively) computed by subtracting the header size and any
  -- other fixed overheads from the maximum block size.
  maxTxCapacity :: TickedLedgerState blk -> Word32

  -- | The maximum transaction size in bytes according to the currently
  -- adopted protocol parameters of the ledger state.
  maxTxSize :: LedgerState blk -> Word32

  -- | Return the post-serialisation size in bytes of a 'GenTx' /when it is
  -- embedded in a block/. This size might differ from the size of the
  -- serialisation used to send and receive the transaction across the
  -- network.
  --
  -- This size is used to compute how many transaction we can put in a block
  -- when forging one.
  --
  -- For example, CBOR-in-CBOR could be used when sending the transaction
  -- across the network, requiring a few extra bytes compared to the actual
  -- in-block serialisation. Another example is the transaction of the
  -- hard-fork combinator which will include an envelope indicating its era
  -- when sent across the network. However, when embedded in the respective
  -- era's block, there is no need for such envelope.
  --
  -- Can be implemented by serialising the 'GenTx', but, ideally, this is
  -- implement more efficiently. E.g., by returning the length of the
  -- annotation.
  txInBlockSize :: GenTx blk -> Word32

-- | Transactions with an identifier
--
-- The mempool will use these to locate transactions, so two different
-- transactions should have different identifiers.
class ( Show               (TxId tx)
      , Ord                (TxId tx)
      , NoUnexpectedThunks (TxId tx)
      ) => HasTxId tx where
  -- | A generalized transaction, 'GenTx', identifier.
  data family TxId tx :: *

  -- | Return the 'TxId' of a 'GenTx'.
  --
  -- NOTE: a 'TxId' must be unique up to ledger rules, i.e., two 'GenTx's with
  -- the same 'TxId' must be the same transaction /according to the ledger/.
  -- However, we do not assume that a 'TxId' uniquely determines a 'GenTx':
  -- two 'GenTx's with the same 'TxId' can differ in, e.g., witnesses.
  --
  -- Should be cheap as this will be called often.
  txId :: tx -> TxId tx

-- | Shorthand: ID of a generalized transaction
type GenTxId blk = TxId (GenTx blk)

-- | Collect all transactions from a block
--
-- This is used for tooling only. We don't require it as part of RunNode
-- (and cannot, because we cannot give an instance for the dual ledger).
class HasTxs blk where
  -- | Return the transactions part of the given block in no particular order.
  extractTxs :: blk -> [GenTx blk]
