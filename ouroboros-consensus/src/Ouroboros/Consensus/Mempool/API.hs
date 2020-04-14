{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE OverloadedStrings       #-}
{-# LANGUAGE RankNTypes              #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE StandaloneDeriving      #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Ouroboros.Consensus.Mempool.API (
    Mempool(..)
  , addTxs
  , BlockLedgerState(..)
  , MempoolCapacityBytes (..)
  , MempoolSnapshot(..)
  , ApplyTx(..)
  , HasTxId(..)
  , GenTxId
  , MempoolAddTxResult (..)
  , isMempoolTxAdded
  , isMempoolTxRejected
  , MempoolSize (..)
  , TraceEventMempool(..)
  , HasTxs(..)
    -- * Re-exports
  , TxSizeInBytes
  ) where

import           Control.Monad.Except
import           Data.Word (Word32)
import           GHC.Stack (HasCallStack)

import           Ouroboros.Network.Protocol.TxSubmission.Type (TxSizeInBytes)

import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Util.IOLike

class ( UpdateLedger blk
      , NoUnexpectedThunks (GenTx blk)
      ) => ApplyTx blk where
  -- | Generalized transaction
  --
  -- The mempool (and, accordingly, blocks) consist of "generalized
  -- transactions"; this could be "proper" transactions (transferring funds) but
  -- also other kinds of things such as update proposals, delegations, etc.
  data family GenTx blk :: *

  -- | Return the post-serialization size in bytes of a 'GenTx'.
  txSize :: GenTx blk -> TxSizeInBytes

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

-- | Transactions with an identifier
--
-- The mempool will use these to locate transactions, so two different
-- transactions should have different identifiers.
class (Ord (TxId tx), NoUnexpectedThunks (TxId tx)) => HasTxId tx where
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

class HasTxs blk where
  -- | Return the transactions part of the given block in no particular order.
  extractTxs :: blk -> [GenTx blk]

-- | Mempool
--
-- The mempool is the set of transactions that should be included in the next
-- block. In principle this is a /set/ of all the transactions that we receive
-- from our peers. In order to avoid flooding the network with invalid
-- transactions,  however, we only want to keep /valid/ transactions in the
-- mempool. That raises the question: valid with respect to which ledger state?
--
-- We opt for a very simple answer to this: the mempool will be interpreted
-- as a /list/ of transactions; which are validated strictly in order, starting
-- from the current ledger state. This has a number of advantages:
--
-- * It's simple to implement and it's efficient. In particular, no search for
--   a valid subset is ever required.
-- * When producing a block, we can simply take the longest possible prefix
--   of transactions that fits in a block.
-- * It supports wallets that submit dependent transactions (where later
--   transaction depends on outputs from earlier ones).
data Mempool m blk idx = Mempool {
      -- | Add a bunch of transactions (oldest to newest)
      --
      -- As long as we keep the mempool entirely in-memory this could live in
      -- @STM m@; we keep it in @m@ instead to leave open the possibility of
      -- persistence.
      --
      -- The new transactions provided will be validated, /in order/, against
      -- the ledger state obtained by applying all the transactions already in
      -- the Mempool to it. Transactions which are found to be invalid, with
      -- respect to the ledger state, are dropped, whereas valid transactions
      -- are added to the mempool.
      --
      -- Note that transactions that are invalid, with respect to the ledger
      -- state, will /never/ be added to the mempool. However, it is possible
      -- that, at a given point in time, transactions which were once valid
      -- but are now invalid, with respect to the current ledger state, could
      -- exist within the mempool until they are revalidated and dropped from
      -- the mempool via a call to 'syncWithLedger' or by the background
      -- thread that watches the ledger for changes.
      --
      -- This function will return two lists
      --
      -- 1. A list containing the following transactions:
      --
      --    * Those transactions provided which were found to be valid, along
      --      with 'MempoolTxAdded' for their accompanying
      --      'MempoolAddTxResult' values. These transactions are now in the
      --      Mempool.
      --    * Those transactions provided which were found to be invalid,
      --      along with their accompanying validation errors. These
      --      transactions are not in the Mempool.
      --
      -- 2. A list containing the transactions that have not yet been added
      --    yet, as the capacity of the Mempool has been reached. I.e., there
      --    is no space in the Mempool to add the first transaction in this
      --    list. Note that we won't try to add smaller transactions after
      --    that first transaction because they might depend on the first
      --    transaction.
      --
      -- POSTCONDITION:
      -- > (processed, toProcess) <- tryAddTxs txs
      -- > map fst processed ++ toProcess == txs
      --
      -- Note that previously valid transaction that are now invalid with
      -- respect to the current ledger state are dropped from the mempool, but
      -- are not part of the first returned list (nor the second).
      --
      -- In principle it is possible that validation errors are transient; for
      -- example, it is possible that a transaction is rejected because one of
      -- its inputs is not /yet/ available in the UTxO (the transaction it
      -- depends on is not yet in the chain, nor in the mempool). In practice
      -- however it is likely that rejected transactions will still be
      -- rejected later, and should just be dropped.
      --
      -- It is important to note one important special case of transactions
      -- being "invalid": a transaction will /also/ be considered invalid if
      -- /that very same transaction/ is already included on the blockchain
      -- (after all, by definition that must mean its inputs have been used).
      -- Rejected transactions are therefore not necessarily a sign of
      -- malicious behaviour. Indeed, we would expect /most/ transactions that
      -- are reported as invalid by 'tryAddTxs' to be invalid precisely
      -- because they have already been included. Distinguishing between these
      -- two cases can be done in theory, but it is expensive unless we have
      -- an index of transaction hashes that have been included on the
      -- blockchain.
      --
      tryAddTxs      :: [GenTx blk]
                     -> m ( [(GenTx blk, MempoolAddTxResult blk)]
                          , [GenTx blk]
                          )

      -- | Manually remove the given transactions from the mempool.
    , removeTxs      :: [GenTxId blk] -> m ()

      -- | Sync the transactions in the mempool with the current ledger state
      --  of the 'ChainDB'.
      --
      -- The transactions that exist within the mempool will be revalidated
      -- against the current ledger state. Transactions which are found to be
      -- invalid with respect to the current ledger state, will be dropped
      -- from the mempool, whereas valid transactions will remain.
      --
      -- We keep this in @m@ instead of @STM m@ to leave open the possibility
      -- of persistence. Additionally, this makes it possible to trace the
      -- removal of invalid transactions.
      --
      -- n.b. in our current implementation, when one opens a mempool, we
      -- spawn a thread which performs this action whenever the 'ChainDB' tip
      -- point changes.
    , syncWithLedger :: m (MempoolSnapshot blk idx)

      -- | Get a snapshot of the current mempool state. This allows for
      -- further pure queries on the snapshot.
      --
      -- This doesn't look at the ledger state at all.
    , getSnapshot    :: STM m (MempoolSnapshot blk idx)

      -- | Get a snapshot of the mempool state that is valid with respect to
      -- the given ledger state
      --
      -- This does not update the state of the mempool.
    , getSnapshotFor :: BlockLedgerState blk -> STM m (MempoolSnapshot blk idx)

      -- | Get the mempool's capacity in bytes.
    , getCapacity    :: STM m MempoolCapacityBytes

      -- | Represents the initial value at which the transaction ticket number
      -- counter will start (i.e. the zeroth ticket number).
    , zeroIdx        :: idx
    }

-- | The result of attempting to add a transaction to the mempool.
data MempoolAddTxResult blk
  = MempoolTxAdded
    -- ^ The transaction was added to the mempool.
  | MempoolTxRejected !(ApplyTxErr blk)
    -- ^ The transaction was rejected and could not be added to the mempool
    -- for the specified reason.

deriving instance Eq (ApplyTxErr blk) => Eq (MempoolAddTxResult blk)
deriving instance Show (ApplyTxErr blk) => Show (MempoolAddTxResult blk)

isMempoolTxAdded :: MempoolAddTxResult blk -> Bool
isMempoolTxAdded MempoolTxAdded = True
isMempoolTxAdded _              = False

isMempoolTxRejected :: MempoolAddTxResult blk -> Bool
isMempoolTxRejected (MempoolTxRejected _) = True
isMempoolTxRejected _                     = False

-- | Wrapper around 'implTryAddTxs' that blocks until all transaction have
-- either been added to the Mempool or rejected.
--
-- This function does not sync the Mempool contents with the ledger state in
-- case the latter changes, it relies on the background thread to do that.
--
-- POSTCONDITON:
-- > processed <- addTxs mpEnv txs
-- > map fst processed == txs
addTxs
  :: forall m blk idx. (MonadSTM m, ApplyTx blk)
  => Mempool m blk idx
  -> [GenTx blk]
  -> m [(GenTx blk, MempoolAddTxResult blk)]
addTxs mempool = \txs -> do
    (processed, toAdd) <- tryAddTxs mempool txs
    case toAdd of
      [] -> return processed
      _  -> go [processed] toAdd
  where
    go
      :: [[(GenTx blk, MempoolAddTxResult blk)]]
         -- ^ The outer list is in reverse order, but all the inner lists will
         -- be in the right order.
      -> [GenTx blk]
      -> m [(GenTx blk, MempoolAddTxResult blk)]
    go acc []         = return (concat (reverse acc))
    go acc txs@(tx:_) = do
      let firstTxSize = txSize tx
      -- Wait until there's at least room for the first transaction we're
      -- trying to add, otherwise there's no point in trying to add it.
      atomically $ do
        curSize <- msNumBytes . snapshotMempoolSize <$> getSnapshot mempool
        MempoolCapacityBytes capacity <- getCapacity mempool
        check (curSize + firstTxSize <= capacity)
      -- It is possible that between the check above and the call below, other
      -- transactions are added, stealing our spot, but that's fine, we'll
      -- just recurse again without progress.
      (added, toAdd) <- tryAddTxs mempool txs
      go (added:acc) toAdd

-- | The ledger state wrt to which we should produce a block
--
-- The transactions in the mempool will be part of the body of a block, but a
-- block consists of a header and a body, and the full validation of a block
-- consists of first processing its header and only then processing the body.
-- This is important, because processing the header may change the state of the
-- ledger: the update system might be updated, scheduled delegations might be
-- applied, etc., and such changes should take effect before we validate any
-- transactions.
data BlockLedgerState blk =
    -- | The slot number of the block is known
    --
    -- This will only be the case when we realized that we are the slot leader
    -- and we are actually producing a block. It is the caller's responsibilit
    -- to call 'applyChainTick' and produce the ticked ledger state.
    TxsForBlockInKnownSlot (TickedLedgerState blk)

    -- | The slot number of the block is not yet known
    --
    -- When we are validating transactions before we know in which block they
    -- will end up, we have to make an assumption about which slot number to use
    -- for 'applyChainTick' to prepare the ledger state; we will assume that
    -- they will end up in the slot after the slot at the tip of the ledger.
  | TxsForBlockInUnknownSlot (LedgerState blk)

-- | Represents the maximum number of bytes worth of transactions that a
-- 'Mempool' can contain.
newtype MempoolCapacityBytes = MempoolCapacityBytes Word32
  deriving (Eq, Show)

-- | A pure snapshot of the contents of the mempool. It allows fetching
-- information about transactions in the mempool, and fetching individual
-- transactions.
--
-- This uses a transaction sequence number type for identifying transactions
-- within the mempool sequence. The sequence number is local to this mempool,
-- unlike the transaction hash. This allows us to ask for all transactions
-- after a known sequence number, to get new transactions. It is also used to
-- look up individual transactions.
--
-- Note that it is expected that 'getTx' will often return 'Nothing'
-- even for tx sequence numbers returned in previous snapshots. This happens
-- when the transaction has been removed from the mempool between snapshots.
--
data MempoolSnapshot blk idx = MempoolSnapshot {
    -- | Get all transactions (oldest to newest) in the mempool snapshot along
    -- with their ticket number.
    snapshotTxs         :: [(GenTx blk, idx)]

    -- | Get all transactions (oldest to newest) in the mempool snapshot,
    -- along with their ticket number, which are associated with a ticket
    -- number greater than the one provided.
  , snapshotTxsAfter    :: idx -> [(GenTx blk, idx)]

    -- | Get as many transactions (oldest to newest) from the mempool
    -- snapshot, along with their ticket number, such that their combined size
    -- is <= the given limit (in bytes).
  , snapshotTxsForSize  :: Word32 -> [(GenTx blk, idx)]

    -- | Get a specific transaction from the mempool snapshot by its ticket
    -- number, if it exists.
  , snapshotLookupTx    :: idx -> Maybe (GenTx blk)

    -- | Determine whether a specific transaction exists within the mempool
    -- snapshot.
  , snapshotHasTx       :: GenTxId blk -> Bool

    -- | Get the size of the mempool snapshot.
  , snapshotMempoolSize :: MempoolSize
  }

-- | The size of a mempool.
data MempoolSize = MempoolSize
  { msNumTxs   :: !Word32
    -- ^ The number of transactions in the mempool.
  , msNumBytes :: !Word32
    -- ^ The summed byte size of all the transactions in the mempool.
  } deriving (Eq, Show)

instance Semigroup MempoolSize where
  MempoolSize xt xb <> MempoolSize yt yb = MempoolSize (xt + yt) (xb + yb)

instance Monoid MempoolSize where
  mempty = MempoolSize { msNumTxs = 0, msNumBytes = 0 }
  mappend = (<>)

-- | Events traced by the Mempool.
data TraceEventMempool blk
  = TraceMempoolAddedTx
      !(GenTx blk)
      -- ^ New, valid transaction that was added to the Mempool.
      !MempoolSize
      -- ^ The size of the Mempool before adding the transaction.
      !MempoolSize
      -- ^ The size of the Mempool after adding the transaction.
  | TraceMempoolRejectedTx
      !(GenTx blk)
      -- ^ New, invalid transaction thas was rejected and thus not added to
      -- the Mempool.
      !(ApplyTxErr blk)
      -- ^ The reason for rejecting the transaction.
      !MempoolSize
      -- ^ The current size of the Mempool.
  | TraceMempoolRemoveTxs
      ![GenTx blk]
      -- ^ Previously valid transactions that are no longer valid because of
      -- changes in the ledger state. These transactions have been removed
      -- from the Mempool.
      !MempoolSize
      -- ^ The current size of the Mempool.
  | TraceMempoolManuallyRemovedTxs
      ![GenTxId blk]
      -- ^ Transactions that have been manually removed from the Mempool.
      ![GenTx blk]
      -- ^ Previously valid transactions that are no longer valid because they
      -- dependend on transactions that were manually removed from the
      -- Mempool. These transactions have also been removed from the Mempool.
      --
      -- This list shares not transactions with the list of manually removed
      -- transactions.
      !MempoolSize
      -- ^ The current size of the Mempool.

deriving instance ( Eq (GenTx blk)
                  , Eq (GenTxId blk)
                  , Eq (ApplyTxErr blk)
                  ) => Eq (TraceEventMempool blk)

deriving instance ( Show (GenTx blk)
                  , Show (GenTxId blk)
                  , Show (ApplyTxErr blk)
                  ) => Show (TraceEventMempool blk)
