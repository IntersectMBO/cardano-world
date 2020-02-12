{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}

module Ouroboros.Consensus.Mempool.Impl (
    openMempool
  , LedgerInterface (..)
  , chainDBLedgerInterface
  , TicketNo
    -- * For testing purposes
  , openMempoolWithoutSyncThread
  ) where

import           Control.Exception (assert)
import           Control.Monad (void)
import           Control.Monad.Except
import           Data.Maybe (isJust, isNothing, listToMaybe)
import qualified Data.Set as Set
import           Data.Typeable
import           Data.Word (Word32)
import           GHC.Generics (Generic)

import           Control.Tracer

import           Ouroboros.Network.Block (ChainHash, Point, SlotNo,
                     StandardHash)
import           Ouroboros.Network.Point (WithOrigin (..))

import           Ouroboros.Storage.ChainDB (ChainDB)
import qualified Ouroboros.Storage.ChainDB.API as ChainDB

import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Mempool.API
import           Ouroboros.Consensus.Mempool.TxSeq (TicketNo, TxSeq (..),
                     TxTicket (..), zeroTicketNo)
import qualified Ouroboros.Consensus.Mempool.TxSeq as TxSeq
import           Ouroboros.Consensus.Util (repeatedly)
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.ResourceRegistry
import           Ouroboros.Consensus.Util.STM (onEachChange)

{-------------------------------------------------------------------------------
  Top-level API
-------------------------------------------------------------------------------}

openMempool :: (IOLike m, ApplyTx blk, HasTxId (GenTx blk), ValidateEnvelope blk)
            => ResourceRegistry m
            -> LedgerInterface m blk
            -> LedgerConfig blk
            -> MempoolCapacityBytes
            -> Tracer m (TraceEventMempool blk)
            -> m (Mempool m blk TicketNo)
openMempool registry ledger cfg capacity tracer = do
    env <- initMempoolEnv ledger cfg capacity tracer
    forkSyncStateOnTipPointChange registry env
    return $ mkMempool env

-- | Unlike 'openMempool', this function does not fork a background thread
-- that synchronises with the ledger state whenever the later changes.
--
-- Intended for testing purposes.
openMempoolWithoutSyncThread
  :: (IOLike m, ApplyTx blk, HasTxId (GenTx blk), ValidateEnvelope blk)
  => LedgerInterface m blk
  -> LedgerConfig blk
  -> MempoolCapacityBytes
  -> Tracer m (TraceEventMempool blk)
  -> m (Mempool m blk TicketNo)
openMempoolWithoutSyncThread ledger cfg capacity tracer =
    mkMempool <$> initMempoolEnv ledger cfg capacity tracer

mkMempool :: (IOLike m, ApplyTx blk, HasTxId (GenTx blk), ValidateEnvelope blk)
          => MempoolEnv m blk -> Mempool m blk TicketNo
mkMempool env = Mempool
    { tryAddTxs      = implTryAddTxs      env
    , removeTxs      = implRemoveTxs      env
    , syncWithLedger = implSyncWithLedger env
    , getSnapshot    = implGetSnapshot    env
    , getSnapshotFor = implGetSnapshotFor env
    , getCapacity    = implGetCapacity    env
    , zeroIdx        = zeroTicketNo
    }

-- | Abstract interface needed to run a Mempool.
data LedgerInterface m blk = LedgerInterface
  { getCurrentLedgerState :: STM m (LedgerState blk)
  }

-- | Create a 'LedgerInterface' from a 'ChainDB'.
chainDBLedgerInterface :: IOLike m => ChainDB m blk -> LedgerInterface m blk
chainDBLedgerInterface chainDB = LedgerInterface
    { getCurrentLedgerState = ledgerState <$> ChainDB.getCurrentLedger chainDB
    }

{-------------------------------------------------------------------------------
  Internal state
-------------------------------------------------------------------------------}

-- | Internal state in the mempool
data InternalState blk = IS {
      -- | Transactions currently in the mempool
      isTxs          :: !(TxSeq (GenTx blk))

      -- | The cached ledger state after applying the transactions in the
      -- Mempool against the chain's ledger state. New transactions will be
      -- validated against this ledger.
      --
      -- INVARIANT: 'isLedgerState' is the ledger resulting from applying the
      -- transactions in 'isTxs' against the ledger identified 'isTip' as tip.
    , isLedgerState  :: !(TickedLedgerState blk)

      -- | The tip of the chain that 'isTxs' was validated against
      --
      -- This comes from the underlying ledger state ('tickedLedgerState')
    , isTip          :: !(ChainHash blk)

      -- | The most recent 'SlotNo' that 'isTxs' was validated against
      --
      -- This comes from 'applyChainTick' ('tickedSlotNo').
    , isSlotNo       :: !SlotNo

      -- | The mempool 'TicketNo' counter.
      --
      -- See 'vrLastTicketNo' for more information.
    , isLastTicketNo :: !TicketNo
    }
  deriving (Generic)

deriving instance ( NoUnexpectedThunks (GenTx blk)
                  , NoUnexpectedThunks (LedgerState blk)
                  , StandardHash blk
                  , Typeable blk
                  ) => NoUnexpectedThunks (InternalState blk)

-- | \( O(1) \). Return the number of transactions in the internal state of
-- the Mempool paired with their total size in bytes.
isMempoolSize :: InternalState blk -> MempoolSize
isMempoolSize = TxSeq.toMempoolSize . isTxs

data MempoolEnv m blk = MempoolEnv {
      mpEnvLedger    :: LedgerInterface m blk
    , mpEnvLedgerCfg :: LedgerConfig blk
    , mpEnvCapacity  :: !MempoolCapacityBytes
    , mpEnvStateVar  :: StrictTVar m (InternalState blk)
    , mpEnvTracer    :: Tracer m (TraceEventMempool blk)
    }

initInternalState
  :: UpdateLedger blk
  => TicketNo  -- ^ Used for 'isLastTicketNo'
  -> TickedLedgerState blk
  -> InternalState blk
initInternalState lastTicketNo st = IS {
      isTxs          = TxSeq.Empty
    , isLedgerState  = st
    , isTip          = ledgerTipHash $ tickedLedgerState st
    , isSlotNo       = tickedSlotNo st
    , isLastTicketNo = lastTicketNo
    }

initMempoolEnv :: (IOLike m, ApplyTx blk, ValidateEnvelope blk)
               => LedgerInterface m blk
               -> LedgerConfig blk
               -> MempoolCapacityBytes
               -> Tracer m (TraceEventMempool blk)
               -> m (MempoolEnv m blk)
initMempoolEnv ledgerInterface cfg capacity tracer = do
    st <- atomically $ getCurrentLedgerState ledgerInterface
    let st' = tickLedgerState cfg TxsForUnknownBlock st
    isVar <- newTVarM $ initInternalState zeroTicketNo st'
    return MempoolEnv
      { mpEnvLedger    = ledgerInterface
      , mpEnvLedgerCfg = cfg
      , mpEnvCapacity  = capacity
      , mpEnvStateVar  = isVar
      , mpEnvTracer    = tracer
      }

-- | Spawn a thread which syncs the 'Mempool' state whenever the 'LedgerState'
-- changes.
forkSyncStateOnTipPointChange :: forall m blk. (
                                   IOLike m
                                 , ApplyTx blk
                                 , ValidateEnvelope blk
                                 )
                              => ResourceRegistry m
                              -> MempoolEnv m blk
                              -> m ()
forkSyncStateOnTipPointChange registry menv =
    void $ onEachChange registry id Nothing getCurrentTip action
  where
    action :: Point blk -> m ()
    action _tipPoint = void $ implSyncWithLedger menv

    -- Using the tip ('Point') allows for quicker equality checks
    getCurrentTip :: STM m (Point blk)
    getCurrentTip = ledgerTipPoint <$> getCurrentLedgerState (mpEnvLedger menv)

{-------------------------------------------------------------------------------
  Mempool Implementation
-------------------------------------------------------------------------------}

-- | Add a bunch of transactions (oldest to newest)
--
-- This function returns two lists: the transactions that were added or
-- rejected, and the transactions that could not yet be added, because the
-- Mempool capacity was reached. See 'addTxs' for a function that blocks in
-- case the Mempool capacity is reached.
--
-- Transactions are added one by one, updating the Mempool each time one was
-- added successfully.
--
-- This function does not sync the Mempool contents with the ledger state in
-- case the latter changes, it relies on the background thread to do that.
--
-- POSTCONDITON:
-- > (processed, toProcess) <- implTryAddTxs mpEnv txs
-- > map fst processed ++ toProcess == txs
implTryAddTxs
  :: forall m blk. (IOLike m, ApplyTx blk)
  => MempoolEnv m blk
  -> [GenTx blk]
  -> m ( [(GenTx blk, Maybe (ApplyTxErr blk))]
         -- Transactions that were added or rejected. A prefix of the input
         -- list.
       , [GenTx blk]
         -- Transactions that have not yet been added because the capacity
         -- of the Mempool has been reached. A suffix of the input list.
       )
implTryAddTxs mpEnv = go []
  where
    MempoolEnv
      { mpEnvStateVar
      , mpEnvLedgerCfg = cfg
      , mpEnvCapacity  = MempoolCapacityBytes capacity
      , mpEnvTracer
      } = mpEnv

    done acc toAdd = return (reverse acc, toAdd)

    go acc []                     = done acc []
    go acc toAdd@(firstTx:toAdd') = do
      let firstTxSize = txSize firstTx
      -- Note: we execute the continuation returned by 'atomically'
      join $ atomically $ do
        is <- readTVar mpEnvStateVar
        let curSize = msNumBytes $ isMempoolSize is
        if curSize + firstTxSize > capacity then
          -- No space in the Mempool
          return $ done acc toAdd
        else do
          let vr  = extendVRNew cfg firstTx $ validationResultFromIS is
              is' = internalStateFromVR vr
          unless (null (vrNewValid vr)) $
            -- Each time we have found a valid transaction, we update the
            -- Mempool. This keeps our STM transactions short, avoiding
            -- repeated work.
            --
            -- Note that even if the transaction were invalid, we could still
            -- write the state, because in that case we would have that @is ==
            -- is'@, but there's no reason to do that additional write.
            writeTVar mpEnvStateVar is'

          -- We only extended the ValidationResult with a single transaction
          -- ('firstTx'). So if it's not in 'vrInvalid', it must be in
          -- 'vrNewValid'.
          return $ case listToMaybe (vrInvalid vr) of
            -- The transaction was valid
            Nothing ->
              assert (isJust (vrNewValid vr)) $ do
                traceWith mpEnvTracer $ TraceMempoolAddedTx
                  firstTx
                  (isMempoolSize is)
                  (isMempoolSize is')
                go ((firstTx, Nothing):acc) toAdd'
            Just (_, err) ->
              assert (isNothing (vrNewValid vr))  $
              assert (length (vrInvalid vr) == 1) $ do
                traceWith mpEnvTracer $ TraceMempoolRejectedTx
                  firstTx
                  err
                  (isMempoolSize is)
                go ((firstTx, Just err):acc) toAdd'

implRemoveTxs
  :: (IOLike m, ApplyTx blk, HasTxId (GenTx blk), ValidateEnvelope blk)
  => MempoolEnv m blk
  -> [GenTxId blk]
  -> m ()
implRemoveTxs mpEnv txIds = do
    (removed, mempoolSize) <- atomically $ do
      IS { isTxs, isLastTicketNo } <- readTVar mpEnvStateVar
      st <- getCurrentLedgerState mpEnvLedger
      -- Filtering is O(n), but this function will rarely be used, as it is an
      -- escape hatch when there's an inconsistency between the ledger and the
      -- mempool.
      let txTickets' = filter
              ((`notElem` toRemove) . txId . txTicketTx)
              (TxSeq.toList isTxs)
          vr = revalidateTxsFor cfg
            (tickLedgerState cfg TxsForUnknownBlock st)
            isLastTicketNo
            txTickets'
          is' = internalStateFromVR vr
      writeTVar mpEnvStateVar is'
      return (map fst (vrInvalid vr), isMempoolSize is')

    unless (null txIds) $
      traceWith mpEnvTracer $
        TraceMempoolManuallyRemovedTxs txIds removed mempoolSize
  where
    MempoolEnv
      { mpEnvLedgerCfg = cfg
      , mpEnvLedger
      , mpEnvTracer
      , mpEnvStateVar
      } = mpEnv

    toRemove = Set.fromList txIds

implSyncWithLedger :: (IOLike m, ApplyTx blk, ValidateEnvelope blk)
                   => MempoolEnv m blk -> m (MempoolSnapshot blk TicketNo)
implSyncWithLedger mpEnv@MempoolEnv{mpEnvTracer, mpEnvStateVar} = do
    (removed, mempoolSize, snapshot) <- atomically $ do
      vr <- validateIS mpEnv TxsForUnknownBlock
      writeTVar mpEnvStateVar (internalStateFromVR vr)
      -- The size of the mempool /after/ removing invalid transactions.
      mempoolSize <- getMempoolSize mpEnv
      snapshot    <- implGetSnapshot mpEnv
      return (map fst (vrInvalid vr), mempoolSize, snapshot)
    unless (null removed) $
      traceWith mpEnvTracer $ TraceMempoolRemoveTxs removed mempoolSize
    return snapshot

implGetSnapshot :: (IOLike m, ApplyTx blk)
                => MempoolEnv m blk
                -> STM m (MempoolSnapshot blk TicketNo)
implGetSnapshot MempoolEnv{mpEnvStateVar} =
    implSnapshotFromIS <$> readTVar mpEnvStateVar

implGetSnapshotFor :: forall m blk. (IOLike m, ApplyTx blk, ValidateEnvelope blk)
                   => MempoolEnv m blk
                   -> BlockSlot
                   -> LedgerState blk
                   -> STM m (MempoolSnapshot blk TicketNo)
implGetSnapshotFor MempoolEnv{mpEnvStateVar, mpEnvLedgerCfg}
                   blockSlot ledger =
    updatedSnapshot <$> readTVar mpEnvStateVar
  where
    updatedSnapshot :: InternalState blk -> MempoolSnapshot blk TicketNo
    updatedSnapshot =
          implSnapshotFromIS
        . internalStateFromVR
        . validateStateFor mpEnvLedgerCfg blockSlot ledger

-- | Return the current capacity of the mempool in bytes.
--
-- TODO #1498: As of right now, this could be a pure operation but, in
-- preparation for #1498 (after which the mempool capacity will be dynamic),
-- this lives in STM.
implGetCapacity :: IOLike m => MempoolEnv m blk -> STM m MempoolCapacityBytes
implGetCapacity = pure . mpEnvCapacity

-- | \( O(1) \). Return the number of transactions in the Mempool paired with
-- their total size in bytes.
getMempoolSize :: (IOLike m, ApplyTx blk)
               => MempoolEnv m blk
               -> STM m MempoolSize
getMempoolSize MempoolEnv{mpEnvStateVar} =
    isMempoolSize <$> readTVar mpEnvStateVar

{-------------------------------------------------------------------------------
  MempoolSnapshot Implementation
-------------------------------------------------------------------------------}

implSnapshotFromIS :: ApplyTx blk
                   => InternalState blk -> MempoolSnapshot blk TicketNo
implSnapshotFromIS is = MempoolSnapshot {
      snapshotTxs         = implSnapshotGetTxs         is
    , snapshotTxsAfter    = implSnapshotGetTxsAfter    is
    , snapshotTxsForSize  = implSnapshotGetTxsForSize  is
    , snapshotLookupTx    = implSnapshotGetTx          is
    , snapshotMempoolSize = implSnapshotGetMempoolSize is
    }

implSnapshotGetTxs :: InternalState blk
                   -> [(GenTx blk, TicketNo)]
implSnapshotGetTxs = (flip implSnapshotGetTxsAfter) zeroTicketNo

implSnapshotGetTxsAfter :: InternalState blk
                        -> TicketNo
                        -> [(GenTx blk, TicketNo)]
implSnapshotGetTxsAfter IS{isTxs} tn =
    TxSeq.toTuples $ snd $ TxSeq.splitAfterTicketNo isTxs tn

implSnapshotGetTxsForSize :: InternalState blk
                          -> Word32
                          -> [(GenTx blk, TicketNo)]
implSnapshotGetTxsForSize IS{isTxs} maxSize =
    TxSeq.toTuples $ fst $ TxSeq.splitAfterTxSize isTxs maxSize

implSnapshotGetTx :: InternalState blk
                  -> TicketNo
                  -> Maybe (GenTx blk)
implSnapshotGetTx IS{isTxs} tn = isTxs `TxSeq.lookupByTicketNo` tn

implSnapshotGetMempoolSize :: ApplyTx blk
                           => InternalState blk
                           -> MempoolSize
implSnapshotGetMempoolSize = TxSeq.toMempoolSize . isTxs

{-------------------------------------------------------------------------------
  Validation
-------------------------------------------------------------------------------}

data ValidationResult blk = ValidationResult {
    -- | The tip of the chain before applying these transactions
    vrBeforeTip    :: ChainHash blk

    -- | The (ticked) slot number before applying these transactions
  , vrBeforeSlotNo :: SlotNo

    -- | The transactions that were found to be valid (oldest to newest)
  , vrValid        :: TxSeq (GenTx blk)

    -- | A new transaction (not previously known) which was found to be valid.
    --
    -- n.b. This will only contain a valid transaction that was /newly/ added
    -- to the mempool (not a previously known valid transaction).
  , vrNewValid     :: Maybe (GenTx blk)

    -- | The state of the ledger after applying 'vrValid' against the ledger
    -- state identifeid by 'vrBeforeTip'.
  , vrAfter        :: TickedLedgerState blk

    -- | The transactions that were invalid, along with their errors
    --
    -- From oldest to newest.
  , vrInvalid      :: [(GenTx blk, ApplyTxErr blk)]

    -- | The mempool 'TicketNo' counter.
    --
    -- When validating new transactions, this should be incremented, starting
    -- from 'isLastTicketNo' of the 'InternalState'.
    -- When validating previously applied transactions, this field should not
    -- be affected.
  , vrLastTicketNo :: TicketNo
  }

-- | Construct internal state from 'ValidationResult'
--
-- Discards information about invalid and newly valid transactions
internalStateFromVR :: ValidationResult blk -> InternalState blk
internalStateFromVR vr = IS {
      isTxs          = vrValid
    , isLedgerState  = vrAfter
    , isTip          = vrBeforeTip
    , isSlotNo       = vrBeforeSlotNo
    , isLastTicketNo = vrLastTicketNo
    }
  where
    ValidationResult {
        vrBeforeTip
      , vrBeforeSlotNo
      , vrValid
      , vrAfter
      , vrLastTicketNo
      } = vr

-- | Construct a 'ValidationResult' from internal state.
validationResultFromIS :: InternalState blk -> ValidationResult blk
validationResultFromIS is = ValidationResult {
      vrBeforeTip    = isTip
    , vrBeforeSlotNo = isSlotNo
    , vrValid        = isTxs
    , vrNewValid     = Nothing
    , vrAfter        = isLedgerState
    , vrInvalid      = []
    , vrLastTicketNo = isLastTicketNo
    }
  where
    IS {
        isTxs
      , isLedgerState
      , isTip
      , isSlotNo
      , isLastTicketNo
      } = is

-- | Extend 'ValidationResult' with a previously validated transaction that
-- may or may not be valid in this ledger state
--
-- n.b. Even previously validated transactions may not be valid in a different
-- ledger state;  it is /still/ useful to indicate whether we have previously
-- validated this transaction because, if we have, we can utilize 'reapplyTx'
-- rather than 'applyTx' and, therefore, skip things like cryptographic
-- signatures.
extendVRPrevApplied :: ApplyTx blk
                    => LedgerConfig blk
                    -> TxTicket (GenTx blk)
                    -> ValidationResult blk
                    -> ValidationResult blk
extendVRPrevApplied cfg txTicket vr =
    case runExcept (reapplyTx cfg tx vrAfter) of
      Left err  -> vr { vrInvalid = (tx, err) : vrInvalid
                      }
      Right st' -> vr { vrValid   = vrValid :> txTicket
                      , vrAfter   = st'
                      }
  where
    TxTicket { txTicketTx = tx } = txTicket
    ValidationResult { vrValid, vrAfter, vrInvalid } = vr

-- | Extend 'ValidationResult' with a new transaction (one which we have not
-- previously validated) that may or may not be valid in this ledger state.
--
-- PRECONDITION: 'vrNewValid' is 'Nothing'. In other words: new transactions
-- should be validated one-by-one, not by calling 'extendVRNew' on its result
-- again.
extendVRNew :: ApplyTx blk
            => LedgerConfig blk
            -> GenTx blk
            -> ValidationResult blk
            -> ValidationResult blk
extendVRNew cfg tx vr = assert (isNothing vrNewValid) $
    case runExcept (applyTx cfg tx vrAfter) of
      Left err  -> vr { vrInvalid      = (tx, err) : vrInvalid
                      }
      Right st' -> vr { vrValid        = vrValid :> TxTicket tx nextTicketNo (txSize tx)
                      , vrNewValid     = Just tx
                      , vrAfter        = st'
                      , vrLastTicketNo = nextTicketNo
                      }
  where
    ValidationResult {
        vrValid
      , vrAfter
      , vrInvalid
      , vrLastTicketNo
      , vrNewValid
      } = vr

    nextTicketNo = succ vrLastTicketNo

-- | Validate the internal state against the current ledger state and the
-- given 'BlockSlot', revalidating if necessary.
validateIS :: forall m blk. (IOLike m, ApplyTx blk, ValidateEnvelope blk)
           => MempoolEnv m blk
           -> BlockSlot
           -> STM m (ValidationResult blk)
validateIS MempoolEnv{mpEnvLedger, mpEnvLedgerCfg, mpEnvStateVar} blockSlot =
    validateStateFor mpEnvLedgerCfg blockSlot
      <$> getCurrentLedgerState mpEnvLedger
      <*> readTVar mpEnvStateVar

-- | Given a (valid) internal state, validate it against the given ledger
-- state and 'BlockSlot'.
--
-- When these match the internal state's 'isTip' and 'isSlotNo', this is very
-- cheap, as the given internal state will already be valid against the given
-- inputs.
--
-- When these don't match, the transaction in the internal state will be
-- revalidated ('revalidateTxsFor').
validateStateFor
  :: forall blk. (ApplyTx blk, ValidateEnvelope blk)
  => LedgerConfig     blk
  -> BlockSlot
  -> LedgerState      blk
  -> InternalState    blk
  -> ValidationResult blk
validateStateFor cfg blockSlot st is
    | isTip    == ledgerTipHash (tickedLedgerState st')
    , isSlotNo == tickedSlotNo st'
    = validationResultFromIS is
    | otherwise
    = revalidateTxsFor cfg st' isLastTicketNo (TxSeq.toList isTxs)
  where
    IS { isTxs, isTip, isSlotNo, isLastTicketNo } = is
    st' = tickLedgerState cfg blockSlot st

-- | Revalidate the given transactions (@['TxTicket' ('GenTx' blk)]@) against
-- the given ticked ledger state.
revalidateTxsFor
  :: forall blk. ApplyTx blk
  => LedgerConfig blk
  -> TickedLedgerState blk
  -> TicketNo
     -- ^ 'isLastTicketNo' & 'vrLastTicketNo'
  -> [TxTicket (GenTx blk)]
  -> ValidationResult blk
revalidateTxsFor cfg st lastTicketNo txTickets =
    repeatedly
      (extendVRPrevApplied cfg)
      txTickets
      (validationResultFromIS is)
  where
    is = initInternalState lastTicketNo st

-- | Tick the 'LedgerState' using the given 'BlockSlot'.
tickLedgerState
  :: forall blk. (UpdateLedger blk, ValidateEnvelope blk)
  => LedgerConfig blk
  -> BlockSlot
  -> LedgerState blk
  -> TickedLedgerState blk
tickLedgerState cfg blockSlot st = applyChainTick cfg slot st
  where
    -- If we don't yet know the slot number, optimistically assume that they
    -- will be included in a block in the next available slot
    slot :: SlotNo
    slot = case blockSlot of
      TxsForBlockInSlot s -> s
      TxsForUnknownBlock  ->
        -- TODO: We should use time here instead
        -- <https://github.com/input-output-hk/ouroboros-network/issues/1298>
        -- Once we do, the ValidateEnvelope constraint can go.
        case ledgerTipSlot st of
          Origin -> minimumPossibleSlotNo (Proxy @blk)
          At s   -> succ s
