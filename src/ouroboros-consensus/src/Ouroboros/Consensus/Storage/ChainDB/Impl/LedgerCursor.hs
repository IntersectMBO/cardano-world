{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Consensus.Storage.ChainDB.Impl.LedgerCursor
  ( newLedgerCursor
  ) where

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Ledger.Extended (ExtLedgerState)
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Util.IOLike

import           Ouroboros.Consensus.Storage.ChainDB.API (LedgerCursor (..),
                     LedgerCursorFailure (..))
import           Ouroboros.Consensus.Storage.ChainDB.Impl.LgrDB (LgrDB)
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.LgrDB as LgrDB

newLedgerCursor
  :: forall m blk. (IOLike m, LedgerSupportsProtocol blk)
  => LgrDB m blk
  -> STM m (Point blk)
     -- ^ Get the immutable point
  -> m (LedgerCursor m blk)
newLedgerCursor lgrDB getImmutablePoint =
    toCursor =<< atomically (LgrDB.getCurrentState lgrDB)
  where
    toCursor :: ExtLedgerState blk -> m (LedgerCursor m blk)
    toCursor ledgerState = do
      varLedgerState <- newTVarM ledgerState
      return LedgerCursor
        { ledgerCursorState = atomically $ readTVar varLedgerState
        , ledgerCursorMove  = \pt -> do
            (mbPastLedgerState, immutablePoint) <-
              atomically $
                ((,) <$> LgrDB.getPastState lgrDB pt <*> getImmutablePoint)
            case mbPastLedgerState of
              Nothing      -> do
                -- Look at the immutable point, i.e., the point @k@ blocks
                -- back, to figure out why we couldn't get access to the
                -- requested ledger.
                return $ Left $ if pointSlot pt < pointSlot immutablePoint
                  then PointTooOld
                  else PointNotOnChain
              Just ledgerState' -> do
                atomically $ writeTVar varLedgerState ledgerState'
                return $ Right ledgerState'
        }
