{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Derive 'EpochInfo'
module Ouroboros.Consensus.HardFork.History.EpochInfo (
    dummyEpochInfo
  , snapshotEpochInfo
  , toPureEpochInfo
  ) where

import           Control.Exception (throw)
import           Control.Monad.Except (Except, runExcept, throwError)
import           GHC.Stack

import           Cardano.Slotting.EpochInfo.API

import           Ouroboros.Consensus.HardFork.History.Qry
import           Ouroboros.Consensus.HardFork.History.Summary

{-------------------------------------------------------------------------------
  Translation to EpochInfo
-------------------------------------------------------------------------------}

-- | Construct an 'EpochInfo' for a /snapshot/ of the ledger state
--
-- When a particular request fails with a 'PastHorizon' error, we throw the
-- error as a /pure/ exception. Such an exception would indicate a bug.
snapshotEpochInfo :: forall xs. Summary xs -> EpochInfo (Except PastHorizonException)
snapshotEpochInfo summary = EpochInfo {
      epochInfoSize_  = \e -> runQuery' (epochToSize  e)
    , epochInfoFirst_ = \e -> runQuery' (epochToSlot' e)
    , epochInfoEpoch_ = \s -> runQuery' (fst <$> slotToEpoch' s)

    , epochInfoSlotToRelativeTime_ = \s ->
        runQuery' (fst <$> slotToWallclock s)
    }
  where
    runQuery' :: HasCallStack => Qry a -> Except PastHorizonException a
    runQuery' q = either throwError pure $ runQuery q summary

-- | A dummy 'EpochInfo' that always throws an 'error'.
--
-- To be used as a placeholder before a summary is available.
dummyEpochInfo :: EpochInfo (Except PastHorizonException)
dummyEpochInfo = EpochInfo {
      epochInfoSize_               = \_ -> error "dummyEpochInfo used"
    , epochInfoFirst_              = \_ -> error "dummyEpochInfo used"
    , epochInfoEpoch_              = \_ -> error "dummyEpochInfo used"
    , epochInfoSlotToRelativeTime_ = \_ -> error "dummyEpochInfo used"
    }

-- | Interpret the 'PastHorizonException' as a _pure exception_ via 'throw'
--
-- As per usual, this should only be used when the pure exception would
-- indicate a bug.
toPureEpochInfo ::
     Applicative f
  => EpochInfo (Except PastHorizonException)
  -> EpochInfo f
toPureEpochInfo = hoistEpochInfo (either throw pure . runExcept)
