{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeOperators              #-}

-- | Intended for qualified import
--
-- > import qualified Ouroboros.Consensus.HardFork.History as HardFork
module Ouroboros.Consensus.HardFork.History (
    -- * History
    -- ** Params
    EraParams(..)
  , SafeZone(..)
  , SafeBeforeEpoch(..)
  , defaultEraParams
    -- ** Shape
  , Shape(..)
  , singletonShape
    -- ** Transitions
  , Transitions(..)
  , transitionsUnknown
    -- * Summary
  , Summary(..)    -- Non-opaque only for the benefit of tests
  , neverForksSummary
  , summarize
  , summaryBounds
  , summaryInit
  , summaryWithExactly
    -- ** Low-level API for summary construction
  , slotToEpochBound
  , maxMaybeEpoch
  , mkEraEnd
    -- * Queries
  , PastHorizonException(..)
  , Query -- Opaque
  , runQuery
  , runQueryThrow
  , runQueryPure
  , wallclockToSlot
  , slotToWallclock
  , slotToEpoch
  , epochToSlot
    -- * Support for 'EpochInfo'
  , summaryToEpochInfo
  , snapshotEpochInfo
    -- * Caching
  , RunWithCachedSummary(..)
  , cachedRunQueryThrow
  , runWithCachedSummary
    -- * Exported only for the benefit of tests
    -- ** Bounds
  , Bound -- Still opaque, even for tests. Has internal invariants.
  , boundEpoch
  , boundSlot
  , boundTime
  , initBound
  , mkUpperBound
    -- ** Summary
  , EraSummary(..)
  , EraEnd(..)
    -- ** Summary translations
  , ShiftTime(..)
    -- ** Invariants
  , invariantSummary
  , invariantShape
    -- ** Auxiliary
  , addEpochs
  , addSlots
  , countEpochs
  , countSlots
  ) where

import           Control.Exception (Exception (..), assert, throw)
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Bifunctor
import           Data.Fixed (divMod')
import           Data.Foldable (toList)
import           Data.Functor.Identity
import qualified Data.List as List
import           Data.Time
import           Data.Word
import           GHC.Generics (Generic)

import           Cardano.Prelude (NoUnexpectedThunks, UseIsNormalFormNamed (..))
import           Cardano.Slotting.EpochInfo.API
import           Cardano.Slotting.Slot

import           Ouroboros.Consensus.BlockchainTime.WallClock.Types
import           Ouroboros.Consensus.Config.SecurityParam
import           Ouroboros.Consensus.Util.Counting
import           Ouroboros.Consensus.Util.IOLike

{-------------------------------------------------------------------------------
  OVERVIEW

  The overall chain consists of various /era/s. Each era has its own set of era
  parameters such as the slot length and epoch size, as well as its own block
  format, ledger format, ledger rules, etc. It is assumed that the overall
  /shape/ of the chain is known. In other words, we statically know which eras
  we expect and what their parameters are; adding an additional era would
  require a code update. What we /don't/ know precisely is /when/ we transition
  from one era to the next, i.e., the hard fork transition points.

  When we are at genesis, the chain therefore looks something like this:

  > *-------------------?--------------------?--------------------
  > ^
  > \-- tip

  where we have (in this example) three eras (say, Byron, Shelley and Goguen)
  and therefore two hard fork transitions. Hard forks happen at epoch
  boundaries; the exact 'EpochNo' of each hard fork is determined by the era
  preceding it. Naturally, the exact 'EpochNo' of /past/ hard forks is known:

  > ---------------A--------------*----------?--------------------
  >                               ^
  >                               \-- tip

  where A is a known hard fork transition, and the next hard fork transition
  is still unknown.

  SAFE ZONES

  Future hard fork points may be known or unknown, where "known" means
  "certain"; i.e., for Byron, it would mean an update proposal has been voted
  on, confirmed, endorsed, and that endorsement is at least @k@ blocks deep into
  the chain; for Shelley it means an update proposal is voted on and accepted,
  and that acceptance is at least @k@ blocks deep into the chain.

  When a hard fork point is still unknown, we assume that each era determines a
  "safe zone": a number of slots from the tip of the ledger in which it is
  guaranteed that the hard fork will not happen.

  > CASE (i)
  >
  > ---------------A--------------*----------?--------------------
  >                               \..../
  >                                safe
  >                                zone

  Since the hard fork will not happen in the safe zone, we can extend the use of
  the current set of era parameters past the tip into the safe zone, giving us a
  limited ability to make predictions for the future (such as converting between
  slot numbers and wallclock time).

  We assume that once a transition point is known (and no longer subject to
  roll-back), this is guaranteed not to change anymore and we can use the era's
  parameters up to the transition point:

  > CASE (ii)
  >
  > ---------------A--------------*----------------B--------------
  >                               \.............../
  >                                implicitly safe

  Moreover, we assume that we can extend B's safe zone from the point of the
  hard fork transition:

  > CASE (iii)
  >
  > ---------------A--------------*----------------B--------------
  >                               \.............../\..../
  >                                implicitly safe  safe
  >                                                 zone

  This is justified because the safe zones arise from stability requirements
  for the transactions that establish the transition point. The earliest point
  such a transaction could be included in the chain is after the hard fork
  transition, since it must be a transaction from the /new/ era.

  NOTE ON STABILITY

  If we used as yet /unconfirmed/ update proposals to determine hard fork
  transition points, then any of the resulting time conversions would be
  subject to rollback; if we switched to a different fork, time conversions
  might suddenly look different. Whilst this /may/ be doable, in practice this
  is a headache we would very much like to avoid. For example, it might mean
  that when a block comes in and we determine that it's from the future,
  we might have prematurely marked it as invalid. So, we insist that time
  conversions must be based on update propsals that are /certain/ (no longer
  subject to rollback). This means that the "safe zone" we have been discussing
  above must extend from the point of stability forward. Moreover, the safe zone
  must be long enough to include a sufficient number of blocks such that we can
  evaluate enough headers of an alternative fork (without having its blocks)
  to decide that we want to switch to that fork; since in the worst case that
  means we have to evaluate @k@ headers (or @k+1@), the safe zone must be long
  enough to cover @k@ blocks (and therefore a safe zone of @2k@ slots for Byron,
  and (probably) a safe zone of @3k/f@ slots for Shelley). Effectively, this
  means that consensus wants "stability itself to be stable"; we need a double
  safe zone after an update proposal has been confirmed.
-------------------------------------------------------------------------------}

-- | Parameters that can vary across hard forks
data EraParams = EraParams {
      eraEpochSize  :: !EpochSize
    , eraSlotLength :: !SlotLength
    , eraSafeZone   :: !SafeZone
    }
  deriving stock    (Show, Eq, Generic)
  deriving anyclass (NoUnexpectedThunks)

-- | Default 'EraParams'
--
-- We set
--
-- * epoch size to @10k@ slots
-- * the safe zone to @2k@ slots
-- * the upper bound to 'UnsafeUnbounded'
--
-- This is primarily useful for tests.
defaultEraParams :: SecurityParam -> SlotLength -> EraParams
defaultEraParams (SecurityParam k) slotLength = EraParams {
      eraEpochSize  = EpochSize (k * 10)
    , eraSlotLength = slotLength
    , eraSafeZone   = SafeZone (k * 2) UnsafeUnbounded
    }

-- | Zone in which it is guaranteed that no hard fork can take place
data SafeZone = SafeZone {
      -- | Number of slots from the tip of the ledger
      --
      -- This should be (at least) the number of slots in which we are
      -- guaranteed to have @k@ blocks.
      safeFromTip     :: !Word64

      -- | Optionally, an 'EpochNo' before which no hard fork can take place
    , safeBeforeEpoch :: !SafeBeforeEpoch
    }
  deriving stock    (Show, Eq, Generic)
  deriving anyclass (NoUnexpectedThunks)

-- | Lower bound on when a transition can take place
data SafeBeforeEpoch =
    -- | No such lower bound is known
    NoLowerBound

    -- | 'EpochNo' before which a transition is guaranteed not to take place
    --
    -- Often such a value is available, since a new era is planned only after
    -- the current era has already been running for a while. For example, at
    -- the time of writing, we know the Byron to Shelley transition cannot
    -- happen before epoch 180, since we are currently already in epoch 179.
    --
    -- Moreover, for epoch transitions that have /already/ taken place, the
    -- exact 'EpochNo' of the transition can be used.
    --
    -- Providing this value is strictly an optimization; for example, it will
    -- reduce the frequency with which 'summaryToEpochInfo' must update its
    -- summary of the hard fork history.
  | LowerBound !EpochNo

    -- | Pretend the transition to the next era will not take place.
    --
    -- This constructor is marked as unsafe because it effectively extends
    -- the safe zone of this era indefinitely into the future. This means that
    -- we might reach invalid conclusions when doing
    --
    -- * slot to time conversions for blocks that are past the actual safe zone
    -- * time to slot conversions for the current time, when behind in syncing
    --
    -- This is safe when the code is simply not yet ready to transition to the
    -- next era, because in that case, we can be sure that blocks that come in
    -- are still from this era. It also means that we can always /produce/ a
    -- block, no matter how far ahead of the current ledger we are.
    --
    -- If the code is ready for the transition, just awaiting an update
    -- proposal, then 'LowerBound' can be used instead.
    --
    -- This constructor can be regarded as an " extreme " version of
    -- 'LowerBound', and can be used for similar reasons.
  | UnsafeUnbounded
  deriving stock    (Show, Eq, Generic)
  deriving anyclass (NoUnexpectedThunks)

-- | The shape of the chain (old to new)
--
-- The shape determines how many hard forks we expect as well as the parameters
-- for each era. The type argument is a type-level list containing one entry
-- per era, emphasizing that this information is statically known.
--
-- The indices are currently not yet used, but the idea is that they look
-- something like @'[Byron, Shelley, Goguen]@ and are then also used by the
-- hard fork combinator (most likely this will be a list of block types, since
-- most of consensus is indexed by block types).
newtype Shape xs = Shape (Exactly xs EraParams)
  deriving (Show)
  deriving NoUnexpectedThunks via UseIsNormalFormNamed "Shape" (Shape xs)

-- | There is only one era
singletonShape :: EraParams -> Shape '[x]
singletonShape params = Shape (exactlyOne params)

-- | The exact point of each confirmed hard fork transition (old to new)
--
-- Unlike the 'Shape' of the chain, which is statically known, the 'Transitions'
-- are derived from the state of the ledger (hard fork transition points only
-- become known after a voting procedure).
--
-- Any transition listed here must be "certain". How certainty is established is
-- ledger dependent, but it should imply that this is no longer subject to
-- rollback.
data Transitions :: [*] -> * where
  -- | If the indices are, say, @'[Byron, Shelley, Goguen]@, then we can have
  -- have at most two transitions: one to Shelley, and one to Goguen. There
  -- cannot be a transition /to/ the initial ledger.
  Transitions :: AtMost xs EpochNo -> Transitions (x ': xs)

deriving instance Show (Transitions xs)

-- | No known transitions yet
transitionsUnknown :: Transitions (x ': xs)
transitionsUnknown = Transitions AtMostNil

{-------------------------------------------------------------------------------
  Bounds
-------------------------------------------------------------------------------}

-- | Detailed information about the time bounds of an era
data Bound = Bound {
      boundTime  :: !UTCTime
    , boundSlot  :: !SlotNo
    , boundEpoch :: !EpochNo
    }
  deriving stock    (Show, Eq, Generic)
  deriving anyclass (NoUnexpectedThunks)

initBound :: SystemStart -> Bound
initBound (SystemStart start) = Bound {
      boundTime  = start
    , boundSlot  = SlotNo 0
    , boundEpoch = EpochNo 0
    }

-- | Version of 'mkUpperBound' when the upper bound may not be known
--
-- If passed 'Nothing', assumes 'EraUnbounded'. This is /NOT/
-- suitable for eras where the transition is simply unknown.
mkEraEnd :: EraParams
         -> Bound          -- ^ Lower bound
         -> Maybe EpochNo  -- ^ Upper bound
         -> EraEnd
mkEraEnd params lo = maybe EraUnbounded (EraEnd . mkUpperBound params lo)

-- | Compute upper bound given just the epoch number and era parameters
mkUpperBound :: EraParams
             -> Bound    -- ^ Lower bound
             -> EpochNo  -- ^ Upper bound
             -> Bound
mkUpperBound EraParams{..} lo hiEpoch = Bound {
      boundTime  = addUTCTime inEraTime  $ boundTime lo
    , boundSlot  = addSlots   inEraSlots $ boundSlot lo
    , boundEpoch = hiEpoch
    }
  where
    inEraEpochs, inEraSlots :: Word64
    inEraEpochs = countEpochs hiEpoch (boundEpoch lo)
    inEraSlots  = inEraEpochs * unEpochSize eraEpochSize

    inEraTime :: NominalDiffTime
    inEraTime = fromIntegral inEraSlots * getSlotLength eraSlotLength

{-------------------------------------------------------------------------------
  Internal: summary

  This is what we use internally for all translations.
-------------------------------------------------------------------------------}

-- | Information about a specific era
--
-- The 'eraEnd' of the final era in the summary will be determined by the
-- safe zone considerations discussed above.
data EraSummary = EraSummary {
      eraStart  :: !Bound     -- ^ Inclusive lower bound
    , eraEnd    :: !EraEnd    -- ^ Exclusive upper bound
    , eraParams :: !EraParams -- ^ Active parameters
    }
  deriving stock    (Show, Eq, Generic)
  deriving anyclass (NoUnexpectedThunks)

-- | Exclusive upper bound on the era
data EraEnd =
    -- | Bounded era
    EraEnd !Bound

    -- | Unbounded era
    --
    -- This arises from the use of 'UnsafeUnbounded'.
  | EraUnbounded
  deriving stock    (Show, Eq, Generic)
  deriving anyclass (NoUnexpectedThunks)

-- | Summary of the /confirmed/ part of the ledger
--
-- The summary zips 'Shape' with 'Forks', and provides detailed information
-- about the start and end of each era.

-- We have at most one summary for each era, and at least one
newtype Summary xs = Summary (NonEmpty xs EraSummary)
  deriving (Show)

-- WHNF is sufficient, because the counting types are all strict
deriving via UseIsNormalFormNamed "Summary" (Summary xs)
         instance NoUnexpectedThunks (Summary xs)

{-------------------------------------------------------------------------------
  Trivial summary
-------------------------------------------------------------------------------}

-- | 'Summary' for a ledger that never forks
neverForksSummary :: SystemStart -> EraParams -> Summary '[x]
neverForksSummary start params = Summary $ NonEmptyOne $ EraSummary {
      eraStart  = initBound start
    , eraEnd    = EraUnbounded
    , eraParams = params
    }

{-------------------------------------------------------------------------------
  Basic API for 'Summary'
-------------------------------------------------------------------------------}

-- | Outer bounds of the summary
summaryBounds :: Summary xs -> (Bound, EraEnd)
summaryBounds (Summary summary) =
    (eraStart (nonEmptyHead summary), eraEnd (nonEmptyLast summary))

-- | Analogue of 'Data.List.init' for 'Summary' (i.e., split off the final era)
--
-- This is primarily useful for tests.
summaryInit :: Summary xs -> (Maybe (Summary xs), EraSummary)
summaryInit (Summary summary) = first (fmap Summary) $ nonEmptyInit summary

-- | Construct 'Summary' with an exact number of 'EraSummary'
--
-- Primarily useful for tests.
summaryWithExactly :: Exactly (x ': xs) EraSummary -> Summary (x ': xs)
summaryWithExactly = Summary . exactlyWeakenNonEmpty

{-------------------------------------------------------------------------------
  Translations (primarily for the benefit of tests)
-------------------------------------------------------------------------------}

class ShiftTime a where
  shiftTime :: NominalDiffTime -> a -> a

instance ShiftTime SystemStart where
  shiftTime delta (SystemStart start) = SystemStart $ shiftTime delta start

instance ShiftTime a => ShiftTime [a] where
  shiftTime = map . shiftTime

instance ShiftTime (Summary xs) where
  shiftTime delta (Summary summary) = Summary $ shiftTime delta <$> summary

instance ShiftTime EraSummary where
  shiftTime delta EraSummary{..} = EraSummary{
      eraStart  = shiftTime delta eraStart
    , eraEnd    = shiftTime delta eraEnd
    , eraParams = eraParams
    }

instance ShiftTime EraEnd where
  shiftTime delta (EraEnd bound) = EraEnd (shiftTime delta bound)
  shiftTime _     EraUnbounded   = EraUnbounded

instance ShiftTime Bound where
  shiftTime delta Bound{..} = Bound {
      boundTime  = shiftTime delta boundTime
    , boundSlot  = boundSlot
    , boundEpoch = boundEpoch
    }

instance ShiftTime UTCTime where
  shiftTime = addUTCTime

{-------------------------------------------------------------------------------
  Invariants
-------------------------------------------------------------------------------}

-- | Check 'Shape' invariants
--
-- The only part of the 'Shape' that must make sense is the 'safeBeforeEpoch'
-- values (they must be strictly increasing).
--
-- NOTE: We assume eras cannot be empty. This will be satisfied by any ledger
-- we are interested in since transitions must be voted on (safe zones will
-- be non-empty).
invariantShape :: Shape xs -> Except String ()
invariantShape = \(Shape shape) ->
    go (EpochNo 0) shape
  where
    go :: EpochNo -- Lower bound on the start of the era
       -> Exactly xs EraParams -> Except String ()
    go _           ExactlyNil                    = return ()
    go lowerBound (ExactlyCons curParams shape') = do
        nextLowerBound <-
          case safeBeforeEpoch (eraSafeZone curParams) of
            NoLowerBound ->
              return $ addEpochs 1 lowerBound
            UnsafeUnbounded ->
              return $ addEpochs 1 lowerBound
            LowerBound e -> do
              unless (e > lowerBound) $
                throwError $ mconcat [
                    "Invalid safeBeforeEpoch in "
                  , show curParams
                  , " (should be greater than "
                  , show lowerBound
                  ,  ")"
                  ]
              return $ e

        go nextLowerBound shape'

-- | Check 'Summary' invariants
invariantSummary :: Summary xs -> Except String ()
invariantSummary = \(Summary summary) ->
    -- Pretend the start of the first era is the "end of the previous" one
    go (eraStart (nonEmptyHead summary)) (toList summary)
  where
    go :: Bound   -- ^ End of the previous era
       -> [EraSummary] -> Except String ()
    go _       []                  = return ()
    go prevEnd (curSummary : next) = do
        unless (curStart == prevEnd) $
          throwError $ mconcat [
              "Bounds don't line up: end of previous era "
            , show prevEnd
            , " /= start of current era "
            , show curStart
            ]

        case mCurEnd of
          EraUnbounded ->
            unless (null next) $
              throwError "Unbounded non-final era"
          EraEnd curEnd -> do
            let epochsInEra, slotsInEra :: Word64
                epochsInEra = countEpochs (boundEpoch curEnd) (boundEpoch curStart)
                slotsInEra  = epochsInEra * unEpochSize (eraEpochSize curParams)

                timeInEra :: NominalDiffTime
                timeInEra = fromIntegral slotsInEra
                          * getSlotLength (eraSlotLength curParams)

            unless (boundEpoch curEnd > boundEpoch curStart) $
              throwError "Empty era"

            case safeBeforeEpoch (eraSafeZone curParams) of
              NoLowerBound    -> return ()
              UnsafeUnbounded -> return ()
              LowerBound e    ->
                unless (boundEpoch curEnd >= e) $
                  throwError $ mconcat [
                      "Invalid upper epoch bound "
                    , show (boundEpoch curStart)
                    , " (should be greater than "
                    , show e
                    , ")"
                    ]

            unless (boundSlot curEnd == addSlots slotsInEra (boundSlot curStart)) $
              throwError $ mconcat [
                  "Invalid final boundSlot in "
                , show curSummary
                ]

            unless (boundTime curEnd == addUTCTime timeInEra (boundTime curStart)) $
              throwError $ mconcat [
                  "Invalid final boundTime in "
                , show curSummary
                ]

            go curEnd next
      where
        curStart  :: Bound
        mCurEnd   :: EraEnd
        curParams :: EraParams
        EraSummary curStart mCurEnd curParams = curSummary

{-------------------------------------------------------------------------------
  Constructing the summary
-------------------------------------------------------------------------------}

-- | Construct hard fork 'Summary'
--
-- NOTE (on epoch to slot translation). In order to translate 'SlotNo' to
-- 'EpochNo', we simply "line up" all slots. For example, suppose we have
-- an initial 'EpochSize' of 10, and then an 'EpochSize' of 20 from 'EpochNo'
-- 3 onwards. We end up with something like
--
-- > Epoch | 0      | 1        | 2        | 3        | 4        | ..
-- > Slot  | 0 .. 9 | 10 .. 19 | 20 .. 29 | 30 .. 49 | 50 .. 69 | ..
--
-- We do this translation /independent/ from the 'minimumPossibleSlotNo'
-- for a particular ledger. This means that for ledgers where the
-- 'minimumPossibleSlotNo' is not zero (e.g., some ledgers might set it to 1),
-- the maximum number of blocks (aka filled slots) in an epoch is just 1 (or
-- more) less than the other epochs.
summarize :: SystemStart
          -> WithOrigin SlotNo -- ^ Slot at the tip of the ledger
          -> Shape       xs
          -> Transitions xs
          -> Summary     xs
summarize systemStart ledgerTip = \(Shape shape) (Transitions transitions) ->
    Summary $ go (initBound systemStart) shape transitions
  where
    go :: Bound                          -- Lower bound for current era
       -> Exactly  (x ': xs) EraParams   -- params for all eras
       -> AtMost         xs  EpochNo     -- transitions
       -> NonEmpty (x ': xs) EraSummary
    -- CASE (ii)
    -- NOTE: Ledger tip might be close to the end of this era (or indeed past
    -- it) but this doesn't matter for the summary of /this/ era.
    go lo (ExactlyCons params ss) (AtMostCons epoch fs) =
        NonEmptyCons (EraSummary lo (EraEnd hi) params) $ go hi ss fs
      where
        hi = mkUpperBound params lo epoch
    -- CASE (i) or (iii)
    go lo (ExactlyCons params@EraParams{..} _) AtMostNil =
        NonEmptyOne (EraSummary lo hi params)
      where
        hi :: EraEnd
        hi = mkEraEnd params lo
           . maxMaybeEpoch (safeBeforeEpoch eraSafeZone)
           . slotToEpochBound params lo
           . addSlots (safeFromTip eraSafeZone)
             -- If the tip is already in this era, safe zone applies from the
             -- ledger tip (CASE (i)). If the ledger tip is in the /previous/
             -- era, but the transition to /this/ era is already known, the safe
             -- zone applies from the start of this era (CASE (iii)).
             --
             -- NOTE: The upper bound is /exclusive/:
             --
             -- o Suppose the ledger tip is at slot 10, and 'safeFromTip' is 2.
             --   Then we should be able to make accurate predictions for slots
             --   10 (of course), as well as (the safe zone) slots 11 and 12.
             --   Since the upper bound is /exclusive/, this means that the
             --   upper bound becomes 13. (Case i)
             -- o If the ledger tip is in the previous era (case iii), and the
             --   start of this era is slot 100, then we should be able to
             --   give accurate predictions for the first two slots in this era
             --   (100 and 101), and the upper bound becomes 102.
             --
             -- This explains the use of the extra addition ('next') for
             -- case (i) but not for case (iii).
           $ max (next ledgerTip) (boundSlot lo)

    -- Upper bound is exclusive, so we count from the /next/ ledger tip
    next :: WithOrigin SlotNo -> SlotNo
    next Origin = SlotNo 0
    next (At s) = succ s

-- Given the 'SlotNo' of the first /slot/ in which a transition could take
-- place, compute the first /epoch/ in which this could happen (since
-- transitions only take place at epoch boundaries). If the 'SlotNo' happens
-- to be the first slot in an epoch, it will be that 'EpochNo'; if it isn't,
-- however, it will be the /next/ epoch.
slotToEpochBound :: EraParams -> Bound -> SlotNo -> EpochNo
slotToEpochBound EraParams{eraEpochSize = EpochSize epochSize} lo hiSlot =
    addEpochs
      (if inEpoch == 0 then epochs else epochs + 1)
      (boundEpoch lo)
  where
    slots             = countSlots hiSlot (boundSlot lo)
    (epochs, inEpoch) = slots `divMod` epochSize

maxMaybeEpoch :: SafeBeforeEpoch -> EpochNo -> Maybe EpochNo
maxMaybeEpoch NoLowerBound    e = Just $ e
maxMaybeEpoch (LowerBound e') e = Just $ max e' e
maxMaybeEpoch UnsafeUnbounded _ = Nothing

{-------------------------------------------------------------------------------
  Internal: looking up values in the history
-------------------------------------------------------------------------------}

data Condition =
    ContainsTime  UTCTime
  | ContainsSlot  SlotNo
  | ContainsEpoch EpochNo
  deriving (Show)

eval :: Condition -> EraSummary -> Bool
eval condition EraSummary{..} =
    case condition of
      ContainsTime  x -> boundTime  `contains` x
      ContainsSlot  x -> boundSlot  `contains` x
      ContainsEpoch x -> boundEpoch `contains` x
  where
    contains :: Ord a => (Bound -> a) -> a -> Bool
    f `contains` x = case eraEnd of
                       EraEnd end   -> f eraStart <= x && x < f end
                       EraUnbounded -> f eraStart <= x

find :: Summary xs -> Condition -> Except PastHorizonException EraSummary
find summary@(Summary eras)  condition =
    case List.find (eval condition) (toList eras) of
      Nothing -> throwError $ PastHorizon (summaryBounds summary) condition
      Just s  -> return s

-- | We tried to convert something that is past the horizon
--
-- That is, we tried to convert something that is past the point in time
-- beyond which we lack information due to uncertainty about the next
-- hard fork.
--
-- We record the condition we were looking for and the bounds on the summary.
data PastHorizonException = PastHorizon (Bound, EraEnd) Condition

deriving instance Show PastHorizonException
instance Exception PastHorizonException

{-------------------------------------------------------------------------------
  Very thin layer for dealing with Queries
-------------------------------------------------------------------------------}

newtype Query (xs :: [*]) a = Query {
      unQuery :: ReaderT (Summary xs) (Except PastHorizonException) a
    }
  deriving newtype (
      Functor
    , Applicative
    , Monad
    , MonadReader (Summary xs)
    , MonadError PastHorizonException
    )

mkQuery :: (Summary xs -> Except PastHorizonException a) -> Query xs a
mkQuery = Query . ReaderT

runQuery :: Query xs a -> Summary xs -> Either PastHorizonException a
runQuery = (runExcept .) . runReaderT . unQuery

runQueryThrow :: MonadThrow m => Query xs a -> Summary xs -> m a
runQueryThrow q = either throwM return . runQuery q

runQueryPure :: Query xs a -> Summary xs -> a
runQueryPure q = either throw id . runQuery q

{-------------------------------------------------------------------------------
  Conversion between wallclock and slots
-------------------------------------------------------------------------------}

-- | Translate 'UTCTime' to 'SlotNo'
--
-- Additionally returns the time spent and time left in this slot.
wallclockToSlot :: UTCTime -> Query xs (SlotNo, NominalDiffTime, NominalDiffTime)
wallclockToSlot time = mkQuery $ \summary ->
    go <$> find summary (ContainsTime time)
  where
    go :: EraSummary -> (SlotNo, NominalDiffTime, NominalDiffTime)
    go EraSummary{..} =
        let (relSlot, timeSpent) = relTime `divMod'` slotLen
        in (toAbsSlot relSlot, timeSpent, slotLen - timeSpent)
      where
        EraParams{..} = eraParams

        toAbsSlot :: Word64 -> SlotNo
        toAbsSlot relSlot = addSlots relSlot (boundSlot eraStart)

        relTime, slotLen :: NominalDiffTime
        relTime = time `diffUTCTime` (boundTime eraStart)
        slotLen = getSlotLength eraSlotLength

-- | Translate 'SlotNo' to the 'UTCTime' at the start of that slot
--
-- Additionally returns the length of the slot.
slotToWallclock :: SlotNo -> Query xs (UTCTime, SlotLength)
slotToWallclock slot = mkQuery $ \summary ->
    go <$> find summary (ContainsSlot slot)
  where
    go :: EraSummary -> (UTCTime, SlotLength)
    go EraSummary{..} = (
          addUTCTime (fromIntegral relSlot * slotLen) (boundTime eraStart)
        , eraSlotLength
        )
      where
        EraParams{..} = eraParams

        relSlot :: Word64
        relSlot = countSlots slot (boundSlot eraStart)

        slotLen :: NominalDiffTime
        slotLen = getSlotLength eraSlotLength

{-------------------------------------------------------------------------------
  Conversion between slots and epochs
-------------------------------------------------------------------------------}

-- | Translate 'SlotNo' to its corresponding 'EpochNo'
--
-- Additionally returns the relative slot within this epoch
slotToEpoch :: SlotNo -> Query xs (EpochNo, Word64)
slotToEpoch slot = mkQuery $ \summary ->
    go <$> find summary (ContainsSlot slot)
  where
    go :: EraSummary -> (EpochNo, Word64)
    go EraSummary{..} = first toAbsEpoch $ relSlot `divMod` epochSize
      where
        EraParams{..} = eraParams

        toAbsEpoch :: Word64 -> EpochNo
        toAbsEpoch relEpoch = addEpochs relEpoch (boundEpoch eraStart)

        relSlot, epochSize :: Word64
        relSlot   = countSlots slot (boundSlot eraStart)
        epochSize = unEpochSize eraEpochSize

-- | Translate 'EpochNo' to the 'SlotNo' of the first slot in that epoch
--
-- Additionally returns the size of the epoch.
epochToSlot :: EpochNo -> Query xs (SlotNo, EpochSize)
epochToSlot epoch = mkQuery $ \summary ->
    go <$> find summary (ContainsEpoch epoch)
  where
    go :: EraSummary -> (SlotNo, EpochSize)
    go EraSummary{..} = (
          addSlots (relEpoch * epochSize) (boundSlot eraStart)
        , eraEpochSize
        )
      where
        EraParams{..} = eraParams

        relEpoch, epochSize :: Word64
        relEpoch  = countEpochs epoch (boundEpoch eraStart)
        epochSize = unEpochSize eraEpochSize

{-------------------------------------------------------------------------------
  Caching the summary
-------------------------------------------------------------------------------}

-- | Stateful abstraction to execute queries
data RunWithCachedSummary xs m = RunWithCachedSummary {
      -- | Run the specified query
      --
      -- If the query fails with a 'PastHorizonException', it will update its
      -- internal state (compute a new summary) and try again. If that /still/
      -- fails, the 'PastHorizonException' is returned.
      --
      -- See also 'cachedRunQueryThrow'.
      cachedRunQuery :: forall a. Query xs a
                     -> STM m (Either PastHorizonException a)
    }

-- | Wrapper around 'cachedRunQuery' which throws the 'PastHorizonException'
--
-- This is useful for callers who know that their queries should not be past
-- the horizon (and it would be a bug if they were).
cachedRunQueryThrow :: (MonadSTM m, MonadThrow (STM m))
                    => RunWithCachedSummary xs m -> Query xs a -> STM m a
cachedRunQueryThrow run qry = either throwM return =<< cachedRunQuery run qry

-- | Construct 'RunWithCachedSummary' given action that computes the summary
--
-- Most use cases will probably construct this action from an action that reads
-- the ledger state and then computes the summary from that.
runWithCachedSummary :: forall m xs. MonadSTM m
                     => STM m (Summary xs)
                     -> m (RunWithCachedSummary xs m)
runWithCachedSummary getSummary = do
    initSummary <- atomically getSummary
    var <- newTVarM initSummary
    return $ RunWithCachedSummary { cachedRunQuery = go var }
  where
    go :: StrictTVar m (Summary xs)
       -> Query xs a -> STM m (Either PastHorizonException a)
    go var q = do
        summary <- readTVar var
        case runQuery q summary of
          Right a             -> return (Right a)
          Left  PastHorizon{} -> do
            summary' <- getSummary
            writeTVar var summary'
            return $ runQuery q summary'

{-------------------------------------------------------------------------------
  Translation to EpochInfo
-------------------------------------------------------------------------------}

-- | Construct 'EpochInfo' from a function that returns the hard fork summary
--
-- When a particular request fails with a 'PastHorizon' error, we ask for an
-- updated summary, in the hope that the ledger state has advanced. If the query
-- /still/ fails with that updated summary, the error is thrown as an exception.
summaryToEpochInfo :: forall m xs. (MonadSTM m, MonadThrow (STM m))
                   => STM m (Summary xs) -> m (EpochInfo (STM m))
summaryToEpochInfo =
    fmap go . runWithCachedSummary
  where
    go :: RunWithCachedSummary xs m -> EpochInfo (STM m)
    go run = EpochInfo {
          epochInfoSize_  = \e -> cachedRunQueryThrow run (snd <$> epochToSlot e)
        , epochInfoFirst_ = \e -> cachedRunQueryThrow run (fst <$> epochToSlot e)
        , epochInfoEpoch_ = \s -> cachedRunQueryThrow run (fst <$> slotToEpoch s)
        }

-- | Construct an 'EpochInfo' for a /snapshot/ of the ledger state
--
-- When a particular request fails with a 'PastHorizon' error, we throw the
-- error as a /pure/ exception. Such an exception would indicate a bug.
snapshotEpochInfo :: forall xs. Summary xs -> EpochInfo Identity
snapshotEpochInfo summary = EpochInfo {
      epochInfoSize_  = \e -> runQueryPure' (snd <$> epochToSlot e)
    , epochInfoFirst_ = \e -> runQueryPure' (fst <$> epochToSlot e)
    , epochInfoEpoch_ = \s -> runQueryPure' (fst <$> slotToEpoch s)
    }
  where
    runQueryPure' :: Query xs a -> Identity a
    runQueryPure' = Identity . flip runQueryPure summary

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

addSlots  :: Word64 -> SlotNo -> SlotNo
addSlots  n (SlotNo  x) = SlotNo  (x + n)

addEpochs :: Word64 -> EpochNo -> EpochNo
addEpochs n (EpochNo x) = EpochNo (x + n)

-- | @countSlots to fr@ counts the slots from @fr@ to @to@ (@to >= fr@)
countSlots :: SlotNo -> SlotNo -> Word64
countSlots (SlotNo to) (SlotNo fr) = assert (to >= fr) $ to - fr

-- | @countEpochs to fr@ counts the epochs from @fr@ to @to@ (@to >= fr@)
countEpochs :: EpochNo -> EpochNo -> Word64
countEpochs (EpochNo to) (EpochNo fr) = assert (to >= fr) $ to - fr
