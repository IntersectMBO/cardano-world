{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module Ouroboros.Storage.LedgerDB.InMemory (
     -- * LedgerDB proper
     LedgerDB
   , ledgerDbWithAnchor
   , ledgerDbFromGenesis
   , LedgerDbParams(..)
   , ledgerDbDefaultParams
     -- ** Queries
   , ChainSummary(..)
   , ledgerDbChainLength
   , ledgerDbToList
   , ledgerDbSnapshots
   , ledgerDbMaxRollback
   , ledgerDbCurrent
   , ledgerDbTip
   , ledgerDbIsSaturated
   , ledgerDbAnchor
     -- ** Result types
   , PushManyResult (..)
   , pushManyResultToEither
     -- ** Updates
   , Apply(..)
   , Err
   , RefOrVal(..)
   , ledgerDbPush
   , ledgerDbReapply
   , ledgerDbSwitch
     -- * Pure wrappers (primarily for property testing)
   , ledgerDbPush'
   , ledgerDbPushMany'
   , ledgerDbSwitch'
     -- * Serialisation
   , encodeChainSummary
   , decodeChainSummary
     -- * Demo
   , demo
     -- * Internal functions exposed for the benefit of tests only
   , ledgerDbCountToPrune
   ) where

import           Prelude hiding (mod, (/))
import qualified Prelude

import           Codec.Serialise (Serialise (..))
import           Codec.Serialise.Decoding (Decoder)
import qualified Codec.Serialise.Decoding as Dec
import           Codec.Serialise.Encoding (Encoding)
import qualified Codec.Serialise.Encoding as Enc
import           Control.Exception (Exception, throw)
import           Control.Monad ((>=>))
import           Control.Monad.Except (ExceptT (..), runExceptT)
import           Data.Foldable (toList)
import           Data.Functor.Identity
import           Data.Sequence (Seq ((:|>), Empty), (|>))
import qualified Data.Sequence as Seq
import           Data.Typeable (Typeable)
import           Data.Void
import           Data.Word
import           GHC.Generics (Generic)
import           GHC.Stack (CallStack, HasCallStack, callStack)

import           Ouroboros.Consensus.Protocol.Abstract (SecurityParam (..))
import           Ouroboros.Consensus.Util

import           Ouroboros.Storage.Common
import           Ouroboros.Storage.LedgerDB.Conf

{-------------------------------------------------------------------------------
  Ledger DB types
-------------------------------------------------------------------------------}

-- | Internal state of the ledger DB
--
-- The ledger DB looks like
--
-- > anchor |> blocks and snapshots <| current
--
-- where @anchor@ records the oldest known snapshot and @current@ the most
-- recent.
--
-- As an example, suppose we have @snapEvery = 4@ (i.e., we will take a
-- snapshot every 4 blocks) and @k = 6@. The ledger DB grows as illustrated
-- below, where we indicate the anchor, number of blocks, the stored
-- blocks/snapshots, and the current ledger; the oldest block we can roll back
-- to is marked.
--
-- > anchor |> #   [ blocks ]                                      <| tip
-- > ---------------------------------------------------------------------------
-- > *G     |> (0) [ ]                                             <| G
-- > *G     |> (1) [ B1]                                           <| L1
-- > *G     |> (2) [ B1,  B2]                                      <| L2
-- > *G     |> (3) [ B1,  B2,   B3]                                <| L3
-- > *G     |> (4) [ B1,  B2,   B3,  L4]                           <| L4
-- > *G     |> (5) [ B1,  B2,   B3,  L4,  B5]                      <| L5
-- > *G     |> (6) [ B1,  B2,   B3,  L4,  B5,  B6]                 <| L6
-- >  G     |> (7) [*B1,  B2,   B3,  L4,  B5,  B6,  B7]            <| L7
-- >  G     |> (8) [ B1, *B2,   B3,  L4,  B5,  B6,  B7,  L8]       <| L8
-- >  G     |> (9) [ B1,  B2,  *B3,  L4,  B5,  B6,  B7,  L8,  B9]  <| L9   (*)
-- > *L4    |> (6) [ B5,  B6,   B7,  L8,  B9,  B10]                <| L10  (**)
-- >  L4    |> (7) [*B5,  B6,   B7,  L8,  B9,  B10, B11]           <| L11
-- >  L4    |> (8) [ B5, *B6,   B7,  L8,  B9,  B10, B11, L12]      <| L12
-- >  L4    |> (9) [ B5,  B6,  *B7,  L8,  B9,  B10, B12, L12, B13] <| L13
-- > *L8    |> (6) [ B9,  B10,  B12, L12, B13, B14]                <| L14
--
-- The ledger DB must guarantee we must at all times be able to roll back @k@
-- blocks. For example, if we are on line (*), and roll back 6 blocks, we get
--
-- > G |> [B1, B2, B3]
--
-- from which we can /compute/ @G |> [B1, B2, B3] <| L3@ by re-applying
-- @snapEvery - 1@ blocks. However, as soon as we pushed one more block @(**)@,
-- and then roll back 6 blocks, we end up with
--
-- > L4 |> [] <| L4
--
-- This representation has the following properties:
--
-- * The distance between snapshots is at most @snapEvery@. This implies that
--   rolling back involves re-applying at most @snapEvery - 1@ blocks
--   (note that @snapEvery >= 1@).
--
-- * This implies that the number of (references to) blocks we store will vary
--   between @k@ and @k + snapEvery - 1@.
--
-- * When the number of blocks reaches @k + snapEvery@, we can drop the first
--   @snapEvery@ blocks, shifting up the anchor.
--
-- * The average number of blocks we have to reapply on a rollback is given by
--
--   >    average [0 .. snapEvery - 1]
--   > == ((snapEvery - 1) * snapEvery / 2) / snapEvery
--   > == (snapEvery - 1) / 2
--
--   (Obvious) boundary case: if @snapEvery == 1@, the average number is zero.
--
-- * The number of snapshots we store is in the range
--
--   > [1 + |_ k / snapEvery _|, 1 + |_ k / snapEvery _| + 1)
--
--   If @snapEvery@ divides @k@, the number is precisely @1 + k / snapEvery@.
--
-- * Picking a suitable value of @snapEvery@ for a given @k@ is a tradeoff
--   between cost of reapplying blocks and the cost of storing more snapshots.
--   The latter is primarily the cost of less opportunity for garbage
--   collection, which is hard to quantify abstractly. This should probably
--   be determined emperically.
--
-- Some example boundary cases:
--
-- * Suppose @k = 4@ and @snapEvery = 1@:
--
--   > *G  |> []               <| G
--   > *G  |> [L1]             <| L1
--   > *G  |> [L1, L2]         <| L2
--   > *G  |> [L1, L2, L3]     <| L3
--   > *G  |> [L1, L2, L3, L4] <| L4
--   > *L1 |> [L2, L3, L4, L5] <| L5
--   > *L2 |> [L3, L4, L5, L6] <| L6
--
--   Note that in this case the number of blocks will always be @k@ (unless we
--   are close to genesis), and the anchor is always the oldest point we can
--   roll back to.
--
-- * If @k = 1@ and @snapEvery = 4@, we get
--
--   > *G  |> [ ]                  <| G
--   > *G  |> [ B1]                <| L1
--   >  G  |> [*B1,  B2]           <| L2
--   >  G  |> [ B1, *B2,  B3]      <| L3
--   >  G  |> [ B1,  B2, *B3, L4]  <| L4
--   > *L4 |> [ B5]                <| L5
--   >  L4 |> [*B5,  B6]           <| L6
--   >  L4 |> [ B5, *B6,  B7]      <| L7
--   >  L4 |> [ B5,  B6, *B7, L8]  <| L8
--   > *L8 |> [*B9]                <| L9
--
--   (The minimum distance between the current ledger and the maximum rollback
--   is @k@.)
--
-- * If @k = 0@ and @snapEvery = 4@, we get
--
--   > *G  |> [ ]                  <| G
--   >  G  |> [*B1]                <| L1
--   >  G  |> [ B1, *B2]           <| L2
--   >  G  |> [ B1,  B2, *B3]      <| L3
--   > *L4 |> [ ]                  <| L4
--   >  L4 |> [*B5]                <| L5
--   >  L4 |> [ B5, *B6]           <| L6
--   >  L4 |> [ B5,  B6, *B7]      <| L7
--   > *L8 |> [ ]                  <| L8
--
--   @k = 0@ is the only case where the list might be empty. Of course, this is
--   not a particularly useful configuration.
--
-- * If @k = 1@ and @snapEvery = 1@, we get
--
--   > *G  |> [ ]   <| G
--   > *G  |> [ L1] <| L1
--   > *L1 |> [ L2] <| L2
--   > *L2 |> [ L3] <| L3
--
-- * If @k = 0@ and @snapEvery = 1@, we get
--
--   > *G  |> [ ] <| G
--   > *L1 |> [ ] <| L1
--   > *L2 |> [ ] <| L2
--
-- * Finally, if @k = snapEvery = k4@, we get
--
--   > *G  |> [ ]                             <| G
--   > *G  |> [ B1]                           <| L1
--   > *G  |> [ B1,  B2]                      <| L2
--   > *G  |> [ B1,  B2,  B3]                 <| L3
--   > *G  |> [ B1,  B2,  B3, L4]             <| L4
--   >  G  |> [*B1,  B2,  B3, L4, B5]         <| L5
--   >  G  |> [ B1, *B2,  B3, L4, B5, B6]     <| L6
--   >  G  |> [ B1,  B2, *B3, L4, B5, B6, B7] <| L7
--   > *L4 |> [ B5,  B6,  B7, L8]             <| L8
--   >  L4 |> [*B5,  B6,  B7, L8, B9]         <| L9
data LedgerDB l r = LedgerDB {
      -- | The ledger state at the tip of the chain
      ledgerDbCurrent :: !l

      -- | Older ledger states
    , ledgerDbBlocks  :: !(Seq (r, Maybe l))

      -- | Information about the state of the ledger /before/
    , ledgerDbAnchor  :: !(ChainSummary l r)

      -- | Ledger DB parameters
    , ledgerDbParams  :: !LedgerDbParams
    }
  deriving (Show, Eq)

data LedgerDbParams = LedgerDbParams {
      -- | Take a snapshot every @n@ blocks
      --
      -- Must be @>= 1@.
      ledgerDbSnapEvery     :: !Word64

      -- | Security parameter (maximum rollback)
    , ledgerDbSecurityParam :: !SecurityParam
    }
  deriving (Show, Eq, Generic)

-- | Default parameters
--
-- TODO: We should decide emperically what a good @snapEvery@ value is.
ledgerDbDefaultParams :: SecurityParam -> LedgerDbParams
ledgerDbDefaultParams (SecurityParam k) = LedgerDbParams {
      ledgerDbSnapEvery     = 100
    , ledgerDbSecurityParam = SecurityParam k
    }

{-------------------------------------------------------------------------------
  Block info
-------------------------------------------------------------------------------}

-- | Has a block previously been applied?
--
-- In addition to the term level value, we also reflect at the type level
-- whether the block has been previously applied. This allows us to be more
-- precise when we apply blocks: re-applying previously applied blocks cannot
-- result in ledger errors.
data Apply :: Bool -> * where
    -- | This block was previously applied
    --
    -- If we reapply a previously applied block, we /must/ reapply it to the
    -- very same state. This means that we cannot have any errors.
    --
    -- This constructor is polymorphic: this allows us to be conservative at
    -- the type level ("this may result in a ledger error") whilst still marking
    -- a block as previously applied ("don't bother checking for errors"). This
    -- is useful for example when we have a bunch of blocks, some of which have
    -- been previously applied and some of which have not. Applying all of them
    -- could definitely result in a ledger error, even if we skip checks for
    -- some of them.
    Reapply :: Apply pa

    -- | Not previously applied
    --
    -- All checks must be performed
    Apply :: Apply 'False

-- | Error we get from applying a block
--
-- If the block was previously applied, we can't get any errors.
type family Err (ap :: Bool) (e :: *) :: * where
  Err 'True  e = Void
  Err 'False e = e

-- | Pass a block by value or by reference
data RefOrVal r b = Ref r | Val r b

ref :: RefOrVal r b -> r
ref (Ref r  ) = r
ref (Val r _) = r

{-------------------------------------------------------------------------------
  Chain summary
-------------------------------------------------------------------------------}

-- | Summary of the chain at a particular point in time
data ChainSummary l r = ChainSummary {
      -- | The tip of the chain
      csTip    :: !(Tip r)

      -- | Length of the chain
    , csLength :: !Word64

      -- | Ledger state
    , csLedger :: !l
    }
  deriving (Show, Eq, Generic)

genesisChainSummary :: l -> ChainSummary l r
genesisChainSummary l = ChainSummary TipGen 0 l

{-------------------------------------------------------------------------------
  LedgerDB proper
-------------------------------------------------------------------------------}

-- | Ledger DB starting at the specified ledger state
ledgerDbWithAnchor :: LedgerDbParams -> ChainSummary l r -> LedgerDB l r
ledgerDbWithAnchor params anchor = LedgerDB {
      ledgerDbCurrent = csLedger anchor
    , ledgerDbBlocks  = Seq.empty
    , ledgerDbAnchor  = anchor
    , ledgerDbParams  = params
    }

ledgerDbFromGenesis :: LedgerDbParams -> l -> LedgerDB l r
ledgerDbFromGenesis params = ledgerDbWithAnchor params . genesisChainSummary

{-------------------------------------------------------------------------------
  Internal: derived functions in terms of 'BlockInfo'
-------------------------------------------------------------------------------}

-- | Apply block to the given ledger state
--
-- Forces the new ledger to WHNF.
applyBlock :: forall m l r b e ap. Monad m
           => LedgerDbConf m l r b e
           -> (Apply ap, RefOrVal r b)
           -> l -> ExceptT (Err ap e) m l
applyBlock LedgerDbConf{..} (pa, rb) l = ExceptT $
    case rb of
      Ref  r   -> apply pa <$> ldbConfResolve r
      Val _r b -> return $ apply pa b
  where
    -- Tie evaluation of the 'Either' to evaluation of the ledger state
    apply :: Apply ap -> b -> Either (Err ap e) l
    apply Reapply b = Right $! ldbConfReapply b l
    apply Apply   b = case ldbConfApply b l of
                        Left err -> Left err
                        Right l' -> Right $! l'

-- | Reapply previously applied block
reapplyBlock :: forall m l r b e. Monad m
             => LedgerDbConf m l r b e -> RefOrVal r b -> l -> m l
reapplyBlock cfg b = fmap mustBeRight
                   . runExceptT
                   . applyBlock cfg (Reapply @'True, b)

{-------------------------------------------------------------------------------
  Queries
-------------------------------------------------------------------------------}

-- | Total length of the chain (in terms of number of blocks)
ledgerDbChainLength :: LedgerDB l r -> Word64
ledgerDbChainLength LedgerDB{..} =
    csLength ledgerDbAnchor + fromIntegral (Seq.length ledgerDbBlocks)

-- | References to blocks and corresponding ledger state (from old to new)
ledgerDbToList :: LedgerDB l r -> [(r, Maybe l)]
ledgerDbToList LedgerDB{..} = toList ledgerDbBlocks

-- | All snapshots currently stored by the ledger DB (new to old)
--
-- For each snapshot we also return the distance from the tip
ledgerDbSnapshots :: forall l r. LedgerDB l r -> [(Word64, l)]
ledgerDbSnapshots LedgerDB{..} = go 0 ledgerDbBlocks
  where
    go :: Word64 -> Seq (r, Maybe l) -> [(Word64, l)]
    go offset Empty                 = [(offset, csLedger ledgerDbAnchor)]
    go offset (ss :|> (_, Nothing)) =               go (offset + 1) ss
    go offset (ss :|> (_, Just l))  = (offset, l) : go (offset + 1) ss

-- | How many blocks can we currently roll back?
ledgerDbMaxRollback :: LedgerDB l r -> Word64
ledgerDbMaxRollback LedgerDB{..} = fromIntegral (Seq.length ledgerDbBlocks)

-- | Reference to the block at the tip of the chain
ledgerDbTip :: LedgerDB l r -> Tip r
ledgerDbTip LedgerDB{..} =
    case ledgerDbBlocks of
      Empty        -> csTip ledgerDbAnchor
      _ :|> (r, _) -> Tip r

-- | Have we seen at least @k@ blocks?
ledgerDbIsSaturated :: LedgerDB l r -> Bool
ledgerDbIsSaturated LedgerDB{..} =
    fromIntegral (Seq.length ledgerDbBlocks) >= k
  where
    LedgerDbParams{..} = ledgerDbParams
    SecurityParam k    = ledgerDbSecurityParam

{-------------------------------------------------------------------------------
  Result types
-------------------------------------------------------------------------------}

-- | The result of 'ledgerDbPushMany', i.e. when validating a number of new
-- blocks.
--
-- About the boolean type index: if all blocks given to 'ledgerDbPushMany' are
-- 'Reapply', and so @ap ~ 'True@, then the result /must/ be 'ValidBlocks'.
data PushManyResult e l r :: Bool -> * where
  -- | A block is invalid. We return the 'LedgerDB' corresponding to the block
  -- before it. We also return the validation error and include a reference to
  -- the invalid block.
  InvalidBlock :: e -> r -> LedgerDB l r -> PushManyResult e l r 'False

  -- | All blocks were valid, a 'LedgerDB' corresponding to the last valid
  -- block is returned.
  ValidBlocks ::            LedgerDB l r -> PushManyResult e l r ap

pushManyResultToEither :: PushManyResult e l r ap
                       -> Either (Err ap e) (LedgerDB l r)
pushManyResultToEither = \case
    InvalidBlock e _ _ -> Left e
    ValidBlocks      l -> Right l

{-------------------------------------------------------------------------------
  Internal updates
-------------------------------------------------------------------------------}

-- | Internal: shift the anchor given a bunch of blocks
--
-- PRE: The last block in the sequence /must/ contain a ledger snapshot.
shiftAnchor :: forall r l. HasCallStack
            => Seq (r, Maybe l) -> ChainSummary l r -> ChainSummary l r
shiftAnchor toRemove ChainSummary{..} = ChainSummary {
      csTip    = Tip csTip'
    , csLength = csLength + fromIntegral (Seq.length toRemove)
    , csLedger = csLedger'
    }
  where
    csTip'    :: r
    csLedger' :: l
    (csTip', csLedger') =
        case toRemove of
          Empty              -> error "shiftAnchor: empty list"
          _ :|> (_, Nothing) -> error "shiftAnchor: missing ledger"
          _ :|> (r, Just l)  -> (r, l)

-- | Internal: count number of blocks to prune, given total number of blocks
--
-- This is exposed for the benefit of tests only.
ledgerDbCountToPrune :: HasCallStack => LedgerDbParams -> Int -> Int
ledgerDbCountToPrune LedgerDbParams{..} curSize'
  | curSize <= maxSize = 0
  | otherwise          = fromIntegral $ numToRemove
  where
    SecurityParam k = ledgerDbSecurityParam

    -- Current, minimum and maximum number of blocks we need
    curSize, minSize, maxSize :: Word64
    curSize = fromIntegral curSize'
    minSize = k
    maxSize = k + ledgerDbSnapEvery - 1

    -- Number of blocks to remove (assuming curSize > maxSize)
    --
    -- Notes:
    --
    -- * If @curSize > maxSize@, then @curSize >= ledgerDbSnapEvery@
    -- * This means we will at least remove 'ledgerDbSnapEvery' blocks
    -- * We will remove an even multiple of 'ledgerDbSnapEvery' blocks
    -- * This means that the last block we remove must contain a snapshot
    numToRemove :: Word64
    numToRemove = nearestMultiple ledgerDbSnapEvery (curSize - minSize)

    -- Nearest multiple of n, not exceeding x
    --
    -- > nearestMultiple 4 3 == 0
    -- > nearestMultiple 4 4 == 4
    -- > nearestMultiple 4 5 == 4
    -- > ..
    -- > nearestMultiple 4 7 == 4
    -- > nearestMultiple 4 8 == 8
    -- > nearestMultiple 4 9 == 8
    nearestMultiple :: Integral a => a -> a -> a
    nearestMultiple n x = floor (x' `safeDiv` n') * n
      where
        n', x' :: Double
        n' = fromIntegral n
        x' = fromIntegral x

-- | Internal: drop unneeded blocks from the head of the list
--
-- Postcondition: number blocks is between k and k + snapEvery - 1
prune :: HasCallStack => LedgerDB l r -> LedgerDB l r
prune db@LedgerDB{..} =
    if toPrune == 0
      then db
      else let (removed, kept) = Seq.splitAt toPrune ledgerDbBlocks
               anchor'         = shiftAnchor removed ledgerDbAnchor
           in db { ledgerDbAnchor = anchor'
                 , ledgerDbBlocks = kept
                 }
  where
    -- Number of blocks to remove (assuming curSize > maxSize)
    toPrune :: Int
    toPrune = ledgerDbCountToPrune ledgerDbParams (Seq.length ledgerDbBlocks)

-- | Rollback
--
-- Precondition: @n <= ledgerDbMaxRollback db@
rollback :: forall m l r b e. (
                Monad m
              , HasCallStack
              , Typeable l
              , Typeable r
              , Show l
              , Show r
              )
         => LedgerDbConf m l r b e
         -> Word64 -> LedgerDB l r -> m (LedgerDB l r)
rollback cfg n db@LedgerDB{..} = do
    current' <- uncurry computeCurrent $ reapply blocks'
    return db {
        ledgerDbCurrent = current'
      , ledgerDbBlocks  = blocks'
      }
  where
    numToKeep = Seq.length ledgerDbBlocks - fromIntegral n
    blocks'   = if numToKeep >= 0
                  then Seq.take numToKeep ledgerDbBlocks
                  else throw $ InvalidRollback {
                           invalidRollbackCount = n
                         , invalidRollbackDb    = db
                         , invalidRollbackErr   = "Rollback too far"
                         , invalidRollbackStack = callStack
                         }

    -- Compute blocks to reapply, and ledger state to reapply them from
    reapply :: Seq (r, Maybe l) -> ([r], l)
    reapply = go []
      where
        go :: [r] -> Seq (r, Maybe l) -> ([r], l)
        go acc Empty                 = (acc, csLedger ledgerDbAnchor)
        go acc (_  :|> (_, Just l))  = (acc, l)
        go acc (ss :|> (r, Nothing)) = go (r:acc) ss

    computeCurrent :: [r] -> l -> m l
    computeCurrent []     = return
    computeCurrent (r:rs) = reapplyBlock cfg (Ref r) >=> computeCurrent rs

-- | Invalid rollback exception
--
-- If this is ever thrown it indicates a bug in the code
data InvalidRollbackException l r =
    InvalidRollback {
        -- | Number of blocks requested to rollback
        invalidRollbackCount :: Word64

        -- | The ledger DB at the time of the rollback
      , invalidRollbackDb    :: LedgerDB l r

        -- | Error message
      , invalidRollbackErr   :: String

        -- | Callstack
      , invalidRollbackStack :: CallStack
      }
  deriving (Show)

instance (Typeable l, Typeable r, Show r, Show l)
      => Exception (InvalidRollbackException l r)

{-------------------------------------------------------------------------------
  Updates
-------------------------------------------------------------------------------}

-- | Push a block
--
-- @O(1)@. Computes the new ledger state, and prunes the ledger DB if needed.
ledgerDbPush :: forall m l r b e ap. Monad m
             => LedgerDbConf m l r b e
             -> (Apply ap, RefOrVal r b)
             -> LedgerDB l r
             -> m (Either (Err ap e) (LedgerDB l r))
ledgerDbPush cfg (pa, new) db@LedgerDB{..} = runExceptT $ do
    !current' <- applyBlock cfg (pa, new) ledgerDbCurrent
    let newPos  = fromIntegral (Seq.length ledgerDbBlocks) + 1
        blocks' = ledgerDbBlocks
               |> if newPos `safeMod` ledgerDbSnapEvery == 0
                    then (ref new, Just current')
                    else (ref new, Nothing)
    return $ prune (db {
        ledgerDbCurrent = current'
      , ledgerDbBlocks  = blocks'
      })
  where
    LedgerDbParams{..} = ledgerDbParams

ledgerDbReapply :: Monad m
                => LedgerDbConf m l r b e
                -> RefOrVal r b -> LedgerDB l r -> m (LedgerDB l r)
ledgerDbReapply cfg b = fmap mustBeRight . ledgerDbPush cfg (Reapply @'True, b)

-- | Push a bunch of blocks (oldest first)
ledgerDbPushMany :: forall ap m l r b e. Monad m
                 => LedgerDbConf m l r b e
                 -> [(Apply ap, RefOrVal r b)]
                 -> LedgerDB l r
                 -> m (PushManyResult e l r ap)
ledgerDbPushMany cfg = flip go
  where
    go :: LedgerDB l r
       -> [(Apply ap, RefOrVal r b)]
       -> m (PushManyResult e l r ap)
    go l = \case
      [] -> return $ ValidBlocks l
      b@(ap, rov):bs -> case ap of
        Apply   -> ledgerDbPush cfg b l >>= \case
          Left  e  -> return $ InvalidBlock e (ref rov) l
          Right l' -> go l' bs
        -- We have Reapply @'False, so 'ledgerDbPush' returns a non-Void
        -- error, but we know it can't happen, so just use Reapply @'True.
        Reapply -> ledgerDbPush cfg (Reapply @'True, rov) l >>= \case
          Left  e  -> absurd e
          Right l' -> go l' bs

-- | Switch to a fork
ledgerDbSwitch :: ( Monad m
                  , HasCallStack
                  , Show l
                  , Show r
                  , Typeable l
                  , Typeable r
                  )
               => LedgerDbConf m l r b e
               -> Word64                      -- ^ How many blocks to roll back
               -> [(Apply ap, RefOrVal r b)]  -- ^ New blocks to apply
               -> LedgerDB l r
               -> m (PushManyResult e l r ap)
ledgerDbSwitch cfg numRollbacks newBlocks =
        rollback cfg numRollbacks
    >=> ledgerDbPushMany cfg newBlocks

{-------------------------------------------------------------------------------
  Pure variations (primarily for testing)
-------------------------------------------------------------------------------}

fromEither :: Identity (Either Void l) -> l
fromEither = mustBeRight . runIdentity

fromPushManyResult :: Identity (PushManyResult Void l b 'False) -> LedgerDB l b
fromPushManyResult = mustBeRight . pushManyResultToEither . runIdentity

pureBlock :: b -> (Apply 'False, RefOrVal b b)
pureBlock b = (Apply, Val b b)

ledgerDbPush' :: PureLedgerDbConf l b -> b -> LedgerDB l b -> LedgerDB l b
ledgerDbPush' cfg b = fromEither . ledgerDbPush cfg (pureBlock b)

ledgerDbPushMany' :: PureLedgerDbConf l b -> [b] -> LedgerDB l b -> LedgerDB l b
ledgerDbPushMany' cfg bs = fromPushManyResult . ledgerDbPushMany cfg (map pureBlock bs)

ledgerDbSwitch' :: (HasCallStack, Show l, Show b, Typeable l, Typeable b)
                => PureLedgerDbConf l b
                -> Word64 -> [b] -> LedgerDB l b -> LedgerDB l b
ledgerDbSwitch' cfg n bs = fromPushManyResult . ledgerDbSwitch cfg n (map pureBlock bs)

{-------------------------------------------------------------------------------
  Serialisation
-------------------------------------------------------------------------------}

instance (Serialise l, Serialise r) => Serialise (ChainSummary l r) where
  encode = encodeChainSummary encode encode
  decode = decodeChainSummary decode decode

encodeChainSummary :: (l -> Encoding)
                   -> (r -> Encoding)
                   -> ChainSummary l r -> Encoding
encodeChainSummary encodeLedger encodeRef ChainSummary{..} = mconcat [
      Enc.encodeListLen 3
    , encodeTip encodeRef csTip
    , Enc.encodeWord64 csLength
    , encodeLedger csLedger
    ]

decodeChainSummary :: (forall s. Decoder s l)
                   -> (forall s. Decoder s r)
                   -> forall s. Decoder s (ChainSummary l r)
decodeChainSummary decodeLedger decodeRef = do
    Dec.decodeListLenOf 3
    csTip    <- decodeTip decodeRef
    csLength <- Dec.decodeWord64
    csLedger <- decodeLedger
    return ChainSummary{..}

{-------------------------------------------------------------------------------
  Demo
-------------------------------------------------------------------------------}

type Branch     = Char
newtype DemoLedger = DL (Branch, Int) deriving (Show)
newtype DemoBlock  = DB (Branch, Int) deriving (Show)
newtype DemoRef    = DR (Branch, Int) deriving (Show)
newtype DemoErr    = DE (Int, Int)    deriving (Show)

demoConf :: LedgerDbConf Identity DemoLedger DemoRef DemoBlock DemoErr
demoConf = LedgerDbConf {
      ldbConfGenesis = Identity $ DL ('a', 0)
    , ldbConfResolve = \(DR b) -> Identity (DB b)
    , ldbConfApply   = \(DB r@(_, n)) (DL (_, l)) ->
        if n > l then Right $ DL r
                 else Left  $ DE (n, l)
    , ldbConfReapply = \(DB r@(_, n)) (DL (_, l)) ->
        if n > l then DL r
                 else error "ledgerDbReapply: block applied in wrong state"
    }

demoParams :: LedgerDbParams
demoParams = LedgerDbParams {
      ledgerDbSnapEvery     = 4
    , ledgerDbSecurityParam = SecurityParam 6
    }

db0 :: LedgerDB DemoLedger DemoRef
db0 = ledgerDbFromGenesis demoParams (DL ('a', 0))

demo :: [LedgerDB DemoLedger DemoRef]
demo = db0 : go 1 8 db0
  where
    go :: Int -> Int -> LedgerDB DemoLedger DemoRef -> [LedgerDB DemoLedger DemoRef]
    go n m db =
      if n > m then
        let blockInfos = [ (Apply, Val (DR ('b', n-1)) (DB ('b', n-1)))
                         , (Apply, Val (DR ('b', n-0)) (DB ('b', n-0)))
                         ]
            Identity (ValidBlocks db') = ledgerDbSwitch demoConf 1 blockInfos db
        in [db']
      else
        let blockInfo = (Apply, Val (DR ('a', n)) (DB ('a', n)))
            Identity (Right db') = ledgerDbPush demoConf blockInfo db
        in db' : go (n + 1) m db'

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

safeMod :: (Integral a, HasCallStack) => a -> a -> a
safeMod _ 0 = error "safeMod: division by zero"
safeMod x y = x `Prelude.mod` y

safeDiv :: (Eq a, Fractional a, HasCallStack) => a -> a -> a
safeDiv _ 0 = error "safeDiv: division by zero"
safeDiv x y = x Prelude./ y
