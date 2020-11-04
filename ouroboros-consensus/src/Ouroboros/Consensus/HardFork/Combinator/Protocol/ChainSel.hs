{-# LANGUAGE EmptyCase           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

-- | Infrastructure for doing chain selection across eras
module Ouroboros.Consensus.HardFork.Combinator.Protocol.ChainSel (
    AcrossEraSelection(..)
  , acrossEraSelection
  , WithBlockNo(..)
  , mapWithBlockNo
  ) where

import           Data.Kind (Type)
import           Data.SOP.Strict

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.TypeFamilyWrappers
import           Ouroboros.Consensus.Util.Assert

import           Ouroboros.Consensus.HardFork.Combinator.Abstract.SingleEraBlock
import           Ouroboros.Consensus.HardFork.Combinator.Util.Tails (Tails (..))

{-------------------------------------------------------------------------------
  Configuration
-------------------------------------------------------------------------------}

data AcrossEraSelection :: Type -> Type -> Type where
  -- | Just compare block numbers
  --
  -- This is a useful default when two eras run totally different consensus
  -- protocols, and we just want to choose the longer chain.
  CompareBlockNo :: AcrossEraSelection x y

  -- | Two eras running the same protocol
  --
  -- In this case, we can just call @compareChains@ even across eras.
  -- (The 'ChainSelConfig' must also be the same in both eras: we assert this
  -- at the value level.)
  --
  -- NOTE: We require that the eras have the same /protocol/, not merely the
  -- same 'SelectView', because if we have two eras with different protocols
  -- that happen to use the same 'SelectView' but a different way to compare
  -- chains, it's not clear how to do cross-era selection.
  SelectSameProtocol ::
       BlockProtocol x ~ BlockProtocol y
    => AcrossEraSelection x y

  -- | Custom chain selection
  --
  -- This is the most general form, and allows to override chain selection for
  -- the specific combination of two eras with a custom comparison function.
  CustomChainSel ::
       (    ChainSelConfig (BlockProtocol x)
         -> ChainSelConfig (BlockProtocol y)
         -> SelectView (BlockProtocol x)
         -> SelectView (BlockProtocol y)
         -> Ordering
       )
    -> AcrossEraSelection x y

{-------------------------------------------------------------------------------
  Compare two eras
-------------------------------------------------------------------------------}

withinEra ::
     forall blk. SingleEraBlock blk
  => WrapChainSelConfig blk
  -> WrapSelectView blk
  -> WrapSelectView blk
  -> Ordering
withinEra (WrapChainSelConfig cfg) (WrapSelectView l) (WrapSelectView r) =
    compareChains (Proxy @(BlockProtocol blk)) cfg l r

acrossEras ::
     forall blk blk'. SingleEraBlock blk
  => WrapChainSelConfig blk
  -> WrapChainSelConfig blk'
  -> WithBlockNo WrapSelectView blk
  -> WithBlockNo WrapSelectView blk'
  -> AcrossEraSelection blk blk'
  -> Ordering
acrossEras (WrapChainSelConfig cfgL)
           (WrapChainSelConfig cfgR)
           (WithBlockNo bnoL (WrapSelectView l))
           (WithBlockNo bnoR (WrapSelectView r)) = \case
    CompareBlockNo     -> compare bnoL bnoR
    CustomChainSel f   -> f cfgL cfgR l r
    SelectSameProtocol -> assertEqWithMsg (cfgL, cfgR) $
                            compareChains
                              (Proxy @(BlockProtocol blk))
                              cfgL
                              l
                              r

acrossEraSelection ::
     All SingleEraBlock              xs
  => NP WrapChainSelConfig           xs
  -> Tails AcrossEraSelection        xs
  -> WithBlockNo (NS WrapSelectView) xs
  -> WithBlockNo (NS WrapSelectView) xs
  -> Ordering
acrossEraSelection = \cfgs ffs l r ->
    goLeft cfgs ffs (distribBlockNo l, distribBlockNo r)
  where
    goLeft ::
         All SingleEraBlock                xs
      => NP WrapChainSelConfig             xs
      -> Tails AcrossEraSelection          xs
      -> ( NS (WithBlockNo WrapSelectView) xs
         , NS (WithBlockNo WrapSelectView) xs
         )
      -> Ordering
    goLeft _         TNil            = \(a, _) -> case a of {}
    goLeft (c :* cs) (TCons fs ffs') = \case
        (Z a, Z b) -> withinEra c (dropBlockNo a) (dropBlockNo b)
        (Z a, S b) ->          goRight c a cs fs b
        (S a, Z b) -> invert $ goRight c b cs fs a
        (S a, S b) -> goLeft cs ffs' (a, b)

    goRight ::
         forall x xs. (SingleEraBlock x, All SingleEraBlock xs)
      => WrapChainSelConfig         x
      -> WithBlockNo WrapSelectView x
      -> NP WrapChainSelConfig           xs
      -> NP (AcrossEraSelection     x)   xs
      -> NS (WithBlockNo WrapSelectView) xs
      -> Ordering
    goRight cfgL a = go
      where
        go :: forall xs'. All SingleEraBlock  xs'
           => NP WrapChainSelConfig           xs'
           -> NP (AcrossEraSelection x)       xs'
           -> NS (WithBlockNo WrapSelectView) xs'
           -> Ordering
        go _         Nil          b  = case b of {}
        go (c :* _)  (f :* _)  (Z b) = acrossEras cfgL c a b f
        go (_ :* cs) (_ :* fs) (S b) = go cs fs b

{-------------------------------------------------------------------------------
  WithBlockNo
-------------------------------------------------------------------------------}

data WithBlockNo (f :: k -> Type) (a :: k) = WithBlockNo {
      getBlockNo  :: BlockNo
    , dropBlockNo :: f a
    }
  deriving (Show)

mapWithBlockNo :: (f x -> g y) -> WithBlockNo f x -> WithBlockNo g y
mapWithBlockNo f (WithBlockNo bno fx) = WithBlockNo bno (f fx)

distribBlockNo :: SListI xs => WithBlockNo (NS f) xs -> NS (WithBlockNo f) xs
distribBlockNo (WithBlockNo b ns) = hmap (WithBlockNo b) ns

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

invert :: Ordering -> Ordering
invert LT = GT
invert GT = LT
invert EQ = EQ
