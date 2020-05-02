{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

-- > import Ouroboros.Consensus.HardFork.Combinator.Util.Tails (Tails(..))
-- > import qualified Ouroboros.Consensus.HardFork.Combinator.Util.Tails as Tails
module Ouroboros.Consensus.HardFork.Combinator.Util.Tails (
    Tails(..)
  , hmap
  , hcmap
  , hpure
  , hcpure
  ) where

import           Data.SOP.Strict hiding (hcmap, hcpure, hmap, hpure)
import qualified Data.SOP.Strict as SOP

{-------------------------------------------------------------------------------
  Tails
-------------------------------------------------------------------------------}

-- | For every tail @(x ': xs)@ of the list, an @f x y@ for every @y@ in @xs@
data Tails (f :: k -> k -> *) (xs :: [k]) where
  TNil  :: Tails f '[]
  TCons :: NP (f x) xs -> Tails f xs -> Tails f (x ': xs)

hmap :: SListI xs
     => (forall x y. f x y -> g x y)
     -> Tails f xs -> Tails g xs
hmap = hcmap (Proxy @Top)

hcmap :: forall proxy c f g xs. All c xs
      => proxy c
      -> (forall x y. (c x, c y) => f x y -> g x y)
      -> Tails f xs -> Tails g xs
hcmap p g = go
  where
    go :: All c xs' => Tails f xs' -> Tails g xs'
    go TNil           = TNil
    go (TCons fs fss) = TCons (SOP.hcmap p g fs) (go fss)

hpure :: SListI xs => (forall x y. f x y) -> Tails f xs
hpure = hcpure (Proxy @Top)

hcpure :: forall proxy f c xs. All c xs
       => proxy c
       -> (forall x y. (c x, c y) => f x y) -> Tails f xs
hcpure p f = go sList
  where
    go :: All c xs' => SList xs' -> Tails f xs'
    go SNil  = TNil
    go SCons = TCons (SOP.hcpure p f) (go sList)
