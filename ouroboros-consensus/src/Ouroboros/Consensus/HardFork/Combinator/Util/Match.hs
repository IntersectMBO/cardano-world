{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE EmptyCase            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Intended for qualified import
--
-- > import Ouroboros.Consensus.HardFork.Combinator.Util.Match (Mismatch(..))
-- > import qualified Ouroboros.Consensus.HardFork.Combinator.Util.Match as Match
module Ouroboros.Consensus.HardFork.Combinator.Util.Match (
    Mismatch(..)
  , matchNS
  , matchTelescope
    -- * Utilities
  , mismatchOne
  , mismatchTwo
  , mkMismatchTwo
    -- * SOP operators
  , bihap
  , bihmap
  , bihcmap
  ) where

import           Data.Bifunctor
import           Data.Functor.Product
import           Data.SOP.Strict
import           Data.Void

import           Cardano.Prelude (NoUnexpectedThunks (..),
                     allNoUnexpectedThunks)

import           Ouroboros.Consensus.Util.SOP ()

import           Ouroboros.Consensus.HardFork.Combinator.Util.Telescope
                     (Telescope (..))
import qualified Ouroboros.Consensus.HardFork.Combinator.Util.Telescope as Telescope

{-------------------------------------------------------------------------------
  Main API
-------------------------------------------------------------------------------}

data Mismatch :: (k -> *) -> (k -> *) -> [k] -> * where
  ML :: f x -> NS g xs -> Mismatch f g (x ': xs)
  MR :: NS f xs -> g x -> Mismatch f g (x ': xs)
  MS :: Mismatch f g xs -> Mismatch f g (x ': xs)

matchNS :: NS f xs -> NS g xs -> Either (Mismatch f g xs) (NS (Product f g) xs)
matchNS = go
  where
    go :: NS f xs -> NS g xs -> Either (Mismatch f g xs) (NS (Product f g) xs)
    go (Z fx) (Z gx) = Right (Z (Pair fx gx))
    go (S l)  (S r)  = bimap MS S $ go l r
    go (Z fx) (S r)  = Left $ ML fx r
    go (S l)  (Z gx) = Left $ MR l gx

matchTelescope :: NS h xs
               -> Telescope g f xs
               -> Either (Mismatch h f xs) (Telescope g (Product h f) xs)
matchTelescope = go
  where
    go :: NS h xs
       -> Telescope g f xs
       -> Either (Mismatch h f xs) (Telescope g (Product h f) xs)
    go (Z l)  (TZ fx)    = Right (TZ (Pair l fx))
    go (S r)  (TS  gx t) = bimap MS (TS gx) $ go r t
    go (Z hx) (TS _gx t) = Left $ ML hx (Telescope.tip t)
    go (S l)  (TZ fx)    = Left $ MR l fx

{-------------------------------------------------------------------------------
  Utilities
-------------------------------------------------------------------------------}

-- | We cannot give a mismatch if we have only one type variable
mismatchOne :: Mismatch f g '[x] -> Void
mismatchOne (ML _ ns) = case ns of {}
mismatchOne (MR ns _) = case ns of {}
mismatchOne (MS m)    = case m  of {}

-- | If we only have two eras, only two possibilities for a mismatch
mismatchTwo :: Mismatch f g '[x, y] -> Either (f x, g y) (f y, g x)
mismatchTwo (ML fx gy) = Left (fx, unZ gy)
mismatchTwo (MR fy gx) = Right (unZ fy, gx)
mismatchTwo (MS m)     = absurd $ mismatchOne m

mkMismatchTwo :: Either (f x, g y) (f y, g x) -> Mismatch f g '[x, y]
mkMismatchTwo (Left  (fx, gy)) = ML fx (Z gy)
mkMismatchTwo (Right (fy, gx)) = MR (Z fy) gx

{-------------------------------------------------------------------------------
  Subset of the (generalized) SOP operators
-------------------------------------------------------------------------------}

bihap :: NP (f -.-> f') xs
      -> NP (g -.-> g') xs
      -> Mismatch f g xs -> Mismatch f' g' xs
bihap = \gs fs t -> go t gs fs
  where
    go :: Mismatch f g xs
       -> NP (f -.-> f') xs
       -> NP (g -.-> g') xs
       -> Mismatch f' g' xs
    go (ML fx r) (f :* _)  (_ :* gs) = ML (apFn f fx) (hap gs r)
    go (MR l gx) (_ :* fs) (g :* _)  = MR (hap fs l) (apFn g gx)
    go (MS m)    (_ :* fs) (_ :* gs) = MS (go m fs gs)

bihmap :: SListI xs
       => (forall x. f x -> f' x)
       -> (forall x. g x -> g' x)
       -> Mismatch f g xs -> Mismatch f' g' xs
bihmap = bihcmap (Proxy @Top)

-- | Bifunctor analogue of 'hcmap'
bihcmap :: All c xs
        => proxy c
        -> (forall x. c x => f x -> f' x)
        -> (forall x. c x => g x -> g' x)
        -> Mismatch f g xs -> Mismatch f' g' xs
bihcmap p g f = bihap (hcpure p (fn g)) (hcpure p (fn f))

{-------------------------------------------------------------------------------
  Standard type class instances
-------------------------------------------------------------------------------}

deriving stock instance ( All (Compose Eq f) xs
                        , All (Compose Eq g) xs
                        ) => Eq (Mismatch f g xs)

deriving stock instance ( All (Compose Eq  f) xs
                        , All (Compose Ord f) xs
                        , All (Compose Eq  g) xs
                        , All (Compose Ord g) xs
                        ) => Ord (Mismatch f g xs)

deriving stock instance ( All (Compose Show f) xs
                        , All (Compose Show g) xs
                        ) => Show (Mismatch f g xs)

instance ( All (Compose NoUnexpectedThunks f) xs
         , All (Compose NoUnexpectedThunks g) xs
         ) => NoUnexpectedThunks (Mismatch f g xs) where
  showTypeOf _ = "Mismatch"
  whnfNoUnexpectedThunks ctxt = \case
    ML l r -> allNoUnexpectedThunks [
                  noUnexpectedThunks ("l" : "ML" : ctxt) l
                , noUnexpectedThunks ("r" : "ML" : ctxt) r
                ]
    MR l r -> allNoUnexpectedThunks [
                  noUnexpectedThunks ("l" : "MR" : ctxt) l
                , noUnexpectedThunks ("r" : "MR" : ctxt) r
                ]
    MS m   -> noUnexpectedThunks ("MS" : ctxt) m
