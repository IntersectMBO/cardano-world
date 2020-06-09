{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Ouroboros.Consensus.Block.NestedContent (
    -- * Block contents
    HasNestedContent (..)
  , NestedCtxt_
  , curriedNest
    -- * Flip type arguments
  , NestedCtxt(..)
  , castNestedCtxt
  , mapNestedCtxt
    -- * Existentials
  , SomeBlock(..)
  , flipSomeNestedCtxt
  , unflipSomeNestedCtxt
  , castSomeNestedCtxt
    -- * Convenience re-exports
  , module Ouroboros.Consensus.Util.DepPair
  ) where

import           Data.Maybe (isJust)
import           Data.Proxy
import           Data.Type.Equality

import           Ouroboros.Consensus.Util (Some (..))
import           Ouroboros.Consensus.Util.DepPair

{-------------------------------------------------------------------------------
  Block contents
-------------------------------------------------------------------------------}

-- | Nested content inside a block
--
-- Consider a simplified version of the hard fork combinator, defining
--
-- > type HardFork a b = Either a b
--
-- Then encoding @Hardfork ByronBlock ShelleyBlock@ is easy, in the same way
-- that we encode /any/ @Either@. The /header/ of such a block will have type
--
-- > HardFork (Header ByronBlock) (Header ShelleyBlock)
--
-- and encoding those (for example, to send them across the network) is
-- similarly trivial. But now suppose we want to read a header from disk. We do
-- not store headers directly, but instead store the blocks. The DB will know
-- the offset and length (both in bytes) of the header inside the block, but
-- how do we decode such a header? If it's a Byron block, we should use the
-- decoder for @Header ByronBlock@, and similarly for Shelley, but how should
-- we express this more generally?
--
-- Here is where 'HasNestedContent' comes in. Continuing the example, we can
-- @unnest@ a @Header (HardFork ByronBlock ShelleyBlock)@ into a pair of values,
-- where the first value (a @NestedCtxt@) tells us what type of block we have,
-- and the second value gives us the actual header. So, if the first value says
-- "this is a Byron block", the second value is a @Header ByronBlock@, and vice
-- versa. In other words, this is a dependent pair.
--
-- This then solves the serialisation problem: we expect a /dependent/ decoder
-- which, /given/ a @NestedCtxt@ identifying the block type, decodes the raw
-- bytes from the block into the type indicated by that @NestedCtxt@.
--
-- TODO: We could perhaps define this independent of blocks in 'DepPair'.
class ( forall a. Show (NestedCtxt_ blk f a)
      , SameDepIndex (NestedCtxt_ blk f)
      ) => HasNestedContent f blk where
  unnest :: f blk -> DepPair (NestedCtxt f blk)
  nest   :: DepPair (NestedCtxt f blk) -> f blk

  -- Defaults when there is only a single type

  default unnest :: ( TrivialDependency (NestedCtxt f blk)
                    , TrivialIndex (NestedCtxt f blk) ~ f blk
                    )
                 => f blk -> DepPair (NestedCtxt f blk)
  unnest = DepPair indexIsTrivial

  default nest :: ( TrivialDependency (NestedCtxt f blk)
                  , TrivialIndex (NestedCtxt f blk) ~ f blk
                  )
               => DepPair (NestedCtxt f blk) -> f blk
  nest (DepPair x y) = fromTrivialDependency x y

curriedNest :: HasNestedContent f blk => NestedCtxt f blk a -> a -> f blk
curriedNest ctxt a = nest (DepPair ctxt a)

-- | Context identifying what kind of block we have
--
-- In almost all places we will use 'NestedCtxt' rather than 'NestedCtxt_'.
data family NestedCtxt_ blk :: (* -> *) -> (* -> *)

{-------------------------------------------------------------------------------
  Flip arguments
-------------------------------------------------------------------------------}

-- | Version of 'NestedCtxt_' with the type arguments swapped
--
-- 'NestedCtxt' must be indexed on @blk@: it is the block that determines this
-- type. However, we often want to partially apply the second argument (the
-- functor), leaving the block type not yet defined.
newtype NestedCtxt f blk a = NestedCtxt {
      flipNestedCtxt :: NestedCtxt_ blk f a
    }

deriving instance Show (NestedCtxt_ blk f a)
               => Show (NestedCtxt f blk a)

instance SameDepIndex (NestedCtxt_ blk f)
      => SameDepIndex (NestedCtxt f blk) where
  sameDepIndex (NestedCtxt ctxt) (NestedCtxt ctxt') =
      sameDepIndex ctxt ctxt'

instance TrivialDependency (NestedCtxt_ blk f)
      => TrivialDependency (NestedCtxt f blk) where
  type TrivialIndex (NestedCtxt f blk) = TrivialIndex (NestedCtxt_ blk f)
  hasSingleIndex (NestedCtxt ctxt) (NestedCtxt ctxt') =
      hasSingleIndex ctxt ctxt'
  indexIsTrivial =
      NestedCtxt indexIsTrivial

castNestedCtxt :: (NestedCtxt_ blk f a -> NestedCtxt_ blk' f a)
               -> NestedCtxt f blk  a
               -> NestedCtxt f blk' a
castNestedCtxt coerce (NestedCtxt ctxt) = NestedCtxt (coerce ctxt)

mapNestedCtxt :: (NestedCtxt_ blk f a -> NestedCtxt_ blk' f' a')
              -> NestedCtxt f  blk  a
              -> NestedCtxt f' blk' a'
mapNestedCtxt f (NestedCtxt ctxt) = NestedCtxt (f ctxt)

{-------------------------------------------------------------------------------
  Existentials
-------------------------------------------------------------------------------}

-- | Hide the type argument of a block-indexed GADT
--
-- @SomeBlock f blk@ is isomorphic to @Some (f blk)@, but is more convenient
-- in partial applications.
data SomeBlock (f :: * -> * -> *) blk where
  SomeBlock :: f blk a -> SomeBlock f blk

deriving instance HasNestedContent f blk => Show (SomeBlock (NestedCtxt f) blk)

instance SameDepIndex (NestedCtxt_ blk f)
      => Eq (SomeBlock (NestedCtxt f) blk) where
  SomeBlock ctxt == SomeBlock ctxt' = isJust (sameDepIndex ctxt ctxt')

flipSomeNestedCtxt :: Some (NestedCtxt_ blk f) -> SomeBlock (NestedCtxt f) blk
flipSomeNestedCtxt (Some ctxt) = SomeBlock (NestedCtxt ctxt)

unflipSomeNestedCtxt :: SomeBlock (NestedCtxt f) blk -> Some (NestedCtxt_ blk f)
unflipSomeNestedCtxt (SomeBlock ctxt) = Some (flipNestedCtxt ctxt)

castSomeNestedCtxt :: (forall a. NestedCtxt_ blk f a -> NestedCtxt_ blk' f a)
                   -> SomeBlock (NestedCtxt f) blk
                   -> SomeBlock (NestedCtxt f) blk'
castSomeNestedCtxt coerce (SomeBlock ctxt) = SomeBlock (castNestedCtxt coerce ctxt)
