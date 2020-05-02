{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

-- | Intended for qualified import
--
-- > import Ouroboros.Consensus.HardFork.Combinator.Protocol.State (HardForkConsensusState)
-- > import qualified Ouroboros.Consensus.HardFork.Combinator.Protocol.State as ProtocolState
module Ouroboros.Consensus.HardFork.Combinator.Protocol.State (
    HardForkConsensusState
  , HardForkValidationErr(..)
    -- * Update the state
  , check
  , rewind
  , update
  ) where

import           Codec.Serialise (Serialise)
import           Control.Monad.Except
import           Crypto.Random (MonadRandom)
import           Data.Functor.Product
import           Data.SOP.Strict
import           GHC.Generics (Generic)

import           Ouroboros.Network.Block

import           Cardano.Prelude (NoUnexpectedThunks)

import           Ouroboros.Consensus.Block.Abstract
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Util ((.:))

import           Ouroboros.Consensus.HardFork.Combinator.Abstract
import           Ouroboros.Consensus.HardFork.Combinator.Basics
import           Ouroboros.Consensus.HardFork.Combinator.Protocol.LedgerView
import           Ouroboros.Consensus.HardFork.Combinator.SingleEra
import           Ouroboros.Consensus.HardFork.Combinator.State (HardForkState,
                     Translate (..))
import qualified Ouroboros.Consensus.HardFork.Combinator.State as State
import           Ouroboros.Consensus.HardFork.Combinator.Translation
import           Ouroboros.Consensus.HardFork.Combinator.Util.InPairs
                     (InPairs (..), RequiringBoth (..))
import qualified Ouroboros.Consensus.HardFork.Combinator.Util.InPairs as InPairs
import qualified Ouroboros.Consensus.HardFork.Combinator.Util.Match as Match

{-------------------------------------------------------------------------------
  Types
-------------------------------------------------------------------------------}

type HardForkConsensusState xs = HardForkState SingleEraConsensusState xs

data HardForkValidationErr xs =
    -- | Validation error from one of the eras
    HardForkValidationErrFromEra (OneEraValidationErr xs)

    -- | We tried to apply a block from the wrong era
  | HardForkValidationErrWrongEra (MismatchEraInfo xs)
  deriving (Generic)

{-------------------------------------------------------------------------------
  Leader check

  NOTE: The precondition to `align` is satisfied: the consensus state will never
  be ahead (but possibly behind) the ledger state, which we tick first.
-------------------------------------------------------------------------------}

check :: forall m xs. (MonadRandom m, CanHardFork xs)
      => ConsensusConfig (HardForkProtocol xs)
      -> Ticked (HardForkLedgerView xs)
      -> HardForkConsensusState xs
      -> m (Maybe (OneEraIsLeader xs))
check cfg@HardForkConsensusConfig{..} (Ticked slot ledgerView) =
      fmap aux
    . hsequence'
    . State.tip
    . State.align
        (translateConsensus ei cfg)
        (hcmap proxySingle (fn_2 . checkOne ei slot) cfgs)
        ledgerView
  where
    cfgs = getPerEraConsensusConfig hardForkConsensusConfigPerEra
    ei   = State.epochInfoLedgerView hardForkConsensusConfigShape ledgerView

    aux :: NS (Maybe :.: SingleEraIsLeader) xs -> Maybe (OneEraIsLeader xs)
    aux ns = hcollapse (hzipWith (K .: injectProof) injections ns)

checkOne :: (MonadRandom m, SingleEraBlock blk)
         => EpochInfo Identity
         -> SlotNo
         -> SingleEraConsensusConfig            blk
         -> HardForkEraLedgerView               blk
         -> SingleEraConsensusState             blk
         -> (m :.: Maybe :.: SingleEraIsLeader) blk
checkOne ei slot cfg
         HardForkEraLedgerView{..}
         (SingleEraConsensusState consensusState) = Comp . fmap Comp $
     fmap (fmap SingleEraIsLeader) $
       checkIsLeader
         (completeConsensusConfig' ei cfg)
         (Ticked slot hardForkEraLedgerView)
         consensusState

{-------------------------------------------------------------------------------
  Rolling forward and backward
-------------------------------------------------------------------------------}

rewind :: (CanHardFork xs, Serialise (HeaderHash hdr))
       => SecurityParam
       -> Point hdr
       -> HardForkConsensusState xs
       -> Maybe (HardForkConsensusState xs)
rewind k p =
        -- Using just the 'SlotNo' is okay: no EBBs near transition
        State.retractToSlot (pointSlot p)
    >=> (hsequence' . hcmap proxySingle rewindOne)
  where
    rewindOne :: forall blk. SingleEraBlock          blk
              => SingleEraConsensusState             blk
              -> (Maybe :.: SingleEraConsensusState) blk
    rewindOne (SingleEraConsensusState st) = Comp $
        SingleEraConsensusState <$>
          rewindConsensusState (Proxy @(BlockProtocol blk)) k p st

update :: forall xs. CanHardFork xs
       => ConsensusConfig (HardForkProtocol xs)
       -> Ticked (HardForkLedgerView xs)
       -> OneEraValidateView xs
       -> HardForkConsensusState xs
       -> Except (HardForkValidationErr xs) (HardForkConsensusState xs)
update cfg@HardForkConsensusConfig{..}
       (Ticked slot ledgerView)
       (OneEraValidateView validateView)
       consensusState =
    case State.match validateView ledgerView of
      Left mismatch ->
        throwError $ HardForkValidationErrWrongEra . MismatchEraInfo $
          Match.bihcmap proxySingle singleEraInfo ledgerInfo mismatch
      Right matched ->
           hsequence'
         . State.tickAllPast hardForkConsensusConfigK
         . State.align
            (translateConsensus ei cfg)
            (hczipWith proxySingle (fn_2 .: updateEra ei slot) cfgs injections)
            matched
         $ consensusState
  where
    cfgs = getPerEraConsensusConfig hardForkConsensusConfigPerEra
    ei   = State.epochInfoLedgerView hardForkConsensusConfigShape ledgerView

updateEra :: forall xs blk. SingleEraBlock blk
          => EpochInfo Identity
          -> SlotNo
          -> SingleEraConsensusConfig                                        blk
          -> Injection SingleEraValidationErr xs                             blk
          -> Product SingleEraValidateView HardForkEraLedgerView             blk
          -> SingleEraConsensusState                                         blk
          -> (Except (HardForkValidationErr xs) :.: SingleEraConsensusState) blk
updateEra ei slot cfg injectErr
          (Pair (SingleEraValidateView validateView) HardForkEraLedgerView{..})
          (SingleEraConsensusState consensusState) = Comp $
    withExcept (injectValidationErr injectErr) $
      fmap SingleEraConsensusState $
        updateConsensusState
          (completeConsensusConfig' ei cfg)
          (Ticked slot hardForkEraLedgerView)
          validateView
          consensusState

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

ledgerInfo :: forall blk. SingleEraBlock blk
           => State.Current HardForkEraLedgerView blk -> LedgerEraInfo blk
ledgerInfo _ = LedgerEraInfo $ singleEraInfo (Proxy @blk)

translateConsensus :: CanHardFork xs
                   => EpochInfo Identity
                   -> ConsensusConfig (HardForkProtocol xs)
                   -> InPairs (Translate SingleEraConsensusState) xs
translateConsensus ei HardForkConsensusConfig{..} =
    InPairs.requiringBoth
      (getPerEraConsensusConfig hardForkConsensusConfigPerEra)
      (InPairs.hcmap
        proxySingle
        (RequireBoth . aux)
        (translateConsensusState hardForkEraTranslation))
  where
    aux :: forall blk blk'. (SingleEraBlock blk, SingleEraBlock blk')
        => TranslateEraConsensusState blk blk'
        -> SingleEraConsensusConfig blk
        -> SingleEraConsensusConfig blk'
        -> Translate SingleEraConsensusState blk blk'
    aux f pcfg pcfg' = Translate $ \epoch (SingleEraConsensusState st) ->
        SingleEraConsensusState $
          translateConsensusStateWith f cfg cfg' epoch st
      where
        cfg  :: ConsensusConfig (BlockProtocol blk)
        cfg' :: ConsensusConfig (BlockProtocol blk')
        cfg  = completeConsensusConfig' ei pcfg
        cfg' = completeConsensusConfig' ei pcfg'

injectValidationErr :: Injection SingleEraValidationErr xs blk
                    -> ValidationErr (BlockProtocol blk)
                    -> HardForkValidationErr xs
injectValidationErr inj =
      HardForkValidationErrFromEra
    . OneEraValidationErr
    . unK
    . apFn inj
    . SingleEraValidationErr

injectProof :: Injection SingleEraIsLeader xs blk
            -> (:.:) Maybe SingleEraIsLeader blk
            -> Maybe (OneEraIsLeader xs)
injectProof inj (Comp pf) = (OneEraIsLeader . unK . apFn inj) <$> pf

{-------------------------------------------------------------------------------
  Instances
-------------------------------------------------------------------------------}

deriving instance CanHardFork xs => Eq                 (HardForkValidationErr xs)
deriving instance CanHardFork xs => Show               (HardForkValidationErr xs)
deriving instance CanHardFork xs => NoUnexpectedThunks (HardForkValidationErr xs)
