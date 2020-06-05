{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Ouroboros.Consensus.Ledger.Extended (
    -- * Extended ledger state
    ExtLedgerState(..)
  , ExtValidationError(..)
    -- * Serialisation
  , encodeExtLedgerState
  , decodeExtLedgerState
    -- * Casts
  , castExtLedgerState
  ) where

import           Codec.CBOR.Decoding (Decoder)
import           Codec.CBOR.Encoding (Encoding)
import           Control.Monad.Except
import           Data.Coerce
import           Data.Proxy
import           Data.Typeable
import           GHC.Generics (Generic)

import           Cardano.Prelude (NoUnexpectedThunks (..))

import           Ouroboros.Network.Block

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Protocol.Abstract

{-------------------------------------------------------------------------------
  Extended ledger state
-------------------------------------------------------------------------------}

-- | Extended ledger state
--
-- This is the combination of the header state and the ledger state proper.
data ExtLedgerState blk = ExtLedgerState {
      ledgerState :: !(LedgerState blk)
    , headerState :: !(HeaderState blk)
    }
  deriving (Generic)

data ExtValidationError blk =
    ExtValidationErrorLedger !(LedgerError blk)
  | ExtValidationErrorHeader !(HeaderError blk)
  deriving (Generic)

instance LedgerSupportsProtocol blk => NoUnexpectedThunks (ExtValidationError blk)

deriving instance LedgerSupportsProtocol blk => Show (ExtLedgerState     blk)
deriving instance LedgerSupportsProtocol blk => Show (ExtValidationError blk)
deriving instance LedgerSupportsProtocol blk => Eq   (ExtValidationError blk)

-- | We override 'showTypeOf' to show the type of the block
--
-- This makes debugging a bit easier, as the block gets used to resolve all
-- kinds of type families.
instance LedgerSupportsProtocol blk => NoUnexpectedThunks (ExtLedgerState blk) where
  showTypeOf _ = show $ typeRep (Proxy @(ExtLedgerState blk))

deriving instance ( LedgerSupportsProtocol blk
                  , Eq (ConsensusState (BlockProtocol blk))
                  ) => Eq (ExtLedgerState blk)

-- | Lemma:
--
-- > let tickedLedger = applyChainTick cfg (blockSlot blk) st
-- > in Right st' = runExcept $
-- >   applyLedgerBlock cfg blk tickedLedger ->
-- >      protocolLedgerView st'
-- >   == protocolLedgerView (tickedLedgerState tickedLedger)
--
-- In other words: 'applyLedgerBlock' doesn't affect the result of
-- 'protocolLedgerView'.
--
-- This should be true for each ledger because consensus depends on it.
_lemma_protocoLedgerView_applyLedgerBlock
  :: (LedgerSupportsProtocol blk, Eq (LedgerView (BlockProtocol blk)))
  => LedgerConfig blk
  -> blk
  -> LedgerState blk
  -> Either String ()
_lemma_protocoLedgerView_applyLedgerBlock cfg blk st
    | Right lhs' <- runExcept lhs
    , lhs' /= rhs
    = Left $ unlines
      [ "protocolLedgerView /= protocolLedgerView . applyLedgerBlock"
      , show lhs'
      , " /= "
      , show rhs
      ]
    | otherwise
    = Right ()
  where
    tickedLedger = applyChainTick cfg (blockSlot blk) st
    lhs = protocolLedgerView cfg <$> applyLedgerBlock cfg blk tickedLedger
    rhs = protocolLedgerView cfg  $  tickedLedgerState        tickedLedger

{-------------------------------------------------------------------------------
  The extended ledger can behave like a ledger
-------------------------------------------------------------------------------}

type instance LedgerCfg (ExtLedgerState blk) = TopLevelConfig blk

instance ( IsLedger (LedgerState  blk)
         , LedgerSupportsProtocol blk
         )
      => IsLedger (ExtLedgerState blk) where
  type LedgerErr (ExtLedgerState blk) = ExtValidationError blk

  applyChainTick cfg slot (ExtLedgerState ledger header) =
      Ticked slot $ ExtLedgerState ledger' header
    where
      Ticked _slot ledger' = applyChainTick (configLedger cfg) slot ledger

instance ( LedgerSupportsProtocol blk
         ) => ApplyBlock (ExtLedgerState blk) blk where
  applyLedgerBlock cfg blk (Ticked {
                                tickedSlotNo      = slot
                              , tickedLedgerState = ExtLedgerState lgr hdr
                              }) = do
      hdr' <- withExcept ExtValidationErrorHeader $
                validateHeader
                  cfg
                  (Ticked slot ledgerView)
                  (getHeader blk)
                  hdr
      lgr' <- withExcept ExtValidationErrorLedger $
                applyLedgerBlock
                  (configLedger cfg)
                  blk
                  (Ticked slot lgr)

      return $! ExtLedgerState lgr' hdr'
    where
      ledgerView :: LedgerView (BlockProtocol blk)
      ledgerView = protocolLedgerView (configLedger cfg) lgr

  reapplyLedgerBlock cfg blk (Ticked {
                                tickedSlotNo      = slot
                              , tickedLedgerState = ExtLedgerState lgr hdr
                              }) =
      ExtLedgerState {
          ledgerState = reapplyLedgerBlock
                          (configLedger cfg)
                          blk
                          (Ticked slot lgr)
        , headerState = cantBeError $
                         validateHeader
                           cfg
                           (Ticked slot ledgerView)
                           (getHeader blk)
                           hdr
        }
    where
      cantBeError :: Except e a -> a
      cantBeError = either (error "reapplyLedgerBlock: impossible") id
                  . runExcept

      ledgerView :: LedgerView (BlockProtocol blk)
      ledgerView = protocolLedgerView (configLedger cfg) lgr

  ledgerTipPoint = ledgerTipPoint . ledgerState

{-------------------------------------------------------------------------------
  Serialisation
-------------------------------------------------------------------------------}

encodeExtLedgerState :: (LedgerState   blk -> Encoding)
                     -> (ConsensusState (BlockProtocol blk) -> Encoding)
                     -> (AnnTip        blk -> Encoding)
                     -> ExtLedgerState blk -> Encoding
encodeExtLedgerState encodeLedgerState
                     encodeConsensusState
                     encodeAnnTip
                     ExtLedgerState{..} = mconcat [
      encodeLedgerState  ledgerState
    , encodeHeaderState' headerState
    ]
  where
    encodeHeaderState' = encodeHeaderState
                           encodeConsensusState
                           encodeAnnTip

decodeExtLedgerState :: (forall s. Decoder s (LedgerState    blk))
                     -> (forall s. Decoder s (ConsensusState (BlockProtocol blk)))
                     -> (forall s. Decoder s (AnnTip         blk))
                     -> (forall s. Decoder s (ExtLedgerState blk))
decodeExtLedgerState decodeLedgerState
                     decodeConsensusState
                     decodeAnnTip = do
    ledgerState <- decodeLedgerState
    headerState <- decodeHeaderState'
    return ExtLedgerState{..}
  where
    decodeHeaderState' = decodeHeaderState
                           decodeConsensusState
                           decodeAnnTip

{-------------------------------------------------------------------------------
  Casts
-------------------------------------------------------------------------------}

castExtLedgerState
  :: ( Coercible (LedgerState blk)
                 (LedgerState blk')
     , Coercible (ConsensusState (BlockProtocol blk))
                 (ConsensusState (BlockProtocol blk'))
     , TipInfo blk ~ TipInfo blk'
     )
  => ExtLedgerState blk -> ExtLedgerState blk'
castExtLedgerState ExtLedgerState{..} = ExtLedgerState {
      ledgerState = coerce ledgerState
    , headerState = castHeaderState headerState
    }
