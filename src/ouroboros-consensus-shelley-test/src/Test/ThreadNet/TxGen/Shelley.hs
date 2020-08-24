{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TypeFamilies             #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Test.ThreadNet.TxGen.Shelley (
    ShelleyTxGenExtra(..)
  , WhetherToGeneratePPUs(..)
  , genTx
  , mkGenEnv
  ) where

import           Control.Monad.Except (runExcept)

import           Cardano.Crypto.Hash (HashAlgorithm)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.SupportsMempool

import qualified Shelley.Spec.Ledger.LedgerState as SL
import qualified Shelley.Spec.Ledger.STS.Ledger as STS

import           Ouroboros.Consensus.Shelley.Ledger

import           Test.QuickCheck

import           Test.ThreadNet.TxGen (TxGen (..))

import qualified Test.Shelley.Spec.Ledger.Generator.Constants as Gen
import qualified Test.Shelley.Spec.Ledger.Generator.Core as Gen
import qualified Test.Shelley.Spec.Ledger.Generator.Presets as Gen.Presets
import qualified Test.Shelley.Spec.Ledger.Generator.Utxo as Gen

import           Test.Consensus.Shelley.MockCrypto (TPraosMockCrypto)
import           Test.ThreadNet.Infra.Shelley

data ShelleyTxGenExtra h = ShelleyTxGenExtra
  { -- | Generator environment.
    stgeGenEnv  :: Gen.GenEnv (TPraosMockCrypto h)
    -- | Generate no transactions before this slot.
  , stgeStartAt :: SlotNo
  }

instance HashAlgorithm h => TxGen (ShelleyBlock (TPraosMockCrypto h)) where

  type TxGenExtra (ShelleyBlock (TPraosMockCrypto h)) = ShelleyTxGenExtra h

  testGenTxs _coreNodeId _numCoreNodes curSlotNo cfg extra lst
      | stgeStartAt > curSlotNo = pure []
      | otherwise               = do
      n <- choose (0, 20)
      go [] n $ applyChainTick (configLedger cfg) curSlotNo lst
    where
      ShelleyTxGenExtra
        { stgeGenEnv
        , stgeStartAt
        } = extra

      go :: [GenTx (ShelleyBlock (TPraosMockCrypto h))]  -- ^ Accumulator
         -> Integer  -- ^ Number of txs to still produce
         -> TickedLedgerState (ShelleyBlock (TPraosMockCrypto h))
         -> Gen [GenTx (ShelleyBlock (TPraosMockCrypto h))]
      go acc 0 _  = return (reverse acc)
      go acc n st = do
        tx <- genTx cfg curSlotNo st stgeGenEnv
        case runExcept $ applyTx (configLedger cfg) curSlotNo tx st of
          -- We don't mind generating invalid transactions
          Left  _   -> go (tx:acc) (n - 1) st
          Right st' -> go (tx:acc) (n - 1) st'

genTx
  :: forall h. HashAlgorithm h
  => TopLevelConfig (ShelleyBlock (TPraosMockCrypto h))
  -> SlotNo
  -> TickedLedgerState (ShelleyBlock (TPraosMockCrypto h))
  -> Gen.GenEnv (TPraosMockCrypto h)
  -> Gen (GenTx (ShelleyBlock (TPraosMockCrypto h)))
genTx _cfg slotNo TickedShelleyLedgerState { tickedShelleyState } genEnv =
    mkShelleyTx <$> Gen.genTx
      genEnv
      ledgerEnv
      (utxoSt, dpState)
  where
    epochState :: SL.EpochState (TPraosMockCrypto h)
    epochState = SL.nesEs tickedShelleyState

    ledgerEnv :: STS.LedgerEnv
    ledgerEnv = STS.LedgerEnv {
        ledgerSlotNo   = slotNo
      , ledgerIx       = 0 -- TODO Ix
      , ledgerPp       = SL.esPp epochState
      , ledgerAccount  = SL.esAccountState epochState
      }

    utxoSt :: SL.UTxOState (TPraosMockCrypto h)
    utxoSt =
        SL._utxoState
      . SL.esLState
      $ epochState

    dpState :: SL.DPState (TPraosMockCrypto h)
    dpState =
        SL._delegationState
      . SL.esLState
      $ epochState

data WhetherToGeneratePPUs = DoNotGeneratePPUs | DoGeneratePPUs
  deriving (Show)

mkGenEnv :: forall h. HashAlgorithm h
         => WhetherToGeneratePPUs
         -> [CoreNode (TPraosMockCrypto h)]
         -> Gen.GenEnv (TPraosMockCrypto h)
mkGenEnv whetherPPUs coreNodes = Gen.GenEnv keySpace constants
  where
    -- Configuration of the transaction generator
    constants :: Gen.Constants
    constants =
        setCerts $
        setPPUs $
        Gen.defaultConstants
          { Gen.frequencyMIRCert = 0
          , Gen.genTxRetries     = 1000
                -- At time of writing, about 85 retries have been enough for
                -- any individual invocation; most need much fewer.
          }
      where
        -- Testing with certificates requires additional handling in the
        -- testing framework, because, for example, they may transfer block
        -- issuance rights from one node to another, and we must have the
        -- relevant nodes brought online at that point.
        setCerts cs = cs{ Gen.maxCertsPerTx = 0 }

        setPPUs cs = case whetherPPUs of
            DoGeneratePPUs    -> cs
            DoNotGeneratePPUs -> cs{ Gen.frequencyTxUpdates = 0 }

    keySpace :: Gen.KeySpace (TPraosMockCrypto h)
    keySpace =
      Gen.KeySpace
        (cnkiCoreNode <$> cn)
        ksGenesisDelegates
        ksStakePools
        (ksKeyPairs <> (cnkiKeyPair <$> cn))
        ksMSigScripts
      where
        cn = coreNodeKeys <$> coreNodes
        Gen.KeySpace_
          { ksKeyPairs,
            ksMSigScripts,
            ksGenesisDelegates,
            ksStakePools
          } =
            Gen.Presets.keySpace constants
