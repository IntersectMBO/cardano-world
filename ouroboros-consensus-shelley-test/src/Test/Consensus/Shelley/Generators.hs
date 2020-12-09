{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -Wno-orphans #-}
module Test.Consensus.Shelley.Generators (
    SomeResult (..)
  ) where

import           Ouroboros.Network.Block (mkSerialised)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.SupportsMempool

import qualified Shelley.Spec.Ledger.API as SL

import           Ouroboros.Consensus.Shelley.Ledger
import           Ouroboros.Consensus.Shelley.Protocol (PraosCrypto,
                     TPraosState (..))

import           Generic.Random (genericArbitraryU)
import           Test.QuickCheck hiding (Result)

import           Test.Util.Orphans.Arbitrary ()
import           Test.Util.Serialisation.Roundtrip (SomeResult (..),
                     WithVersion (..))

import           Test.Cardano.Ledger.Allegra ()
import           Test.Cardano.Ledger.Mary ()
import           Test.Cardano.Ledger.ShelleyMA.Serialisation.Generators ()
import           Test.Consensus.Shelley.MockCrypto (CanMock)
import           Test.Shelley.Spec.Ledger.ConcreteCryptoTypes as SL
import           Test.Shelley.Spec.Ledger.Generator.ShelleyEraGen ()
import           Test.Shelley.Spec.Ledger.Serialisation.EraIndepGenerators
                     (genPParams)
import           Test.Shelley.Spec.Ledger.Serialisation.Generators ()

{-------------------------------------------------------------------------------
  Generators

  These are generators for roundtrip tests, so the generated values are not
  necessarily valid
-------------------------------------------------------------------------------}

instance CanMock era => Arbitrary (ShelleyBlock era) where
  arbitrary = mkShelleyBlock <$> arbitrary

instance CanMock era => Arbitrary (Header (ShelleyBlock era)) where
  arbitrary = getHeader <$> arbitrary

instance SL.Mock c => Arbitrary (ShelleyHash c) where
  arbitrary = ShelleyHash <$> arbitrary

instance CanMock era => Arbitrary (GenTx (ShelleyBlock era)) where
  arbitrary = mkShelleyTx <$> arbitrary

instance CanMock era => Arbitrary (GenTxId (ShelleyBlock era)) where
  arbitrary = ShelleyTxId <$> arbitrary

instance CanMock era => Arbitrary (SomeSecond Query (ShelleyBlock era)) where
  arbitrary = oneof
    [ pure $ SomeSecond GetLedgerTip
    , pure $ SomeSecond GetEpochNo
    , SomeSecond . GetNonMyopicMemberRewards <$> arbitrary
    , pure $ SomeSecond GetCurrentPParams
    , pure $ SomeSecond GetProposedPParamsUpdates
    , pure $ SomeSecond GetStakeDistribution
    , pure $ SomeSecond DebugEpochState
    , (\(SomeSecond q) -> SomeSecond (GetCBOR q)) <$> arbitrary
    , SomeSecond . GetFilteredDelegationsAndRewardAccounts <$> arbitrary
    , pure $ SomeSecond GetGenesisConfig
    , pure $ SomeSecond DebugNewEpochState
    ]

instance CanMock era => Arbitrary (SomeResult (ShelleyBlock era)) where
  arbitrary = oneof
    [ SomeResult GetLedgerTip <$> arbitrary
    , SomeResult GetEpochNo <$> arbitrary
    , SomeResult <$> (GetNonMyopicMemberRewards <$> arbitrary) <*> arbitrary
    , SomeResult GetCurrentPParams <$> genPParams (Proxy @era)
    , SomeResult GetProposedPParamsUpdates <$> arbitrary
    , SomeResult GetStakeDistribution <$> arbitrary
    , SomeResult DebugEpochState <$> arbitrary
    , (\(SomeResult q r) ->
        SomeResult (GetCBOR q) (mkSerialised (encodeShelleyResult q) r)) <$>
      arbitrary
    , SomeResult <$> (GetFilteredDelegationsAndRewardAccounts <$> arbitrary) <*> arbitrary
    , SomeResult GetGenesisConfig . compactGenesis <$> arbitrary
    , SomeResult DebugNewEpochState <$> arbitrary
    ]

instance PraosCrypto c => Arbitrary (NonMyopicMemberRewards c) where
  arbitrary = NonMyopicMemberRewards <$> arbitrary

instance CanMock era => Arbitrary (Point (ShelleyBlock era)) where
  arbitrary = BlockPoint <$> arbitrary <*> arbitrary

instance PraosCrypto c => Arbitrary (TPraosState c) where
  arbitrary = do
      lastSlot <- frequency
        [ (1, return Origin)
        , (5, NotOrigin . SlotNo <$> choose (0, 100))
        ]
      TPraosState lastSlot <$> arbitrary

instance CanMock era => Arbitrary (ShelleyTip era) where
  arbitrary = ShelleyTip
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary

instance Arbitrary ShelleyTransition where
  arbitrary = ShelleyTransitionInfo <$> arbitrary

instance CanMock era => Arbitrary (LedgerState (ShelleyBlock era)) where
  arbitrary = ShelleyLedgerState
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary

instance CanMock era => Arbitrary (AnnTip (ShelleyBlock era)) where
  arbitrary = AnnTip
    <$> arbitrary
    <*> (BlockNo <$> arbitrary)
    <*> arbitrary

instance Arbitrary ShelleyNodeToNodeVersion where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary ShelleyNodeToClientVersion where
  arbitrary = arbitraryBoundedEnum

instance ShelleyBasedEra era
      => Arbitrary (SomeSecond (NestedCtxt f) (ShelleyBlock era)) where
  arbitrary = return (SomeSecond indexIsTrivial)

{-------------------------------------------------------------------------------
  Generators for cardano-ledger-specs
-------------------------------------------------------------------------------}

instance Arbitrary (SL.PParams' SL.StrictMaybe era) where
  arbitrary = genericArbitraryU
  shrink    = genericShrink

instance PraosCrypto c => Arbitrary (SL.ChainDepState c) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

{-------------------------------------------------------------------------------
  Versioned generators for serialisation
-------------------------------------------------------------------------------}

-- | We only have single version, so no special casing required.
--
-- This blanket orphan instance will have to be replaced with more specific
-- ones, once we introduce a different Shelley version.
instance Arbitrary a => Arbitrary (WithVersion ShelleyNodeToNodeVersion a) where
  arbitrary = WithVersion <$> arbitrary <*> arbitrary

-- | This is @OVERLAPPABLE@ because we have to override the default behaviour
-- for 'Query's.
instance {-# OVERLAPPABLE #-} Arbitrary a
      => Arbitrary (WithVersion ShelleyNodeToClientVersion a) where
  arbitrary = WithVersion <$> arbitrary <*> arbitrary

-- | Some 'Query's are only supported by 'ShelleyNodeToClientVersion2', so we
-- make sure to not generate those queries in combination with
-- 'ShelleyNodeToClientVersion1'.
instance CanMock era
      => Arbitrary (WithVersion ShelleyNodeToClientVersion (SomeSecond Query (ShelleyBlock era))) where
  arbitrary = do
      query@(SomeSecond q) <- arbitrary
      version <- arbitrary `suchThat` querySupportedVersion q
      return $ WithVersion version query
