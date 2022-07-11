{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Alonzo.Serialisation.Generators where

import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Alonzo.Data (AuxiliaryData (..), BinaryData, Data (..), dataToBinaryData)
import Cardano.Ledger.Alonzo.Language
import Cardano.Ledger.Alonzo.PParams
import Cardano.Ledger.Alonzo.Rules.Utxo (UtxoPredicateFailure (..))
import Cardano.Ledger.Alonzo.Rules.Utxos
  ( FailureDescription (..),
    TagMismatchDescription (..),
    UtxosPredicateFailure (..),
  )
import Cardano.Ledger.Alonzo.Rules.Utxow (UtxowPredicateFail (..))
import Cardano.Ledger.Alonzo.Scripts
  ( CostModels (..),
    ExUnits (..),
    Prices (..),
    Script (..),
    Tag (..),
    mkCostModel,
  )
import Cardano.Ledger.Alonzo.Tx
import Cardano.Ledger.Alonzo.TxBody
  ( TxOut (..),
  )
import Cardano.Ledger.Alonzo.TxWitness
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era (Crypto, Era, ValidateScript (..))
import Cardano.Ledger.Hashes (ScriptHash)
import Cardano.Ledger.Shelley.Constraints (UsesScript, UsesValue)
import Data.Int (Int64)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text, pack)
import Numeric.Natural (Natural)
import qualified Plutus.V1.Ledger.Api as PV1
import qualified Plutus.V2.Ledger.Api as PV2
import Test.Cardano.Ledger.Alonzo.Scripts (alwaysFails, alwaysSucceeds)
import Test.Cardano.Ledger.Shelley.ConcreteCryptoTypes (Mock)
import Test.Cardano.Ledger.Shelley.Serialisation.EraIndepGenerators ()
import Test.Cardano.Ledger.ShelleyMA.Serialisation.Generators (genMintValues)
import Test.QuickCheck

instance Arbitrary (Data era) where
  arbitrary = Data <$> arbitrary

instance Arbitrary (BinaryData era) where
  arbitrary = dataToBinaryData <$> arbitrary

instance Arbitrary PV1.Data where
  arbitrary = resize 5 (sized gendata)
    where
      gendata n
        | n > 0 =
            oneof
              [ PV1.I <$> arbitrary,
                PV1.B <$> arbitrary,
                PV1.Map <$> listOf ((,) <$> gendata (n `div` 2) <*> gendata (n `div` 2)),
                PV1.Constr <$> fmap fromIntegral (arbitrary :: Gen Natural)
                  <*> listOf (gendata (n `div` 2)),
                PV1.List <$> listOf (gendata (n `div` 2))
              ]
      gendata _ = oneof [PV1.I <$> arbitrary, PV1.B <$> arbitrary]

instance
  ( UsesScript era,
    Ord (Core.Script era),
    Core.Script era ~ Script era,
    Arbitrary (Core.Script era)
  ) =>
  Arbitrary (AuxiliaryData era)
  where
  arbitrary = AuxiliaryData <$> arbitrary <*> arbitrary

instance Arbitrary Tag where
  arbitrary = elements [Spend, Mint, Cert, Rewrd]

instance Arbitrary RdmrPtr where
  arbitrary = RdmrPtr <$> arbitrary <*> arbitrary

instance Arbitrary ExUnits where
  arbitrary = ExUnits <$> genUnit <*> genUnit
    where
      genUnit = fromIntegral <$> choose (0, maxBound :: Int64)

instance (Era era) => Arbitrary (Redeemers era) where
  arbitrary = Redeemers <$> arbitrary

instance
  ( Era era,
    UsesValue era,
    Mock (Crypto era),
    Arbitrary (Core.Script era),
    Script era ~ Core.Script era,
    ValidateScript era,
    UsesScript era
  ) =>
  Arbitrary (TxWitness era)
  where
  arbitrary =
    TxWitness
      <$> arbitrary
      <*> arbitrary
      <*> genScripts
      <*> genData
      <*> arbitrary

keyBy :: Ord k => (a -> k) -> [a] -> Map k a
keyBy f xs = Map.fromList ((\x -> (f x, x)) <$> xs)

genScripts ::
  forall era.
  ( Core.Script era ~ Script era,
    ValidateScript era,
    Arbitrary (Script era)
  ) =>
  Gen (Map (ScriptHash (Crypto era)) (Core.Script era))
genScripts = keyBy (hashScript @era) <$> (arbitrary :: Gen [Core.Script era])

genData :: forall era. Era era => Gen (TxDats era)
genData = TxDats . keyBy hashData <$> arbitrary

instance
  ( Era era,
    UsesValue era,
    Mock (Crypto era),
    Arbitrary (Core.Value era)
  ) =>
  Arbitrary (TxOut era)
  where
  arbitrary =
    TxOut
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary

instance
  forall c.
  (Mock c) =>
  Arbitrary (TxBody (AlonzoEra c))
  where
  arbitrary =
    TxBody
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> genMintValues @c
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

deriving newtype instance Arbitrary IsValid

instance Mock c => Arbitrary (ValidatedTx (AlonzoEra c)) where
  arbitrary =
    ValidatedTx
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance Mock c => Arbitrary (Script (AlonzoEra c)) where
  arbitrary = do
    lang <- arbitrary -- The language is not present in the Script serialization
    frequency
      [ (1, pure (alwaysSucceeds lang 1)),
        (1, pure (alwaysFails lang 1)),
        (10, TimelockScript <$> arbitrary)
      ]

-- ==========================
--

instance Arbitrary Language where
  arbitrary = elements nonNativeLanguages

instance Arbitrary Prices where
  arbitrary = Prices <$> arbitrary <*> arbitrary

mkNullCostModel :: Set Text -> Map Text Integer
mkNullCostModel = Map.fromSet (const 0)

genCM :: Language -> Set Text -> Gen CostModel
genCM lang costModelParamNames = do
  newCMPs <- traverse (const arbitrary) (mkNullCostModel costModelParamNames)
  either (error "Corrupt cost model") pure $ mkCostModel lang newCMPs

genCostModel :: Language -> Gen (Language, CostModel)
genCostModel PlutusV1 = (PlutusV1,) <$> genCM PlutusV1 PV1.costModelParamNames
genCostModel PlutusV2 = (PlutusV2,) <$> genCM PlutusV2 PV2.costModelParamNames

instance Arbitrary CostModel where
  arbitrary = snd <$> (elements nonNativeLanguages >>= genCostModel)

instance Arbitrary CostModels where
  arbitrary = CostModels . Map.fromList <$> (sublistOf nonNativeLanguages >>= mapM genCostModel)

instance Arbitrary (PParams era) where
  arbitrary =
    PParams
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance Arbitrary (PParamsUpdate era) where
  arbitrary =
    PParams
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance Arbitrary FailureDescription where
  arbitrary = PlutusFailure <$> (pack <$> arbitrary) <*> arbitrary

instance Arbitrary TagMismatchDescription where
  arbitrary =
    oneof [pure PassedUnexpectedly, FailedUnexpectedly <$> ((:|) <$> arbitrary <*> arbitrary)]

instance Mock c => Arbitrary (UtxosPredicateFailure (AlonzoEra c)) where
  arbitrary =
    oneof
      [ ValidationTagMismatch <$> arbitrary <*> arbitrary,
        UpdateFailure <$> arbitrary
      ]

instance Mock c => Arbitrary (UtxoPredicateFailure (AlonzoEra c)) where
  arbitrary =
    oneof
      [ BadInputsUTxO <$> arbitrary,
        OutsideValidityIntervalUTxO <$> arbitrary <*> arbitrary,
        MaxTxSizeUTxO <$> arbitrary <*> arbitrary,
        pure InputSetEmptyUTxO,
        FeeTooSmallUTxO <$> arbitrary <*> arbitrary,
        ValueNotConservedUTxO <$> arbitrary <*> arbitrary,
        OutputTooSmallUTxO <$> arbitrary,
        UtxosFailure <$> arbitrary,
        WrongNetwork <$> arbitrary <*> arbitrary,
        WrongNetworkWithdrawal <$> arbitrary <*> arbitrary,
        OutputBootAddrAttrsTooBig <$> arbitrary,
        pure TriesToForgeADA,
        OutputTooBigUTxO <$> arbitrary,
        InsufficientCollateral <$> arbitrary <*> arbitrary,
        ScriptsNotPaidUTxO <$> arbitrary,
        ExUnitsTooBigUTxO <$> arbitrary <*> arbitrary,
        CollateralContainsNonADA <$> arbitrary
      ]

instance Mock c => Arbitrary (UtxowPredicateFail (AlonzoEra c)) where
  arbitrary =
    oneof
      [ WrappedShelleyEraFailure <$> arbitrary,
        MissingRedeemers <$> arbitrary,
        MissingRequiredDatums <$> arbitrary <*> arbitrary,
        PPViewHashesDontMatch <$> arbitrary <*> arbitrary
      ]

instance Mock c => Arbitrary (ScriptPurpose c) where
  arbitrary =
    oneof
      [ Minting <$> arbitrary,
        Spending <$> arbitrary,
        Rewarding <$> arbitrary,
        Certifying <$> arbitrary
      ]

instance Mock c => Arbitrary (ScriptIntegrity (AlonzoEra c)) where
  arbitrary =
    ScriptIntegrity
      <$> arbitrary
      <*> genData
      <*> (Set.singleton <$> (getLanguageView @(AlonzoEra c) <$> arbitrary <*> arbitrary))
