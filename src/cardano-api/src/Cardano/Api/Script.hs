{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Api.Script (
    -- * Languages
    SimpleScriptV1,
    SimpleScriptV2,
    ScriptLanguage(..),
    SimpleScriptVersion(..),
    PlutusScriptVersion,
    AnyScriptLanguage(..),
    IsScriptLanguage(..),
    IsSimpleScriptLanguage(..),

    -- * Scripts in a specific language
    Script(..),

    -- * Scripts in any language
    ScriptInAnyLang(..),
    toScriptInAnyLang,

    -- * Scripts in an era
    ScriptInEra(..),
    toScriptInEra,
    eraOfScriptInEra,

    -- ** Languages supported in each era
    ScriptLanguageInEra(..),
    scriptLanguageSupportedInEra,
    languageOfScriptLanguageInEra,
    eraOfScriptLanguageInEra,

    -- * The simple script language
    SimpleScript(..),
    TimeLocksSupported(..),
    timeLocksSupported,
    adjustSimpleScriptVersion,

    -- * Script hashes
    ScriptHash(..),
    hashScript,

    -- * Internal conversion functions
    toShelleyScript,
    toShelleyMultiSig,
    fromShelleyMultiSig,
    toAllegraTimelock,
    fromAllegraTimelock,
    toShelleyScriptHash,
    fromShelleyScriptHash,

    -- * Data family instances
    AsType(..)
  ) where

import           Prelude

import qualified Data.ByteString.Lazy as LBS
import           Data.Foldable (toList)
import           Data.Scientific (toBoundedInteger)
import           Data.String (IsString)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Data.Type.Equality (TestEquality (..), (:~:) (Refl))

import           Data.Aeson (Value (..), object, (.:), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Sequence.Strict as Seq
import           Data.Vector (Vector)
import qualified Data.Vector as Vector

import           Control.Applicative
import           Control.Monad

import qualified Cardano.Binary as CBOR
import qualified Cardano.Prelude as CBOR (cborError)

import qualified Cardano.Crypto.Hash.Class as Crypto

import           Cardano.Slotting.Slot (SlotNo)

import qualified Cardano.Ledger.Core as Ledger

import qualified Cardano.Ledger.ShelleyMA.Timelocks as Timelock
import           Ouroboros.Consensus.Shelley.Eras (StandardCrypto)
import qualified Shelley.Spec.Ledger.Keys as Shelley
import qualified Shelley.Spec.Ledger.Scripts as Shelley

import           Cardano.Api.Eras
import           Cardano.Api.HasTypeProxy
import           Cardano.Api.Hash
import           Cardano.Api.KeysShelley
import           Cardano.Api.SerialiseCBOR
import           Cardano.Api.SerialiseJSON
import           Cardano.Api.SerialiseRaw
import           Cardano.Api.SerialiseTextEnvelope

{- HLINT ignore "Use section" -}


-- ----------------------------------------------------------------------------
-- Types for script language and version
--

-- | The original simple script language which supports
--
-- * require a signature from a given key (by verification key hash)
-- * n-way and combinator
-- * n-way or combinator
-- * m-of-n combinator
--
-- This version of the language was introduced in the 'ShelleyEra'.
--
data SimpleScriptV1

-- | The second version of the simple script language. It has all the features
-- of 'SimpleScriptV1' plus new atomic predicates:
--
-- * require the time be before a given slot number
-- * require the time be after a given slot number
--
-- This version of the language was introduced in the 'AllegraEra'.
--
data SimpleScriptV2

-- | Place holder type to show what the pattern is to extend to multiple
-- languages, not just multiple versions of a single language.
--
data PlutusScriptV1

instance HasTypeProxy SimpleScriptV1 where
    data AsType SimpleScriptV1 = AsSimpleScriptV1
    proxyToAsType _ = AsSimpleScriptV1

instance HasTypeProxy SimpleScriptV2 where
    data AsType SimpleScriptV2 = AsSimpleScriptV2
    proxyToAsType _ = AsSimpleScriptV2

instance HasTypeProxy PlutusScriptV1 where
    data AsType PlutusScriptV1 = AsPlutusScriptV1
    proxyToAsType _ = AsPlutusScriptV1


-- ----------------------------------------------------------------------------
-- Value level representation for script languages
--
data ScriptLanguage lang where

     SimpleScriptLanguage :: SimpleScriptVersion lang -> ScriptLanguage lang

     PlutusScriptLanguage :: PlutusScriptVersion lang -> ScriptLanguage lang

deriving instance (Eq   (ScriptLanguage lang))
deriving instance (Show (ScriptLanguage lang))

instance TestEquality ScriptLanguage where
    testEquality (SimpleScriptLanguage lang)
                 (SimpleScriptLanguage lang') = testEquality lang lang'

    testEquality (PlutusScriptLanguage lang)
                 (PlutusScriptLanguage lang') = testEquality lang lang'

    testEquality  _ _ = Nothing


data SimpleScriptVersion lang where

     SimpleScriptV1 :: SimpleScriptVersion SimpleScriptV1
     SimpleScriptV2 :: SimpleScriptVersion SimpleScriptV2

deriving instance (Eq   (SimpleScriptVersion lang))
deriving instance (Show (SimpleScriptVersion lang))

instance TestEquality SimpleScriptVersion where
    testEquality SimpleScriptV1 SimpleScriptV1 = Just Refl
    testEquality SimpleScriptV2 SimpleScriptV2 = Just Refl
    testEquality _              _              = Nothing


data PlutusScriptVersion lang
  -- For now, there are no such versions, but it'd be like this:
  -- PlutusScriptV1 :: PlutusScriptVersion PlutusScriptV1

deriving instance (Eq   (PlutusScriptVersion lang))
deriving instance (Show (PlutusScriptVersion lang))

instance TestEquality PlutusScriptVersion where
    testEquality lang = case lang of {}


data AnyScriptLanguage where
     AnyScriptLanguage :: ScriptLanguage lang -> AnyScriptLanguage

deriving instance (Show AnyScriptLanguage)

instance Eq AnyScriptLanguage where
    AnyScriptLanguage lang == AnyScriptLanguage lang' =
      case testEquality lang lang' of
        Nothing   -> False
        Just Refl -> True -- since no constructors share types

instance Enum AnyScriptLanguage where
    toEnum 0 = AnyScriptLanguage (SimpleScriptLanguage SimpleScriptV1)
    toEnum 1 = AnyScriptLanguage (SimpleScriptLanguage SimpleScriptV2)
    toEnum _ = error "AnyScriptLanguage.toEnum: bad argument"

    fromEnum (AnyScriptLanguage (SimpleScriptLanguage SimpleScriptV1)) = 0
    fromEnum (AnyScriptLanguage (SimpleScriptLanguage SimpleScriptV2)) = 1
    fromEnum (AnyScriptLanguage (PlutusScriptLanguage lang)) = case lang of {}

instance Bounded AnyScriptLanguage where
    minBound = AnyScriptLanguage (SimpleScriptLanguage SimpleScriptV1)
    maxBound = AnyScriptLanguage (SimpleScriptLanguage SimpleScriptV2)


class HasTypeProxy lang => IsScriptLanguage lang where
    scriptLanguage :: ScriptLanguage lang

instance IsScriptLanguage SimpleScriptV1 where
    scriptLanguage = SimpleScriptLanguage SimpleScriptV1

instance IsScriptLanguage SimpleScriptV2 where
    scriptLanguage = SimpleScriptLanguage SimpleScriptV2

--instance IsScriptLanguage PlutusScriptV1 where
--    scriptLanguage = PlutusScriptLanguage PlutusScriptV1


class IsScriptLanguage lang => IsSimpleScriptLanguage lang where
    simpleScriptVersion :: SimpleScriptVersion lang

instance IsSimpleScriptLanguage SimpleScriptV1 where
    simpleScriptVersion = SimpleScriptV1

instance IsSimpleScriptLanguage SimpleScriptV2 where
    simpleScriptVersion = SimpleScriptV2



-- ----------------------------------------------------------------------------
-- Script type: covering all script languages
--

-- | A script in a particular language.
--
-- See also 'ScriptInAnyLang' for a script in any of the languages that is available within
-- a particular era.
--
-- See also 'ScriptInEra' for a script in a language that is available within
-- a particular era.
--
data Script lang where

     SimpleScript :: !(SimpleScriptVersion lang)
                  -> !(SimpleScript lang)
                  -> Script lang

     -- Place holder type to show what the pattern is to extend to multiple
     -- languages, not just multiple versions of a single language.
     -- For now there are no values of PlutusScriptVersion so this branch
     -- is inaccessible.
     PlutusScript :: !(PlutusScriptVersion lang)
                  -> ()
                  -> Script lang

deriving instance (Eq   (Script lang))
deriving instance (Show (Script lang))

instance HasTypeProxy lang => HasTypeProxy (Script lang) where
    data AsType (Script lang) = AsScript (AsType lang)
    proxyToAsType _ = AsScript (proxyToAsType (Proxy :: Proxy lang))

instance IsScriptLanguage lang => SerialiseAsCBOR (Script lang) where
    serialiseToCBOR (SimpleScript SimpleScriptV1 s) =
      CBOR.serialize' (toShelleyMultiSig s)

    serialiseToCBOR (SimpleScript SimpleScriptV2 s) =
      CBOR.serialize' (toAllegraTimelock s :: Timelock.Timelock StandardCrypto)

    deserialiseFromCBOR _ bs =
      case scriptLanguage :: ScriptLanguage lang of
        SimpleScriptLanguage SimpleScriptV1 ->
              SimpleScript SimpleScriptV1
            . fromShelleyMultiSig
          <$> CBOR.decodeAnnotator "Script" fromCBOR (LBS.fromStrict bs)

        SimpleScriptLanguage SimpleScriptV2 ->
              SimpleScript SimpleScriptV2
            . (fromAllegraTimelock TimeLocksInSimpleScriptV2
                                :: Timelock.Timelock StandardCrypto
                                -> SimpleScript SimpleScriptV2)
          <$> CBOR.decodeAnnotator "Script" fromCBOR (LBS.fromStrict bs)

        PlutusScriptLanguage v -> case v of {}


instance IsScriptLanguage lang => HasTextEnvelope (Script lang) where
    textEnvelopeType _ =
      case scriptLanguage :: ScriptLanguage lang of
        SimpleScriptLanguage SimpleScriptV1 -> "SimpleScriptV1"
        SimpleScriptLanguage SimpleScriptV2 -> "SimpleScriptV2"
        PlutusScriptLanguage v -> case v of {}


-- ----------------------------------------------------------------------------
-- Scripts in any language
--

-- | Sometimes it is necessary to handle all languages without making static
-- type distinctions between languages. For example, when reading external
-- input, or before the era context is known.
--
-- Use 'toScriptInEra' to convert to a script in the context of an era.
--
data ScriptInAnyLang where
     ScriptInAnyLang :: ScriptLanguage lang
                     -> Script lang
                     -> ScriptInAnyLang

deriving instance Show ScriptInAnyLang

-- The GADT in the ScriptInAnyLang constructor requires a custom Eq instance
instance Eq ScriptInAnyLang where
    (==) (ScriptInAnyLang lang  script)
         (ScriptInAnyLang lang' script') =
      case testEquality lang lang' of
        Nothing   -> False
        Just Refl -> script == script'


-- | Convert a script in a specific statically-known language to a
-- 'ScriptInAnyLang'.
--
-- No inverse to this is provided, just do case analysis on the 'ScriptLanguage'
-- field within the 'ScriptInAnyLang' constructor.
--
toScriptInAnyLang :: Script lang -> ScriptInAnyLang
toScriptInAnyLang s@(SimpleScript v _) =
    ScriptInAnyLang (SimpleScriptLanguage v) s

instance HasTypeProxy ScriptInAnyLang where
    data AsType ScriptInAnyLang = AsScriptInAnyLang
    proxyToAsType _ = AsScriptInAnyLang

instance SerialiseAsCBOR ScriptInAnyLang where

    serialiseToCBOR (ScriptInAnyLang (SimpleScriptLanguage SimpleScriptV1)
                                     (SimpleScript _v s)) =
      -- Note that the CBOR encoding here is compatible with the previous
      -- serialisation format for the @Script@ type from @cardano-ledger-specs@.
      --
      CBOR.serializeEncoding' $
          CBOR.encodeListLen 2
       <> CBOR.encodeWord 0
       <> toCBOR (toShelleyMultiSig s)

    serialiseToCBOR (ScriptInAnyLang (SimpleScriptLanguage SimpleScriptV2)
                                     (SimpleScript _v s)) =
      CBOR.serializeEncoding' $
          CBOR.encodeListLen 2
       <> CBOR.encodeWord 1
       <> toCBOR (toAllegraTimelock s :: Timelock.Timelock StandardCrypto)

    serialiseToCBOR (ScriptInAnyLang (PlutusScriptLanguage v) _) = case v of {}

    deserialiseFromCBOR AsScriptInAnyLang bs =
        CBOR.decodeAnnotator "Script" decodeScript (LBS.fromStrict bs)
      where
        decodeScript :: CBOR.Decoder s (CBOR.Annotator ScriptInAnyLang)
        decodeScript = do
          CBOR.decodeListLenOf 2
          tag <- CBOR.decodeWord8

          case tag of
            0 -> fmap (fmap convert) fromCBOR
              where
                convert :: Shelley.MultiSig StandardCrypto -> ScriptInAnyLang
                convert = ScriptInAnyLang (SimpleScriptLanguage SimpleScriptV1)
                        . SimpleScript SimpleScriptV1
                        . fromShelleyMultiSig

            1 -> fmap (fmap convert) fromCBOR
              where
                convert :: Timelock.Timelock StandardCrypto -> ScriptInAnyLang
                convert = ScriptInAnyLang (SimpleScriptLanguage SimpleScriptV2)
                        . SimpleScript SimpleScriptV2
                        . fromAllegraTimelock TimeLocksInSimpleScriptV2

            _ -> CBOR.cborError $ CBOR.DecoderErrorUnknownTag "Script" tag

instance HasTextEnvelope ScriptInAnyLang where
    textEnvelopeType _ = "Script"


-- ----------------------------------------------------------------------------
-- Scripts in the context of a ledger era
--

data ScriptInEra era where
     ScriptInEra :: ScriptLanguageInEra lang era
                 -> Script lang
                 -> ScriptInEra era

deriving instance Show (ScriptInEra era)

-- The GADT in the ScriptInEra constructor requires a custom instance
instance Eq (ScriptInEra era) where
    (==) (ScriptInEra langInEra  script)
         (ScriptInEra langInEra' script') =
      case testEquality (languageOfScriptLanguageInEra langInEra)
                        (languageOfScriptLanguageInEra langInEra') of
        Nothing   -> False
        Just Refl -> script == script'


data ScriptLanguageInEra lang era where

     SimpleScriptV1InShelley :: ScriptLanguageInEra SimpleScriptV1 ShelleyEra
     SimpleScriptV1InAllegra :: ScriptLanguageInEra SimpleScriptV1 AllegraEra
     SimpleScriptV1InMary    :: ScriptLanguageInEra SimpleScriptV1 MaryEra

     SimpleScriptV2InAllegra :: ScriptLanguageInEra SimpleScriptV2 AllegraEra
     SimpleScriptV2InMary    :: ScriptLanguageInEra SimpleScriptV2 MaryEra

deriving instance Eq   (ScriptLanguageInEra lang era)
deriving instance Show (ScriptLanguageInEra lang era)

instance HasTypeProxy era => HasTypeProxy (ScriptInEra era) where
    data AsType (ScriptInEra era) = AsScriptInEra (AsType era)
    proxyToAsType _ = AsScriptInEra (proxyToAsType (Proxy :: Proxy era))

instance IsCardanoEra era => SerialiseAsCBOR (ScriptInEra era) where
    serialiseToCBOR (ScriptInEra _lang s) =
      serialiseToCBOR (toScriptInAnyLang s)

    deserialiseFromCBOR (AsScriptInEra _) bs = do
      s@(ScriptInAnyLang lang _) <- deserialiseFromCBOR AsScriptInAnyLang bs
      case toScriptInEra cardanoEra s of
        Just s' -> Right s'
        Nothing ->
          Left $ CBOR.DecoderErrorCustom
                 (Text.pack (show (cardanoEra :: CardanoEra era)) <> " Script")
                 ("Script language " <> Text.pack (show lang) <>
                  " not supported in this era")

instance IsShelleyBasedEra era => HasTextEnvelope (ScriptInEra era) where
    textEnvelopeType _ =
      case shelleyBasedEra :: ShelleyBasedEra era of
        ShelleyBasedEraShelley -> "ScriptInEra ShelleyEra"
        ShelleyBasedEraAllegra -> "ScriptInEra AllegraEra"
        ShelleyBasedEraMary    -> "ScriptInEra MaryEra"


-- | Check if a given script language is supported in a given era, and if so
-- return the evidence.
--
scriptLanguageSupportedInEra :: CardanoEra era
                             -> ScriptLanguage lang
                             -> Maybe (ScriptLanguageInEra lang era)
scriptLanguageSupportedInEra era lang =
    case (era, lang) of
      (ShelleyEra, SimpleScriptLanguage SimpleScriptV1) ->
        Just SimpleScriptV1InShelley

      (AllegraEra, SimpleScriptLanguage SimpleScriptV1) ->
        Just SimpleScriptV1InAllegra

      (MaryEra, SimpleScriptLanguage SimpleScriptV1) ->
        Just SimpleScriptV1InMary

      (AllegraEra, SimpleScriptLanguage SimpleScriptV2) ->
        Just SimpleScriptV2InAllegra

      (MaryEra, SimpleScriptLanguage SimpleScriptV2) ->
        Just SimpleScriptV2InMary

      _ -> Nothing

languageOfScriptLanguageInEra :: ScriptLanguageInEra lang era
                              -> ScriptLanguage lang
languageOfScriptLanguageInEra langInEra =
    case langInEra of
      SimpleScriptV1InShelley -> SimpleScriptLanguage SimpleScriptV1
      SimpleScriptV1InAllegra -> SimpleScriptLanguage SimpleScriptV1
      SimpleScriptV1InMary    -> SimpleScriptLanguage SimpleScriptV1

      SimpleScriptV2InAllegra -> SimpleScriptLanguage SimpleScriptV2
      SimpleScriptV2InMary    -> SimpleScriptLanguage SimpleScriptV2

eraOfScriptLanguageInEra :: ScriptLanguageInEra lang era
                         -> ShelleyBasedEra era
eraOfScriptLanguageInEra langInEra =
    case langInEra of
      SimpleScriptV1InShelley -> ShelleyBasedEraShelley

      SimpleScriptV1InAllegra -> ShelleyBasedEraAllegra
      SimpleScriptV2InAllegra -> ShelleyBasedEraAllegra

      SimpleScriptV1InMary    -> ShelleyBasedEraMary
      SimpleScriptV2InMary    -> ShelleyBasedEraMary


-- | Given a target era and a script in some language, check if the language is
-- supported in that era, and if so return a 'ScriptInEra'.
--
toScriptInEra :: CardanoEra era -> ScriptInAnyLang -> Maybe (ScriptInEra era)
toScriptInEra era (ScriptInAnyLang lang s) = do
    lang' <- scriptLanguageSupportedInEra era lang
    return (ScriptInEra lang' s)

eraOfScriptInEra :: ScriptInEra era -> ShelleyBasedEra era
eraOfScriptInEra (ScriptInEra langInEra _) = eraOfScriptLanguageInEra langInEra


-- ----------------------------------------------------------------------------
-- Script Hash
--

-- | We have this type separate from the 'Hash' type to avoid the script
-- hash type being parametrised by the era. The representation is era
-- independent, and there are many places where we want to use a script
-- hash where we don't want things to be era-parametrised.
--
newtype ScriptHash = ScriptHash (Shelley.ScriptHash StandardCrypto)
  deriving stock (Eq, Ord)
  deriving (Show, IsString) via UsingRawBytesHex ScriptHash

instance HasTypeProxy ScriptHash where
    data AsType ScriptHash = AsScriptHash
    proxyToAsType _ = AsScriptHash

instance SerialiseAsRawBytes ScriptHash where
    serialiseToRawBytes (ScriptHash (Shelley.ScriptHash h)) =
      Crypto.hashToBytes h

    deserialiseFromRawBytes AsScriptHash bs =
      ScriptHash . Shelley.ScriptHash <$> Crypto.hashFromBytes bs


hashScript :: Script lang -> ScriptHash
hashScript (SimpleScript SimpleScriptV1 s) =
    -- For V1, we convert to the Shelley-era version specifically and hash that.
    -- Later ledger eras have to be compatible anyway.
    ScriptHash
  . Shelley.hashMultiSigScript
  . toShelleyMultiSig
  $ s

hashScript (SimpleScript SimpleScriptV2 s) =
    -- For V1, we convert to the Allegra-era version specifically and hash that.
    -- Later ledger eras have to be compatible anyway.
    ScriptHash
  . Timelock.hashTimelockScript
  . (toAllegraTimelock :: SimpleScript SimpleScriptV2
                       -> Timelock.Timelock StandardCrypto)
  $ s


toShelleyScriptHash :: ScriptHash -> Shelley.ScriptHash StandardCrypto
toShelleyScriptHash (ScriptHash h) =  h

fromShelleyScriptHash :: Shelley.ScriptHash StandardCrypto -> ScriptHash
fromShelleyScriptHash = ScriptHash


-- ----------------------------------------------------------------------------
-- The simple native script language
--

data SimpleScript lang where

     RequireSignature  :: !(Hash PaymentKey)
                       -> SimpleScript lang

     RequireTimeBefore :: !(TimeLocksSupported lang)
                       -> !SlotNo
                       -> SimpleScript lang

     RequireTimeAfter  :: !(TimeLocksSupported lang)
                       -> !SlotNo
                       -> SimpleScript lang

     RequireAllOf      ::        [SimpleScript lang] -> SimpleScript lang
     RequireAnyOf      ::        [SimpleScript lang] -> SimpleScript lang
     RequireMOf        :: Int -> [SimpleScript lang] -> SimpleScript lang

deriving instance Eq   (SimpleScript lang)
deriving instance Show (SimpleScript lang)


-- | Time lock feature in the 'SimpleScript' language.
--
-- The constructors of this type serve as evidence that the timelocks feature
-- is supported in particular versions of the language.
--
data TimeLocksSupported lang where
     TimeLocksInSimpleScriptV2 :: TimeLocksSupported SimpleScriptV2

deriving instance Eq   (TimeLocksSupported lang)
deriving instance Show (TimeLocksSupported lang)

timeLocksSupported :: SimpleScriptVersion lang
                   -> Maybe (TimeLocksSupported lang)
timeLocksSupported SimpleScriptV1 = Nothing
timeLocksSupported SimpleScriptV2 = Just TimeLocksInSimpleScriptV2


-- | Try converting the 'SimpleScript' into a different version of the language.
--
-- This will work when the script only uses the features of the target language
-- version. For example converting from 'SimpleScriptV2' to 'SimpleScriptV1'
-- will work if the script happens not to use time locks feature. On the other
-- hand converting 'SimpleScriptV1' to 'SimpleScriptV2' will always work because
-- it is backwards compatible.
--
adjustSimpleScriptVersion :: SimpleScriptVersion lang'
                          -> SimpleScript lang
                          -> Maybe (SimpleScript lang')
adjustSimpleScriptVersion target = go
  where
    go (RequireSignature sig) = pure (RequireSignature sig)

    go (RequireTimeBefore _ slot) = do
      supported <- timeLocksSupported target
      pure (RequireTimeBefore supported slot)

    go (RequireTimeAfter _ slot) = do
      supported <- timeLocksSupported target
      pure (RequireTimeAfter supported slot)

    go (RequireAllOf ss) = RequireAllOf <$> traverse go ss
    go (RequireAnyOf ss) = RequireAnyOf <$> traverse go ss
    go (RequireMOf m ss) = RequireMOf m <$> traverse go ss


-- ----------------------------------------------------------------------------
-- Conversion functions
--

toShelleyScript :: ScriptInEra era -> Ledger.Script (ShelleyLedgerEra era)
toShelleyScript (ScriptInEra langInEra (SimpleScript _ script)) =
    case langInEra of
      SimpleScriptV1InShelley -> toShelleyMultiSig script

      SimpleScriptV1InAllegra -> toAllegraTimelock script
      SimpleScriptV1InMary    -> toAllegraTimelock script
      SimpleScriptV2InAllegra -> toAllegraTimelock script
      SimpleScriptV2InMary    -> toAllegraTimelock script


-- | Conversion for the 'Shelley.MultiSig' language used by the Shelley era.
--
toShelleyMultiSig :: SimpleScript SimpleScriptV1
                  -> Shelley.MultiSig StandardCrypto
toShelleyMultiSig = go
  where
    go :: SimpleScript SimpleScriptV1 -> Shelley.MultiSig StandardCrypto
    go (RequireSignature (PaymentKeyHash kh))
                        = Shelley.RequireSignature (Shelley.coerceKeyRole kh)
    go (RequireAllOf s) = Shelley.RequireAllOf (map go s)
    go (RequireAnyOf s) = Shelley.RequireAnyOf (map go s)
    go (RequireMOf m s) = Shelley.RequireMOf m (map go s)

-- | Conversion for the 'Shelley.MultiSig' language used by the Shelley era.
--
fromShelleyMultiSig :: Shelley.MultiSig StandardCrypto -> SimpleScript lang
fromShelleyMultiSig = go
  where
    go (Shelley.RequireSignature kh)
                                = RequireSignature
                                    (PaymentKeyHash (Shelley.coerceKeyRole kh))
    go (Shelley.RequireAllOf s) = RequireAllOf (map go s)
    go (Shelley.RequireAnyOf s) = RequireAnyOf (map go s)
    go (Shelley.RequireMOf m s) = RequireMOf m (map go s)

-- | Conversion for the 'Timelock.Timelock' language that is shared between the
-- Allegra and Mary eras.
--
toAllegraTimelock :: forall lang.
                     SimpleScript lang -> Timelock.Timelock StandardCrypto
toAllegraTimelock = go
  where
    go :: SimpleScript lang -> Timelock.Timelock StandardCrypto
    go (RequireSignature (PaymentKeyHash kh))
                        = Timelock.RequireSignature (Shelley.coerceKeyRole kh)
    go (RequireAllOf s) = Timelock.RequireAllOf (Seq.fromList (map go s))
    go (RequireAnyOf s) = Timelock.RequireAnyOf (Seq.fromList (map go s))
    go (RequireMOf m s) = Timelock.RequireMOf m (Seq.fromList (map go s))
    go (RequireTimeBefore _ t) = Timelock.RequireTimeExpire t
    go (RequireTimeAfter  _ t) = Timelock.RequireTimeStart  t

-- | Conversion for the 'Timelock.Timelock' language that is shared between the
-- Allegra and Mary eras.
--
fromAllegraTimelock :: TimeLocksSupported lang
                    -> Timelock.Timelock StandardCrypto
                    -> SimpleScript lang
fromAllegraTimelock timelocks = go
  where
    go (Timelock.RequireSignature kh) = RequireSignature
                                          (PaymentKeyHash (Shelley.coerceKeyRole kh))
    go (Timelock.RequireTimeExpire t) = RequireTimeBefore timelocks t
    go (Timelock.RequireTimeStart  t) = RequireTimeAfter  timelocks t
    go (Timelock.RequireAllOf      s) = RequireAllOf (map go (toList s))
    go (Timelock.RequireAnyOf      s) = RequireAnyOf (map go (toList s))
    go (Timelock.RequireMOf      i s) = RequireMOf i (map go (toList s))


-- ----------------------------------------------------------------------------
-- JSON serialisation
--

instance ToJSON (Script lang) where
  toJSON (SimpleScript _ script) = toJSON script

instance ToJSON ScriptInAnyLang where
  toJSON (ScriptInAnyLang _ script) = toJSON script

instance ToJSON (ScriptInEra era) where
  toJSON (ScriptInEra _ script) = toJSON script

instance ToJSON (SimpleScript lang) where
  toJSON (RequireSignature pKeyHash) =
    object [ "type"    .= String "sig"
           , "keyHash" .= serialiseToRawBytesHexText pKeyHash
           ]
  toJSON (RequireTimeBefore _ slot) =
    object [ "type" .= String "before"
           , "slot" .= slot
           ]
  toJSON (RequireTimeAfter _ slot) =
    object [ "type" .= String "after"
           , "slot" .= slot
           ]
  toJSON (RequireAnyOf reqScripts) =
    object [ "type" .= String "any", "scripts" .= map toJSON reqScripts ]
  toJSON (RequireAllOf reqScripts) =
    object [ "type" .= String "all", "scripts" .= map toJSON reqScripts ]
  toJSON (RequireMOf reqNum reqScripts) =
    object [ "type" .= String "atLeast"
           , "required" .= reqNum
           , "scripts" .= map toJSON reqScripts
           ]


instance IsScriptLanguage lang => FromJSON (Script lang) where
  parseJSON v =
    case scriptLanguage :: ScriptLanguage lang of
      SimpleScriptLanguage lang -> SimpleScript lang <$>
                                     parseSimpleScript lang v
      PlutusScriptLanguage lang -> case lang of {}


instance FromJSON ScriptInAnyLang where
  parseJSON v =
      -- The SimpleScript language has the property that it is backwards
      -- compatible, so we can parse as the latest version and then downgrade
      -- to the minimum version that has all the features actually used.
      toMinimumSimpleScriptVersion <$> parseSimpleScript SimpleScriptV2 v
    where
      --TODO: this will need to be adjusted when more versions are added
      -- with appropriate helper functions it can probably be done in an
      -- era-generic style
      toMinimumSimpleScriptVersion s =
        case adjustSimpleScriptVersion SimpleScriptV1 s of
          Nothing -> ScriptInAnyLang (SimpleScriptLanguage SimpleScriptV2)
                                     (SimpleScript SimpleScriptV2 s)
          Just s' -> ScriptInAnyLang (SimpleScriptLanguage SimpleScriptV1)
                                     (SimpleScript SimpleScriptV1 s')


instance IsCardanoEra era => FromJSON (ScriptInEra era) where
  parseJSON v =
    case cardanoEra :: CardanoEra era of
      ByronEra   -> fail "Scripts are not supported in the Byron era"

      ShelleyEra -> ScriptInEra SimpleScriptV1InShelley
                  . SimpleScript SimpleScriptV1
                <$> parseSimpleScript SimpleScriptV1 v

      --TODO: this will need to be adjusted when more versions are added.
      -- It can probably be done in an era-generic style, with the use of
      -- appropriate helper functions.
      AllegraEra -> toMinimumSimpleScriptVersion
                <$> parseSimpleScript SimpleScriptV2 v
        where
          toMinimumSimpleScriptVersion s =
            case adjustSimpleScriptVersion SimpleScriptV1 s of
              Nothing -> ScriptInEra SimpleScriptV2InAllegra
                                     (SimpleScript SimpleScriptV2 s)
              Just s' -> ScriptInEra SimpleScriptV1InAllegra
                                     (SimpleScript SimpleScriptV1 s')

      MaryEra -> toMinimumSimpleScriptVersion
             <$> parseSimpleScript SimpleScriptV2 v
        where
          toMinimumSimpleScriptVersion s =
            case adjustSimpleScriptVersion SimpleScriptV1 s of
              Nothing -> ScriptInEra SimpleScriptV2InMary
                                     (SimpleScript SimpleScriptV2 s)
              Just s' -> ScriptInEra SimpleScriptV1InMary
                                     (SimpleScript SimpleScriptV1 s')


instance IsSimpleScriptLanguage lang => FromJSON (SimpleScript lang) where
  parseJSON = parseSimpleScript simpleScriptVersion


parseSimpleScript :: SimpleScriptVersion lang
                  -> Value -> Aeson.Parser (SimpleScript lang)
parseSimpleScript lang v = parseScriptSig          v
                       <|> parseScriptBefore  lang v
                       <|> parseScriptAfter   lang v
                       <|> parseScriptAny     lang v
                       <|> parseScriptAll     lang v
                       <|> parseScriptAtLeast lang v

parseScriptAny :: SimpleScriptVersion lang
               -> Value -> Aeson.Parser (SimpleScript lang)
parseScriptAny lang =
    Aeson.withObject "any" $ \obj -> do
      t <- obj .: "type"
      case t :: Text of
        "any" -> do vs <- obj .: "scripts"
                    RequireAnyOf <$> gatherSimpleScriptTerms lang vs
        _ -> fail "\"any\" script value not found"

parseScriptAll :: SimpleScriptVersion lang
               -> Value -> Aeson.Parser (SimpleScript lang)
parseScriptAll lang =
    Aeson.withObject "all" $ \obj -> do
      t <- obj .: "type"
      case t :: Text of
        "all" -> do vs <- obj .: "scripts"
                    RequireAllOf <$> gatherSimpleScriptTerms lang vs
        _ -> fail "\"all\" script value not found"

parseScriptAtLeast :: SimpleScriptVersion lang
                   -> Value -> Aeson.Parser (SimpleScript lang)
parseScriptAtLeast lang =
    Aeson.withObject "atLeast" $ \obj -> do
      v <- obj .: "type"
      case v :: Text of
        "atLeast" -> do
          r  <- obj .: "required"
          vs <- obj .: "scripts"
          case r of
            Number sci ->
              case toBoundedInteger sci of
                Just reqInt ->
                  do scripts <- gatherSimpleScriptTerms lang vs
                     let numScripts = length scripts
                     when
                       (reqInt > numScripts)
                       (fail $ "Required number of script signatures exceeds the number of scripts."
                             <> " Required number: " <> show reqInt
                             <> " Number of scripts: " <> show numScripts)
                     return $ RequireMOf reqInt scripts
                Nothing -> fail $ "Error in \"required\" key: "
                                <> show sci <> " is not a valid Int"
            _ -> fail "\"required\" value should be an integer"
        _        -> fail "\"atLeast\" script value not found"

gatherSimpleScriptTerms :: SimpleScriptVersion lang
                        -> Vector Value -> Aeson.Parser [SimpleScript lang]
gatherSimpleScriptTerms lang = mapM (parseSimpleScript lang) . Vector.toList

parseScriptSig :: Value -> Aeson.Parser (SimpleScript lang)
parseScriptSig =
    Aeson.withObject "sig" $ \obj -> do
      v <- obj .: "type"
      case v :: Text of
        "sig" -> do k <- obj .: "keyHash"
                    RequireSignature <$> parsePaymentKeyHash k
        _     -> fail "\"sig\" script value not found"

parseScriptBefore :: SimpleScriptVersion lang
                  -> Value -> Aeson.Parser (SimpleScript lang)
parseScriptBefore lang =
    Aeson.withObject "before" $ \obj -> do
      v <- obj .: "type"
      case v :: Text of
        "before" ->
          case timeLocksSupported lang of
            Just supported -> RequireTimeBefore supported <$> obj .: "slot"
            Nothing -> fail ("type \"before\" not supported in " ++ show lang)
        _ -> fail "\"before\" script value not found"

parseScriptAfter :: SimpleScriptVersion lang
                 -> Value -> Aeson.Parser (SimpleScript lang)
parseScriptAfter lang =
    Aeson.withObject "after" $ \obj -> do
      v <- obj .: "type"
      case v :: Text of
        "after" ->
          case timeLocksSupported lang of
            Just supported -> RequireTimeAfter supported <$> obj .: "slot"
            Nothing -> fail ("type \"after\" not supported in " ++ show lang)
        _       -> fail "\"after\" script value not found"

parsePaymentKeyHash :: Text -> Aeson.Parser (Hash PaymentKey)
parsePaymentKeyHash txt =
    case deserialiseFromRawBytesHex (AsHash AsPaymentKey) (Text.encodeUtf8 txt) of
      Just payKeyHash -> return payKeyHash
      Nothing -> fail $ "Error deserialising payment key hash: " <> Text.unpack txt

