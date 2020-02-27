{-# LANGUAGE GADTs #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE NamedFieldPuns #-}

module Ouroboros.Network.Protocol.Handshake.Version
  ( Versions (..)
  , Application (..)
  , Version (..)
  , Sigma (..)
  , Accept (..)
  , Acceptable (..)
  , Dict (..)
  , DictVersion (..)
  , CodecCBORTerm (..)
  , pickVersions
  , VersionMismatch (..)

  -- * Simple or no versioning
  , simpleSingletonVersions
  , unversionedProtocol
  ) where

import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as Map
import Data.Typeable ((:~:)(Refl), Typeable, eqT)
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Term as CBOR
import Codec.Serialise (Serialise(..))


-- Description of versions.
--
-- - Each particular version is a function from a pair of version data
--   (peculiar to that version, existentially-quantified) to some result
--   type (called `r` in this module).
-- - A version-numbering scheme, and the set of supported versions, is
--   defined simultaneously by a `Map` keyed on something, perhaps `Word32` in
--   a real-world instance. The `Sigma` GADT pairs the particular version data
--   with each version definition.

-- | The set of versions supported by the local agent are described by a map
-- keyed on the version identifier.
--
-- If one needs to combine multiple versions the simplest way is to use the
-- 'Semigroup' instance.
--
-- >
-- > fold $ (simpleSingletonVersions ...)
-- >       :| [ (simpleSingletonVersions ...)
-- >          , (simpleSingletonVersions ...)
-- >          , ...
-- >          ]
-- >
--
newtype Versions vNum extra r = Versions
  { getVersions :: Map vNum (Sigma (Version extra r))
  }
  deriving Semigroup via (Map vNum (Sigma (Version extra r)))

instance Functor (Versions vNum extra) where
    fmap f (Versions vs) = Versions $ Map.map fmapSigma vs
      where
        fmapSigma (Sigma t (Version (Application app) extra)) = Sigma t (Version (Application $ \x y -> f (app x y)) extra)

data Sigma f where
  Sigma :: !t -> !(f t) -> Sigma f


-- |
-- A @'Maybe'@ like type which better explains its purpose.
--
data Accept
  = Accept
  | Refuse !Text
  deriving (Eq, Show)

class Acceptable v where
  acceptableVersion :: v -> v -> Accept

-- | Takes a pair of version data: local then remote.
newtype Application r vData = Application
  { runApplication :: vData -> vData -> r
  }

data Version extra r vData = Version
  { versionApplication :: Application r vData
  , versionExtra       :: extra vData
  }

data VersionMismatch vNum where
  NoCommonVersion     :: VersionMismatch vNum
  InconsistentVersion :: vNum -> VersionMismatch vNum

data Dict constraint thing where
  Dict :: constraint thing => Dict constraint thing

data DictVersion vData where
     DictVersion :: ( Typeable vData
                    , Acceptable vData
                    , Show vData
                    )
                 => CodecCBORTerm Text vData
                 -> DictVersion vData

data CodecCBORTerm fail a = CodecCBORTerm
  { encodeTerm :: a -> CBOR.Term
  , decodeTerm :: CBOR.Term -> Either fail a
  }

-- | Pick the version with the highest version number (by `Ord vNum`) common
-- in both maps.
--
-- This is a useful guide for comparison with a version negotiation scheme for
-- use in production between different processes. If the `Versions` maps
-- used by each process are given to `pickVersions`, it should come up with
-- the same result as the production version negotiation.
--
-- It is _assumed_ that if the maps agree on a key, then the existential
-- types in the `Sigma` value at the key are also equal.
--
-- So, the issue here is that they may not have the same version data type.
-- This becomes a non-issue on the network because the decoder/encoder
-- basically fills the role of a safe dynamic type cast.
pickVersions
  :: ( Ord vNum )
  => (forall vData . extra vData -> Dict Typeable vData)
  -> Versions vNum extra r
  -> Versions vNum extra r
  -> Either (VersionMismatch vNum) (r, r)
pickVersions isTypeable lversions rversions = case Map.toDescList commonVersions of
  [] -> Left NoCommonVersion
  (vNum, (Sigma (ldata :: ldata) lversion, Sigma (rdata :: rdata) rversion)) : _ ->
    case (isTypeable (versionExtra lversion), isTypeable (versionExtra rversion)) of
      (Dict, Dict) -> case eqT :: Maybe (ldata :~: rdata) of
        Nothing   -> Left $ InconsistentVersion vNum
        Just Refl ->
          let lapp = versionApplication lversion
              rapp = versionApplication rversion
          in  Right (runApplication lapp ldata rdata, runApplication rapp rdata rdata)
  where
  commonVersions = getVersions lversions `intersect` getVersions rversions
  intersect = Map.intersectionWith (,)

-- Examples commented out because of unused definition warnings.

{-
import Data.Functor.Const (Const (..))

exApplication1 :: Application (IO ()) ()
exApplication1 = Application $ \localUnit remoteUnit ->
  putStrLn "Application 1 does nothing"

exVersion1 :: Version (Const ()) (IO ()) ()
exVersion1 = Version exApplication1 (Const ())

newtype Magic = Magic { getMagic :: Word32 }
  deriving (Show, Eq)

exApplication2 :: Application (IO ()) Magic
exApplication2 = Application $ \localMagic remoteMagic ->
  if localMagic == remoteMagic
  then putStrLn "Magic is consistent"
  else putStrLn "Magic is inconsistent"

exVersion2 :: Version (Const ()) (IO ()) Magic
exVersion2 = Version exApplication2 (Const ())

exVersions :: Versions (Const ()) (IO ())
exVersions = Versions $ Map.fromList
  [ (0, Sigma () exVersion1)
  , (1, Sigma (Magic 42) exVersion2)
  ]
-}

--
-- Simple version negotation
--

-- | Singleton smart constructor for 'Versions'.
--
simpleSingletonVersions
  :: vNum
  -> vData
  -> extra vData
  -> r
  -> Versions vNum extra r
simpleSingletonVersions vNum vData extra r =
  Versions
    $ Map.singleton vNum
        (Sigma vData (Version (Application $ \_ _ -> r) extra))

-- | Version negotiation for an unversioned protocol. Only use this for
-- tests and demos where proper versioning is excessive.
--
data UnversionedProtocol = UnversionedProtocol
  deriving (Eq, Ord, Enum, Show)

instance Serialise UnversionedProtocol where
    encode UnversionedProtocol = CBOR.encodeWord 1
    decode = do
      tag <- CBOR.decodeWord
      case tag of
        1 -> return UnversionedProtocol
        _ -> fail "decode UnversionedProtocol: expected version 1"

data UnversionedProtocolData = UnversionedProtocolData
  deriving (Eq, Show)

instance Acceptable UnversionedProtocolData where
  acceptableVersion UnversionedProtocolData
                    UnversionedProtocolData = Accept

unversionedProtocolDataCodec :: CodecCBORTerm Text UnversionedProtocolData
unversionedProtocolDataCodec = CodecCBORTerm {encodeTerm, decodeTerm}
    where
      encodeTerm :: UnversionedProtocolData -> CBOR.Term
      encodeTerm UnversionedProtocolData = CBOR.TNull

      decodeTerm :: CBOR.Term -> Either Text UnversionedProtocolData
      decodeTerm CBOR.TNull = Right UnversionedProtocolData
      decodeTerm t          = Left $ T.pack $ "unexpected term: " ++ show t

-- | Make a 'Versions' for an unversioned protocol. Only use this for
-- tests and demos where proper versioning is excessive.
--
unversionedProtocol :: app -> Versions UnversionedProtocol DictVersion app
unversionedProtocol =
    simpleSingletonVersions UnversionedProtocol UnversionedProtocolData
                            (DictVersion unversionedProtocolDataCodec)
