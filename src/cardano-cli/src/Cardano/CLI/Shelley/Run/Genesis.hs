{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TupleSections #-}
module Cardano.CLI.Shelley.Run.Genesis
  ( runGenesisCreate
  ) where

import           Cardano.Prelude

import           Cardano.Config.Shelley.ColdKeys (KeyError, KeyRole (..), OperatorKeyRole (..),
                    readVerKey)

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT, handleIOExceptT, hoistEither)

import qualified Data.Aeson as Aeson
import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Char (isDigit)
import           Data.Coerce (coerce)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import           Data.Time.Clock (NominalDiffTime, UTCTime, addUTCTime, getCurrentTime)

import           Cardano.Chain.Common (Lovelace, unsafeGetLovelace)
import           Cardano.CLI.Ops (CliError (..))
import           Cardano.CLI.Shelley.Parsers (GenesisDir (..))
import           Cardano.Config.Shelley.Genesis

import           Ouroboros.Consensus.BlockchainTime (SystemStart (..))
import           Ouroboros.Consensus.Shelley.Protocol (TPraosStandardCrypto)

import qualified Prelude

import           Shelley.Spec.Ledger.Keys (DiscKeyHash (..), GenKeyHash, KeyHash)
import qualified Shelley.Spec.Ledger.Keys as Ledger

import           System.Directory (listDirectory)
import           System.FilePath ((</>), takeExtension)

runGenesisCreate :: GenesisDir -> Maybe SystemStart -> Lovelace -> ExceptT CliError IO ()
runGenesisCreate (GenesisDir gendir) mStart amount = do
  start <- maybe (SystemStart <$> getCurrentTimePlus30) pure mStart
  template <- readShelleyGenesis (gendir </> "genesis.spec.json")
  genDlgs <- readGenDelegsMap gendir

  -- create the final dynamic type and feed to updateTemplate
  writeShelleyGenesis (gendir </> "genesis.json") (updateTemplate start amount genDlgs template)

  liftIO $ putTextLn "Genesis file created, but still need to set genesis delegates and genesis distribution."

-- -------------------------------------------------------------------------------------------------

-- | Current UTCTime plus 30 seconds
getCurrentTimePlus30 :: ExceptT CliError IO UTCTime
getCurrentTimePlus30 =
    plus30sec <$> liftIO getCurrentTime
  where
    plus30sec :: UTCTime -> UTCTime
    plus30sec = addUTCTime (30 :: NominalDiffTime)


readShelleyGenesis :: FilePath -> ExceptT CliError IO (ShelleyGenesis TPraosStandardCrypto)
readShelleyGenesis fpath = do
  lbs <- handleIOExceptT (IOError fpath) $ LBS.readFile fpath
  firstExceptT (AesonDecode fpath . Text.pack) . hoistEither $ Aeson.eitherDecode' lbs

updateTemplate
    :: SystemStart -> Lovelace
    -> (Map (GenKeyHash TPraosStandardCrypto) (KeyHash TPraosStandardCrypto))
    -> ShelleyGenesis TPraosStandardCrypto -> ShelleyGenesis TPraosStandardCrypto
updateTemplate start amount delKeys template =
  template
    { sgStartTime = start
    , sgMaxLovelaceSupply = unsafeGetLovelace amount
    , sgGenDelegs = delKeys
    }

writeShelleyGenesis :: FilePath -> ShelleyGenesis TPraosStandardCrypto -> ExceptT CliError IO ()
writeShelleyGenesis fpath sg =
  handleIOExceptT (IOError fpath) $ LBS.writeFile fpath (encodePretty sg)

readGenDelegsMap :: FilePath -> ExceptT CliError IO (Map (GenKeyHash TPraosStandardCrypto) (KeyHash TPraosStandardCrypto))
readGenDelegsMap gendir = do
    gkm <- firstExceptT KeyCliError $ readGenesisKeys (gendir </> "genesis-keys")
    dkm <- firstExceptT KeyCliError $ readDelegateKeys (gendir </> "delegate-keys")
    -- Both maps should have an identical set of keys.
    -- The mapMaybe will silently drop any map elements for which the key does not exist
    -- in both maps.
    pure $ Map.fromList (mapMaybe (rearrange gkm dkm) $ Map.keys gkm)
  where
    rearrange :: Map Int (Ledger.VKey TPraosStandardCrypto) -> Map Int (Ledger.VKey TPraosStandardCrypto)
             -> Int -> Maybe (GenKeyHash TPraosStandardCrypto, KeyHash TPraosStandardCrypto)
    rearrange gkm dkm i =
      case (Map.lookup i gkm, Map.lookup i dkm) of
        (Just a, Just b) -> Just (coerce (Ledger.hashKey a), Ledger.hashKey b)
        _otherwise -> Nothing


readGenesisKeys :: FilePath -> ExceptT KeyError IO (Map Int (Ledger.VKey TPraosStandardCrypto))
readGenesisKeys gendir = do
  files <- filter isVkey <$> liftIO (listDirectory gendir)
  fmap Map.fromList <$> traverse (readIndexVerKey GenesisKey) $ map (gendir </>) files

readDelegateKeys :: FilePath -> ExceptT KeyError IO (Map Int (Ledger.VKey TPraosStandardCrypto))
readDelegateKeys deldir = do
  files <- filter isVkey <$> liftIO (listDirectory deldir)
  fmap Map.fromList <$> traverse (readIndexVerKey (OperatorKey GenesisDelegateKey)) $ map (deldir </>) files

readIndexVerKey :: KeyRole -> FilePath -> ExceptT KeyError IO (Int, Ledger.VKey TPraosStandardCrypto)
readIndexVerKey role fpath =
  (extractIndex fpath,) <$> readVerKey role fpath

extractIndex :: FilePath -> Int
extractIndex fpath =
  case filter isDigit fpath of
    [] -> 0
    xs -> Prelude.read xs

isVkey :: FilePath -> Bool
isVkey fp = takeExtension fp == ".vkey"
