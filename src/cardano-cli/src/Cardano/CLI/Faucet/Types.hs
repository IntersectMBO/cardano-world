{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Faucet.Types where

import Cardano.Api (AnyCardanoEra, TxBodyErrorAutoBalance, IsCardanoEra, TxIn, TxOut, CtxUTxO, NetworkId, TxInMode, CardanoMode, AnyConsensusMode, TxId, FileError, Lovelace, AddressAny, NetworkId(Testnet, Mainnet), NetworkMagic(NetworkMagic))
import Cardano.CLI.Environment (EnvSocketError)
import Cardano.CLI.Shelley.Key
import Cardano.CLI.Shelley.Run.Address (SomeAddressVerificationKey)
import Cardano.CLI.Shelley.Run.Transaction (ShelleyTxCmdError, TxFeature, SomeWitness, renderShelleyTxCmdError)
import Cardano.Prelude
import Control.Concurrent.STM (TMVar, TQueue)
import Control.Monad.Trans.Except.Extra (left)
import Data.Aeson (ToJSON(..), object, (.=), Options(fieldLabelModifier), defaultOptions, camelTo2, genericToJSON, FromJSON(parseJSON), genericParseJSON, eitherDecodeFileStrict, Value(Object, String), (.:?))
import Data.HashMap.Strict
import Data.Time.Clock (UTCTime)
import Network.Socket (SockAddr)
import Ouroboros.Consensus.Cardano.Block (EraMismatch)
import Ouroboros.Network.Protocol.LocalStateQuery.Type (AcquireFailure)
import Prelude (String, error, fail)
import Text.Parsec

data FaucetError = FaucetErrorInvalidAddress Text ParseError
  | FaucetErrorUtxoNotFound
  | FaucetErrorEraConversion
  | FaucetErrorTodo ShelleyTxCmdError
  | FaucetErrorSocketNotFound EnvSocketError
  | FaucetErrorEraMismatch EraMismatch
  | FaucetErrorAutoBalance TxBodyErrorAutoBalance
  | FaucetErrorFeatureMismatch AnyCardanoEra TxFeature
  | FaucetErrorAcquireFailure AcquireFailure
  | FaucetErrorConsensusModeMismatchTxBalance AnyConsensusMode AnyCardanoEra
  | FaucetErrorLoadingKey (FileError InputDecodeError)
  | FaucetErrorParsingConfig String
  | FaucetErrorRateLimit

data ApiKey = Recaptcha | ApiKey Text deriving (Ord, Eq)

data IsCardanoEra era => FaucetState era = FaucetState
  { utxoTMVar :: TMVar (Map TxIn (TxOut CtxUTxO era))
  , network :: NetworkId
  , queue :: TQueue (TxInMode CardanoMode, ByteString)
  , skey :: SomeWitness
  , vkey :: SomeAddressVerificationKey
  , fsConfig :: FaucetConfigFile
  , fsRateLimitState :: TMVar (Map ApiKey (Map (Either AddressAny SockAddr) UTCTime))
  , fsBucketSizes :: [Lovelace]
  }

data SendMoneyReply = SendMoneyReply
  { txid :: TxId
  , txin :: TxIn
  }

instance ToJSON SendMoneyReply where
  toJSON (SendMoneyReply{txid,txin}) = object [ "txid" .= txid, "txin" .= txin ]

renderFaucetError :: FaucetError -> Text
renderFaucetError (FaucetErrorInvalidAddress a b) = show a <> show b
renderFaucetError (FaucetErrorEraConversion) = "unexpected error"
renderFaucetError (FaucetErrorTodo err) = renderShelleyTxCmdError err
renderFaucetError (FaucetErrorUtxoNotFound) = "no utxo of proper size found"
renderFaucetError (FaucetErrorSocketNotFound err) = show err
renderFaucetError (FaucetErrorEraMismatch err) = show err
renderFaucetError (FaucetErrorAutoBalance err) = show err
renderFaucetError (FaucetErrorFeatureMismatch a b) = show a <> " " <> show b
renderFaucetError (FaucetErrorAcquireFailure err) = show err
renderFaucetError (FaucetErrorConsensusModeMismatchTxBalance a b) = show a <> " " <> show b
renderFaucetError (FaucetErrorLoadingKey err) = show err
renderFaucetError (FaucetErrorParsingConfig err) = show err
renderFaucetError (FaucetErrorRateLimit) = "rate limit error"

-- TODO, find a better way to do this
jsonOptions :: Options
jsonOptions = defaultOptions { fieldLabelModifier = (camelTo2 '_') . stripPrefix }
  where
    stripPrefix :: String -> String
    stripPrefix (_:_:_:baseName) = baseName
    stripPrefix bad = error $ "bad fieldname: " ++ bad

-- copied from tx-generator, should probably not be orphaned?
instance ToJSON NetworkId where
  toJSON Mainnet = "Mainnet"
  toJSON (Testnet (NetworkMagic t)) = object ["Testnet" .= t]

instance FromJSON NetworkId where
  parseJSON j = case j of
    (String "Mainnet") -> return Mainnet
    (Object v) -> v .:? "Testnet" >>= \case
      Nothing -> failed
      Just w -> return $ Testnet $ NetworkMagic w
    _invalid -> failed
    where
      failed = fail $ "Parsing of NetworkId failed: " <> show j

data FaucetConfigFile = FaucetConfigFile
  { fcfSkeyPath :: FilePath
  , fcfApiKeys :: HashMap Text (Lovelace, Int)
  , fcfRecaptchaLimits :: (Lovelace, Int)
  , fcfNetwork :: NetworkId
  } deriving (Generic, Show)

instance ToJSON FaucetConfigFile where
  toJSON = genericToJSON jsonOptions

instance FromJSON FaucetConfigFile where
  parseJSON = genericParseJSON jsonOptions

parseConfig :: FilePath -> ExceptT FaucetError IO FaucetConfigFile
parseConfig path = do
  eResult <- liftIO $ eitherDecodeFileStrict path
  case eResult of
    Left err -> left $ FaucetErrorParsingConfig err
    Right res -> pure res
