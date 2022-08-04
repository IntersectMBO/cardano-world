{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}

module Cardano.Faucet.Types where

import Cardano.Api (AnyCardanoEra, IsCardanoEra, TxIn, TxOut, CtxUTxO, NetworkId, TxInMode, CardanoMode, TxId, FileError, Lovelace, AddressAny(AddressByron, AddressShelley), NetworkId, AssetId(AssetId, AdaAssetId), Quantity, SigningKey(PaymentExtendedSigningKey), getVerificationKey, makeByronAddress, castVerificationKey, PaymentExtendedKey, serialiseAddress)
import Cardano.CLI.Environment (EnvSocketError)
import Cardano.CLI.Shelley.Key
import Cardano.CLI.Shelley.Run.Address (SomeAddressVerificationKey(AByronVerificationKey, APaymentVerificationKey, APaymentExtendedVerificationKey, AGenesisUTxOVerificationKey), ShelleyAddressCmdError, buildShelleyAddress)
import Cardano.CLI.Shelley.Run.Transaction (ShelleyTxCmdError, SomeWitness(APaymentExtendedSigningKey), renderShelleyTxCmdError)
import Cardano.Prelude
import Control.Concurrent.STM (TMVar, TQueue)
import Control.Monad.Trans.Except.Extra (left)
import Data.Aeson (ToJSON(..), object, (.=), Options(fieldLabelModifier), defaultOptions, camelTo2, genericToJSON, FromJSON(parseJSON), genericParseJSON, eitherDecodeFileStrict, Value, withObject, (.:))
import Data.HashMap.Strict qualified as HM
import Data.Time.Clock (UTCTime, NominalDiffTime)
import Network.Socket (SockAddr)
import Prelude (String, error)
import Web.Internal.FormUrlEncoded (ToForm(toForm), fromEntriesByKey)
import Cardano.Mnemonic (mkSomeMnemonic, getMkSomeMnemonicError)
import Cardano.Address.Derivation (Depth(RootK, AccountK, PaymentK), XPrv, genMasterKeyFromMnemonic, indexFromWord32, deriveAccountPrivateKey, deriveAddressPrivateKey)
import Cardano.Address.Style.Shelley (Shelley, Role(UTxOExternal, Stake), getKey)
import qualified Data.Text as T

data FaucetError = FaucetErrorTodo ShelleyTxCmdError
  | FaucetErrorSocketNotFound EnvSocketError
  | FaucetErrorLoadingKey (FileError InputDecodeError)
  | FaucetErrorParsingConfig String
  | FaucetErrorConfigFileNotSet
  | FaucetErrorBadMnemonic Text
  | FaucetErrorBadIdx
  | FaucetErrorShelleyAddr ShelleyAddressCmdError
  deriving Generic

data FaucetWebError = FaucetWebErrorInvalidAddress Text Text
  | FaucetWebErrorRateLimit
  | FaucetWebErrorRateLimitExeeeded NominalDiffTime
  | FaucetWebErrorUtxoNotFound
  | FaucetWebErrorEraConversion
  | FaucetWebErrorSocketNotFound Text
  | FaucetWebErrorTodo Text
  | FaucetWebErrorFeatureMismatch AnyCardanoEra Text
  | FaucetWebErrorConsensusModeMismatchTxBalance Text AnyCardanoEra
  | FaucetWebErrorAcquireFailure Text
  | FaucetWebErrorEraMismatch Text
  | FaucetWebErrorAutoBalance Text
  deriving Generic

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

data SendMoneySent = SendMoneySent
  { txid :: TxId
  , txin :: TxIn
  }

data SendMoneyReply = SendMoneyReplySuccess SendMoneySent
  | SendMoneyError FaucetWebError

data ApiKeyValue = ApiKeyValue
  { akvLovelace :: Lovelace
  , akvRateLimit :: NominalDiffTime
  , akvTokens :: [FaucetToken]
  } deriving (Generic, Show)

data FaucetConfigFile = FaucetConfigFile
  { fcfMnemonic :: [Text]
  , fcfApiKeys :: HM.HashMap Text ApiKeyValue
  , fcfRecaptchaLimits :: ApiKeyValue
  , fcfNetwork :: NetworkId
  } deriving (Generic, Show)

data FaucetValue = Ada Lovelace
  | FaucetValueMultiAsset [(AssetId, Quantity)] deriving (Show, Eq, Ord)

data UtxoStats = UtxoStats (Map FaucetValue Integer) deriving Show

data SiteVerifyRequest = SiteVerifyRequest
  { svrSecret :: Text
  , svrResponse :: Text
  , svrRemoteIP :: Maybe Text
  }

data SiteVerifyReply = SiteVerifyReply
  { svrSuccess :: Bool
  , svrChallengeTs :: Text
  , svrHostname :: Text
  , svrErrorCodes :: Maybe [Text]
  } deriving (Generic, Show)

instance ToJSON FaucetWebError where
  toJSON = genericToJSON defaultOptions

instance ToJSON SendMoneyReply where
  toJSON (SendMoneyReplySuccess (SendMoneySent{txid,txin})) = object [ "txid" .= txid, "txin" .= txin ]
  toJSON (SendMoneyError err) = object [ "error" .= err ]

tokenToValue :: FaucetToken -> Value
tokenToValue (FaucetToken (AssetId policyid token, q)) = object [ "policyid" .= policyid, "token" .= token, "quantity" .= q ]
tokenToValue (FaucetToken (AdaAssetId, q)) = object [ "lovelace" .= q ]

data FaucetToken = FaucetToken (AssetId, Quantity) deriving Show

instance FromJSON FaucetToken where
  parseJSON = withObject "FaucetToken" $ \v -> do
    policyid <- v .: "policy_id"
    quantity <- v .: "quantity"
    token <- v .: "token"
    pure $ FaucetToken (AssetId policyid token,quantity)

instance ToJSON ApiKeyValue where
  toJSON (ApiKeyValue l r t) = object [ "lovelace" .= l, "rate_limit" .= r, "tokens" .= map tokenToValue t ]

instance FromJSON ApiKeyValue where
  parseJSON = withObject "ApiKeyValue" $ \v -> do
    lovelace <- v .: "lovelace"
    ratelimit <- v .: "rate_limit"
    tokens <- v .: "tokens"
    pure $ ApiKeyValue lovelace ratelimit tokens

instance ToJSON FaucetConfigFile where
  toJSON = genericToJSON jsonOptions

instance FromJSON FaucetConfigFile where
  parseJSON = genericParseJSON jsonOptions

instance ToForm SiteVerifyRequest where
  toForm (SiteVerifyRequest secret token mRemoteIp) = fromEntriesByKey foo
    where
      foo :: [(Text, [Text])]
      foo = [ ("secret", [secret]), ("response", [token]) ] ++ maybe [] (\x -> [("remoteip",[x])]) mRemoteIp

instance FromJSON SiteVerifyReply where
  parseJSON = genericParseJSON jsonOptions

renderFaucetError :: FaucetError -> Text
renderFaucetError (FaucetErrorTodo err) = renderShelleyTxCmdError err
renderFaucetError (FaucetErrorSocketNotFound err) = show err
renderFaucetError (FaucetErrorLoadingKey err) = show err
renderFaucetError (FaucetErrorParsingConfig err) = show err
renderFaucetError FaucetErrorConfigFileNotSet = "$CONFIG_FILE not set"
renderFaucetError (FaucetErrorBadMnemonic msg) = "bad mnemonic " <> msg
renderFaucetError FaucetErrorBadIdx = "bad index"
renderFaucetError (FaucetErrorShelleyAddr err) = show err

-- TODO, find a better way to do this
jsonOptions :: Options
jsonOptions = defaultOptions { fieldLabelModifier = (camelTo2 '_') . stripPrefix }
  where
    stripPrefix :: String -> String
    stripPrefix (_:_:_:baseName) = baseName
    stripPrefix bad = error $ "bad fieldname: " ++ bad

parseConfig :: FilePath -> ExceptT FaucetError IO FaucetConfigFile
parseConfig path = do
  eResult <- liftIO $ eitherDecodeFileStrict path
  case eResult of
    Left err -> left $ FaucetErrorParsingConfig err
    Right res -> pure res

mnemonicToRootKey :: Monad m => [Text] -> ExceptT FaucetError m (Shelley 'RootK XPrv)
mnemonicToRootKey mnemonic = do
  mw <- either (left . FaucetErrorBadMnemonic . T.pack . getMkSomeMnemonicError) pure $ mkSomeMnemonic @'[24] mnemonic
  pure $ genMasterKeyFromMnemonic mw mempty

rootKeytoAcctKey :: Monad m => Shelley 'RootK XPrv -> Word32 -> ExceptT FaucetError m (Shelley 'AccountK XPrv)
rootKeytoAcctKey rootK index = do
  accIx <- maybe (left FaucetErrorBadIdx) pure $ indexFromWord32 index
  pure $ deriveAccountPrivateKey rootK accIx

accountKeyToPaymentKey :: Monad m => Shelley 'AccountK XPrv -> Word32 -> ExceptT FaucetError m (Shelley 'PaymentK XPrv)
accountKeyToPaymentKey acctK index = do
  addIx <- maybe (left FaucetErrorBadIdx) pure $ indexFromWord32 index
  pure $ deriveAddressPrivateKey acctK UTxOExternal addIx

accountKeyToStakeKey :: Monad m => Shelley 'AccountK XPrv -> Word32 -> ExceptT FaucetError m (Shelley 'PaymentK XPrv)
accountKeyToStakeKey acctK index = do
  addIx <- maybe (left FaucetErrorBadIdx) pure $ indexFromWord32 index
  pure $ deriveAddressPrivateKey acctK Stake addIx

vkeyToAddr :: NetworkId -> SomeAddressVerificationKey -> ExceptT ShelleyAddressCmdError IO AddressAny
vkeyToAddr nw (AByronVerificationKey vk) = return (AddressByron (makeByronAddress nw vk))
vkeyToAddr nw (APaymentVerificationKey vk) = AddressShelley <$> buildShelleyAddress vk Nothing nw
vkeyToAddr nw (APaymentExtendedVerificationKey vk) = AddressShelley <$> buildShelleyAddress (castVerificationKey vk) Nothing nw
vkeyToAddr nw (AGenesisUTxOVerificationKey vk) = AddressShelley <$> buildShelleyAddress (castVerificationKey vk) Nothing nw

paymentKeyToAddress :: SigningKey PaymentExtendedKey -> NetworkId -> ExceptT FaucetError IO AddressAny
paymentKeyToAddress skey network = do
  withExceptT FaucetErrorShelleyAddr $ vkeyToAddr network (APaymentExtendedVerificationKey $ getVerificationKey skey)

test :: IO ()
test = do
  eResult <- runExceptT $ do
    config <- parseConfig "/home/clever/iohk/cardano-world/sample-config.json"
    print config
    rootK <- mnemonicToRootKey $ fcfMnemonic config
    acctK <- rootKeytoAcctKey rootK 0x80000000

    addrK <- accountKeyToPaymentKey acctK 0x14
    _stakeK <- accountKeyToStakeKey acctK 0x123
    let
      foo' :: SigningKey PaymentExtendedKey
      foo' = PaymentExtendedSigningKey $ getKey addrK
      _foo :: SomeWitness
      _foo = APaymentExtendedSigningKey foo'
    addr <- paymentKeyToAddress foo' $ fcfNetwork config
    -- addr_test1vq4p02j5rf5w69kldld0t9wt6fe2efvfhcxsk4qdhwm2pnglr5yaj
    print $ serialiseAddress addr
    pure $ getVerificationKey foo'
  case eResult of
    Left err -> print $ renderFaucetError err
    Right msg -> print msg
    --(Right shortmw) = mkSomeMnemonic @'[15] ["network","empty","cause","mean","expire","private","finger","accident","session","problem","absurd","banner","stage","void","what"]
    --rootK = Shelley $ genMasterKeyFromMnemonicShelley mw sndFactor
