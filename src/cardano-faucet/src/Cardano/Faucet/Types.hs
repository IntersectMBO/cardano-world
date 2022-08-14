{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

module Cardano.Faucet.Types where

import Cardano.Api (AnyCardanoEra, IsCardanoEra, TxIn, TxOut, CtxUTxO, NetworkId, TxInMode, CardanoMode, TxId, FileError, Lovelace, AddressAny(AddressByron, AddressShelley), NetworkId, AssetId(AssetId, AdaAssetId), Quantity, SigningKey, getVerificationKey, makeByronAddress, castVerificationKey, PaymentExtendedKey)
import Cardano.CLI.Environment (EnvSocketError)
import Cardano.CLI.Shelley.Key
import Cardano.CLI.Shelley.Run.Address (SomeAddressVerificationKey(AByronVerificationKey, APaymentVerificationKey, APaymentExtendedVerificationKey, AGenesisUTxOVerificationKey), ShelleyAddressCmdError, buildShelleyAddress)
import Cardano.CLI.Shelley.Run.Transaction (ShelleyTxCmdError, SomeWitness, renderShelleyTxCmdError)
import Cardano.Prelude
import Control.Concurrent.STM (TMVar, TQueue)
import Control.Monad.Trans.Except.Extra (left)
import Data.Aeson (ToJSON(..), object, (.=), Options(fieldLabelModifier), defaultOptions, camelTo2, genericToJSON, FromJSON(parseJSON), genericParseJSON, eitherDecodeFileStrict, Value(String), withObject, (.:))
import Data.HashMap.Strict qualified as HM
import Data.Time.Clock (UTCTime, NominalDiffTime)
import Prelude (String, error, id)
import Web.Internal.FormUrlEncoded (ToForm(toForm), fromEntriesByKey)
import Cardano.Mnemonic (mkSomeMnemonic, getMkSomeMnemonicError)
import Cardano.Address.Derivation (Depth(RootK, AccountK, PaymentK), XPrv, genMasterKeyFromMnemonic, indexFromWord32, deriveAccountPrivateKey, deriveAddressPrivateKey)
import Cardano.Address.Style.Shelley (Shelley, Role(UTxOExternal, Stake))
import qualified Data.Text as T
import Data.IP (IPv4)
import Cardano.Api.Shelley (PoolId, StakeExtendedKey, StakeCredential)

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
  | FaucetWebErrorInvalidApiKey
  | FaucetWebErrorRateLimitExeeeded NominalDiffTime Text
  | FaucetWebErrorUtxoNotFound FaucetValue
  | FaucetWebErrorEraConversion
  | FaucetWebErrorSocketNotFound Text
  | FaucetWebErrorTodo Text
  | FaucetWebErrorFeatureMismatch AnyCardanoEra Text
  | FaucetWebErrorConsensusModeMismatchTxBalance Text AnyCardanoEra
  | FaucetWebErrorAcquireFailure Text
  | FaucetWebErrorEraMismatch Text
  | FaucetWebErrorAutoBalance Text
  | FaucetWebErrorBadIdx
  | FaucetWebErrorAlreadyDelegated
  deriving (Generic, Show)

data ApiKey = Recaptcha | ApiKey Text deriving (Ord, Eq)

data DelegationAtomicResult = DelegationAtomicResultPoolAlreadyDelegated
  | DelegationAtomicResultNoKeys
  | DelegationAtomicResult (SigningKey StakeExtendedKey, StakeCredential)
  deriving Show

data IsCardanoEra era => FaucetState era = FaucetState
  { utxoTMVar :: TMVar (Map TxIn (TxOut CtxUTxO era))
  , stakeTMVar :: TMVar ([(Word32, SigningKey StakeExtendedKey, StakeCredential)], [(Word32, Lovelace, PoolId)])
  , network :: NetworkId
  , queue :: TQueue (TxInMode CardanoMode, ByteString)
  , skey :: SomeWitness
  , vkey :: SomeAddressVerificationKey
  , fsAcctKey :: Shelley 'AccountK XPrv
  , fsConfig :: FaucetConfigFile
  , fsRateLimitState :: TMVar (Map ApiKey (Map (Either AddressAny IPv4) UTCTime))
  , fsBucketSizes :: [Lovelace]
  }

data SendMoneySent = SendMoneySent
  { txid :: TxId
  , txin :: TxIn
  }

data StakeKeyIntermediateState = StakeKeyIntermediateStateNotRegistered Word32 | StakeKeyIntermediateStateRegistered (Word32, SigningKey StakeExtendedKey, StakeCredential, Lovelace)

data StakeKeyState = StakeKeyRegistered Word32 (SigningKey StakeExtendedKey) StakeCredential Lovelace
  | StakeKeyDelegated Word32 Lovelace PoolId
  | StakeKeyNotRegistered Word32 deriving Show

data SendMoneyReply = SendMoneyReplySuccess SendMoneySent
  | SendMoneyError FaucetWebError

data DelegationReply = DelegationReplySuccess
  |DelegationReplyError FaucetWebError

data ApiKeyValue = ApiKeyValue
  { akvApiKey :: Text
  , akvLovelace :: Lovelace
  , akvRateLimit :: NominalDiffTime
  , akvTokens :: [FaucetToken]
  } deriving (Generic, Show)

data FaucetConfigFile = FaucetConfigFile
  { fcfMnemonic :: [Text]
  , fcfApiKeys :: HM.HashMap Text ApiKeyValue
  , fcfRecaptchaLimits :: ApiKeyValue
  , fcfNetwork :: NetworkId
  , fcfMaxStakeKeyIndex :: Maybe Word32
  , fcfDebug :: Bool
  , fcfDelegationUtxoSize :: Integer
  , fcfRecaptchaSiteKey :: Text
  , fcfRecaptchaSecretKey :: Text
  } deriving (Generic, Show)

instance FromJSON FaucetConfigFile where
  parseJSON = withObject "FaucetConfigFile" $ \o -> do
    fcfMnemonic <- o .: "mnemonic"
    apiKeyList <- o .: "api_keys"
    let fcfApiKeys = HM.fromList $ map (\key@ApiKeyValue{akvApiKey} -> (akvApiKey, key)) apiKeyList
    fcfRecaptchaLimits <- o .: "recaptcha_limits"
    fcfNetwork <- o .: "network"
    fcfMaxStakeKeyIndex <- o .: "max_stake_key_index"
    fcfDebug <- o .: "debug"
    fcfDelegationUtxoSize <- o .: "delegation_utxo_size"
    fcfRecaptchaSiteKey <- o .: "recaptcha_site_key"
    fcfRecaptchaSecretKey <- o .: "recaptcha_secret_key"
    pure FaucetConfigFile{..}

data FaucetValue = Ada Lovelace
  | FaucetValueMultiAsset [(AssetId, Quantity)] deriving (Show, Eq, Ord)

faucetValueToLovelace :: FaucetValue -> Lovelace
faucetValueToLovelace (Ada l) = l
faucetValueToLovelace _ = error "unfinished"

instance ToJSON FaucetValue where
  toJSON (Ada lovelace) = object [ "lovelace" .= lovelace ]
  toJSON (FaucetValueMultiAsset _) = String "unsupported"

data UtxoStats = UtxoStats (Map FaucetValue Int) deriving Show

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

instance ToJSON DelegationReply where
  toJSON (DelegationReplySuccess) = object [ "success" .= True]
  toJSON (DelegationReplyError err) = object [ "error" .= err ]

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

instance FromJSON ApiKeyValue where
  parseJSON = withObject "ApiKeyValue" $ \v -> do
    akvApiKey <- v .: "api_key"
    akvLovelace <- v .: "lovelace"
    akvRateLimit <- v .: "rate_limit"
    akvTokens <- v .: "tokens"
    pure $ ApiKeyValue{..}

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

accountKeyToStakeKey :: Shelley 'AccountK XPrv -> Word32 -> Shelley 'PaymentK XPrv
accountKeyToStakeKey acctK index = deriveAddressPrivateKey acctK Stake (maybe (error "bad stake index") Prelude.id $ indexFromWord32 index)


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

    _addrK <- accountKeyToPaymentKey acctK 0x14
    pure ()
    {-
    stakeK <- accountKeyToStakeKey acctK 0x1
    let
      x :: SigningKey StakeExtendedKey
      x = StakeExtendedSigningKey $ getKey stakeK
      foo' :: SigningKey PaymentExtendedKey
      foo' = PaymentExtendedSigningKey $ getKey addrK
      _foo :: SomeWitness
      _foo = APaymentExtendedSigningKey foo'
      a :: VerificationKey StakeExtendedKey
      a = getVerificationKey x
      b :: Hash StakeKey
      b = verificationKeyHash $ castVerificationKey a
      c :: StakeCredential
      c = StakeCredentialByKey b
      e :: Either String PoolId
      e = eitherDecode "\"pool1mjhwg36auxnd37mcfqlc647w92hdedejj5jalvckaxsw68tjjxw\""
      d :: PoolId
      d = fromRight (error "todo") e
      f :: Certificate
      f = makeStakeAddressDelegationCertificate c d
    print b
    print c
    print e
    print f
    addr <- paymentKeyToAddress foo' $ fcfNetwork config
    print $ serialiseAddress addr
    pure $ getVerificationKey foo'
    -}
    -- addr_test1vq4p02j5rf5w69kldld0t9wt6fe2efvfhcxsk4qdhwm2pnglr5yaj
  case eResult of
    Left err -> print $ renderFaucetError err
    Right msg -> print msg
    --(Right shortmw) = mkSomeMnemonic @'[15] ["network","empty","cause","mean","expire","private","finger","accident","session","problem","absurd","banner","stage","void","what"]
    --rootK = Shelley $ genMasterKeyFromMnemonicShelley mw sndFactor
