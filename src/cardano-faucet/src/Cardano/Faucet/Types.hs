{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

module Cardano.Faucet.Types where

import Cardano.Address.Derivation (Depth(RootK, AccountK, PaymentK), XPrv, genMasterKeyFromMnemonic, indexFromWord32, deriveAccountPrivateKey, deriveAddressPrivateKey)
import Cardano.Address.Style.Shelley (Shelley, Role(UTxOExternal, Stake))
import Cardano.Api (AnyCardanoEra, IsCardanoEra, TxIn, TxOut, CtxUTxO, NetworkId, TxInMode, CardanoMode, TxId, FileError, Lovelace, AddressAny(AddressByron, AddressShelley), NetworkId, AssetId(AssetId, AdaAssetId), Quantity, SigningKey, getVerificationKey, makeByronAddress, castVerificationKey, PaymentExtendedKey)
import Cardano.Api.Shelley (PoolId, StakeExtendedKey, StakeCredential)
import Cardano.CLI.Environment (EnvSocketError)
import Cardano.CLI.Shelley.Key (InputDecodeError)
import Cardano.CLI.Shelley.Run.Address (SomeAddressVerificationKey(AByronVerificationKey, APaymentVerificationKey, APaymentExtendedVerificationKey, AGenesisUTxOVerificationKey), ShelleyAddressCmdError, buildShelleyAddress)
import Cardano.CLI.Shelley.Run.Transaction (ShelleyTxCmdError, SomeWitness, renderShelleyTxCmdError)
import Cardano.Mnemonic (mkSomeMnemonic, getMkSomeMnemonicError)
import Cardano.Prelude
import Control.Concurrent.STM (TMVar, TQueue)
import Control.Monad.Trans.Except.Extra (left)
import Data.Aeson (ToJSON(..), object, (.=), Options(fieldLabelModifier), defaultOptions, camelTo2, genericToJSON, FromJSON(parseJSON), eitherDecodeFileStrict, Value(String), withObject, (.:), (.:?))
import Data.ByteString.Char8 qualified as BSC
import Data.Either.Combinators (mapRight)
import Data.HashMap.Strict qualified as HM
import Data.IP (IPv4)
import Data.List.Split (splitOn)
import Data.Text qualified as T
import Data.Time.Clock (UTCTime, NominalDiffTime)
import Prelude (String, error, id, read)
import Servant (FromHttpApiData(parseHeader, parseQueryParam, parseUrlPiece))
import Web.Internal.FormUrlEncoded (ToForm(toForm), fromEntriesByKey)

-- the sitekey, secretkey, and token from recaptcha
newtype SiteKey = SiteKey { unSiteKey :: Text } deriving Show
newtype SecretKey = SecretKey { unSecretKey :: Text } deriving Show
newtype CaptchaToken = CaptchaToken Text

instance FromHttpApiData CaptchaToken where
  parseHeader bs = mapRight CaptchaToken (parseHeader bs)
  parseQueryParam t = mapRight CaptchaToken (parseQueryParam t)

-- the X-Forwarded-For header
newtype ForwardedFor = ForwardedFor [IPv4] deriving (Eq, Show)

parseIpList :: Prelude.String -> ForwardedFor
parseIpList input = ForwardedFor $ reverse $ map (Prelude.read) (splitOn "," input)

instance FromHttpApiData ForwardedFor where
  parseHeader = Right . parseIpList . BSC.unpack
  parseUrlPiece = Right . parseIpList . T.unpack

-- errors not sent to users
data FaucetError = FaucetErrorTodo ShelleyTxCmdError
  | FaucetErrorSocketNotFound EnvSocketError
  | FaucetErrorLoadingKey (FileError InputDecodeError)
  | FaucetErrorParsingConfig String
  | FaucetErrorConfigFileNotSet
  | FaucetErrorBadMnemonic Text
  | FaucetErrorBadIdx
  | FaucetErrorShelleyAddr ShelleyAddressCmdError
  deriving Generic

renderFaucetError :: FaucetError -> Text
renderFaucetError (FaucetErrorTodo err) = renderShelleyTxCmdError err
renderFaucetError (FaucetErrorSocketNotFound err) = show err
renderFaucetError (FaucetErrorLoadingKey err) = show err
renderFaucetError (FaucetErrorParsingConfig err) = show err
renderFaucetError FaucetErrorConfigFileNotSet = "$CONFIG_FILE not set"
renderFaucetError (FaucetErrorBadMnemonic msg) = "bad mnemonic " <> msg
renderFaucetError FaucetErrorBadIdx = "bad index"
renderFaucetError (FaucetErrorShelleyAddr err) = show err

-- errors that can be sent to the user
data FaucetWebError = FaucetWebErrorInvalidAddress Text Text
  | FaucetWebErrorInvalidApiKey
  | FaucetWebErrorKeyCantDelegate
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
  | FaucetWebErrorStakeKeyNotFound
  deriving (Generic, Show)

instance Exception FaucetWebError

instance ToJSON FaucetWebError where
  toJSON = genericToJSON defaultOptions

-- an api key
-- Recaptcha is a special key, that can only be obtained by answering a recaptcha prompt
data ApiKey = Recaptcha | ApiKey Text deriving (Ord, Eq)

-- the state of the entire faucet
data IsCardanoEra era => FaucetState era = FaucetState
  { utxoTMVar :: TMVar (Map TxIn (TxOut CtxUTxO era))
  , stakeTMVar :: TMVar ([(Word32, SigningKey StakeExtendedKey, StakeCredential)], [(Word32, Lovelace, PoolId)])
  , network :: NetworkId
  , queue :: TQueue (TxInMode CardanoMode, ByteString)
  , skey :: SomeWitness
  , vkey :: SomeAddressVerificationKey
  , fsAcctKey :: Shelley 'AccountK XPrv
  , fsConfig :: FaucetConfigFile
  , fsRateLimitState :: TMVar (Map ApiKey (Map RateLimitAddress UTCTime))
  , fsBucketSizes :: [Lovelace]
  }

-- the result of checking a rate limit
data RateLimitResult = RateLimitResultAllow | RateLimitResultDeny NominalDiffTime

-- the key for rate limits
data RateLimitAddress = RateLimitAddressCardano AddressAny | RateLimitAddressNetwork IPv4 | RateLimitAddressPool PoolId deriving (Eq, Ord)

-- send-money success reply
data SendMoneySent = SendMoneySent
  { txid :: TxId
  , txin :: TxIn
  }

data StakeKeyIntermediateState = StakeKeyIntermediateStateNotRegistered Word32 | StakeKeyIntermediateStateRegistered (Word32, SigningKey StakeExtendedKey, StakeCredential, Lovelace)

data StakeKeyState = StakeKeyRegistered Word32 (SigningKey StakeExtendedKey) StakeCredential Lovelace
  | StakeKeyDelegated Word32 Lovelace PoolId
  | StakeKeyNotRegistered Word32 deriving Show

-- the full reply type for /send-money
data SendMoneyReply = SendMoneyReplySuccess SendMoneySent
  | SendMoneyError FaucetWebError

instance ToJSON SendMoneyReply where
  toJSON (SendMoneyReplySuccess (SendMoneySent{txid,txin})) = object [ "txid" .= txid, "txin" .= txin ]
  toJSON (SendMoneyError err) = object [ "error" .= err ]

-- the full reply type for /delegate
data DelegationReply = DelegationReplySuccess TxId
  | DelegationReplyError FaucetWebError

instance ToJSON DelegationReply where
  toJSON (DelegationReplySuccess txid) = object [ "success" .= True, "txid" .= txid ]
  toJSON (DelegationReplyError err) = object [ "error" .= err ]

-- a complete description of an api key
data ApiKeyValue = ApiKeyValue
  { akvApiKey :: Text
  , akvLovelace :: Lovelace
  , akvRateLimit :: NominalDiffTime
  , akvTokens :: [FaucetToken]
  , akvCanDelegate :: Bool
  } deriving (Generic, Show)

instance FromJSON ApiKeyValue where
  parseJSON = withObject "ApiKeyValue" $ \v -> do
    akvApiKey <- v .: "api_key"
    akvLovelace <- v .: "lovelace"
    akvRateLimit <- v .: "rate_limit"
    akvTokens <- fromMaybe [] <$> v .:? "tokens"
    akvCanDelegate <- fromMaybe False <$> v .:? "delegate"
    pure $ ApiKeyValue{..}

-- the complete config file
data FaucetConfigFile = FaucetConfigFile
  { fcfMnemonic :: [Text]
  , fcfApiKeys :: HM.HashMap Text ApiKeyValue
  , fcfRecaptchaLimits :: ApiKeyValue
  , fcfNetwork :: NetworkId
  , fcfMaxStakeKeyIndex :: Maybe Word32
  , fcfDebug :: Bool
  , fcfDelegationUtxoSize :: Integer
  , fcfRecaptchaSiteKey :: SiteKey
  , fcfRecaptchaSecretKey :: SecretKey
  , fcfAllowedCorsOrigins :: [Text]
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
    fcfRecaptchaSiteKey <- SiteKey <$> o .: "recaptcha_site_key"
    fcfRecaptchaSecretKey <- SecretKey <$> o .: "recaptcha_secret_key"
    fcfAllowedCorsOrigins <- o .: "allowed_cors_origins"
    pure FaucetConfigFile{..}

-- a value with only ada, or a value containing a mix of assets
data FaucetValue = Ada Lovelace
  | FaucetValueMultiAsset [(AssetId, Quantity)] deriving (Show, Eq, Ord)

faucetValueToLovelace :: FaucetValue -> Lovelace
faucetValueToLovelace (Ada l) = l
faucetValueToLovelace _ = error "unfinished"

instance ToJSON FaucetValue where
  toJSON (Ada lovelace) = object [ "lovelace" .= lovelace ]
  toJSON (FaucetValueMultiAsset _) = String "unsupported"

data UtxoStats = UtxoStats (Map FaucetValue Int) deriving Show

-- a request to recaptcha
data SiteVerifyRequest = SiteVerifyRequest
  { svrSecret :: SecretKey
  , svrResponse :: CaptchaToken
  , svrRemoteIP :: Maybe Text
  }

instance ToForm SiteVerifyRequest where
  toForm (SiteVerifyRequest (SecretKey secret) (CaptchaToken token) mRemoteIp) = fromEntriesByKey foo
    where
      foo :: [(Text, [Text])]
      foo = [ ("secret", [secret]), ("response", [token]) ] ++ maybe [] (\x -> [("remoteip",[x])]) mRemoteIp

-- a reply from recaptcha
data SiteVerifyReply = SiteVerifyReply Text Text
  | SiteVerifyError [Text]
  deriving (Generic, Show)

-- example replies:
-- { "success": false, "error-codes": [ "timeout-or-duplicate" ]}
instance FromJSON SiteVerifyReply where
  parseJSON = withObject "SiteVerifyReply" $ \o -> do
    success <- o .: "success"
    case success of
      True -> do
        ts <- o .: "challenge_ts"
        hostname <- o .: "hostname"
        pure $ SiteVerifyReply ts hostname
      False -> do
        errors <- o .: "error-codes"
        pure $ SiteVerifyError errors

data FaucetToken = FaucetToken (AssetId, Quantity) deriving Show

tokenToValue :: FaucetToken -> Value
tokenToValue (FaucetToken (AssetId policyid token, q)) = object [ "policyid" .= policyid, "token" .= token, "quantity" .= q ]
tokenToValue (FaucetToken (AdaAssetId, q)) = object [ "lovelace" .= q ]

instance FromJSON FaucetToken where
  parseJSON = withObject "FaucetToken" $ \v -> do
    policyid <- v .: "policy_id"
    quantity <- v .: "quantity"
    token <- v .: "token"
    pure $ FaucetToken (AssetId policyid token,quantity)

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
