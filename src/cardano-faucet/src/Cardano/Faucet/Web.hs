{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Faucet.Web (userAPI, server, SiteVerifyRequest(..)) where

import Cardano.Api (CardanoEra, IsShelleyBasedEra, ShelleyBasedEra, TxInMode(TxInMode), AddressAny, Lovelace(Lovelace), IsCardanoEra, TxCertificates(TxCertificatesNone), serialiseAddress, SigningKey)
import Cardano.Api.Shelley (StakeCredential, makeStakeAddressDelegationCertificate, PoolId, TxCertificates(TxCertificates), certificatesSupportedInEra, BuildTxWith(BuildTxWith), Witness(KeyWitness), KeyWitnessInCtx(KeyWitnessForStakeAddr), StakeExtendedKey, serialiseToBech32)
import Cardano.CLI.Run.Friendly (friendlyTxBS)
import Cardano.CLI.Shelley.Run.Address (renderShelleyAddressCmdError)
import Cardano.CLI.Shelley.Run.Transaction (SomeWitness(AStakeExtendedSigningKey))
import Cardano.Faucet.Misc
import Cardano.Faucet.TxUtils
import Cardano.Faucet.Types
import Cardano.Faucet.Utils
import Cardano.Prelude hiding ((%))
import Control.Concurrent.STM (writeTQueue, TMVar, takeTMVar, putTMVar, readTMVar)
import Control.Monad.Trans.Except.Extra (left)
import Data.Aeson (eitherDecode)
import Data.HashMap.Strict qualified as HM
import Data.IP
import Data.List.Split
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.Lazy.Encoding qualified as LT
import Data.Time.Clock
import Formatting ((%), format, sformat)
import Formatting.ShortFormatters hiding (x, b, f, l)
import Network.HTTP.Client.TLS (newTlsManagerWith, tlsManagerSettings)
import Network.Socket (SockAddr(SockAddrInet))
import Servant
import Servant.Client (ClientM, ClientError, client, runClientM, mkClientEnv, BaseUrl(BaseUrl), Scheme(Https))
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Lazy as LT
import qualified Prelude (read, String, id)
import Network.HTTP.Media.MediaType

-- create recaptcha api keys at https://www.google.com/recaptcha/admin/create
-- reCAPTCHA v2, "i am not a robot"

newtype ForwardedFor = ForwardedFor [IPv4] deriving (Eq, Show)

parseIpList :: Prelude.String -> ForwardedFor
parseIpList input = ForwardedFor $ reverse $ map (Prelude.read) (splitOn "," input)

instance FromHttpApiData ForwardedFor where
  parseHeader = Right . parseIpList . BSC.unpack
  parseUrlPiece = Right . parseIpList . T.unpack

instance FromHttpApiData PoolId where
  parseUrlPiece input = either (Left . T.pack) (Right . Prelude.id) $ eitherDecode (LBS.fromStrict $ encodeUtf8 $ "\"" <> input <> "\"")

data HTML
instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

instance MimeRender HTML Text where
  mimeRender _ = LT.encodeUtf8 . LT.fromStrict

type SendMoneyUrl = "send-money" :> Capture "destination_address" Text :> QueryParam' '[Optional] "api_key" Text :> QueryParam' '[Optional] "g-recaptcha-response" Text :> RemoteHost :> Header "X-Forwarded-For" ForwardedFor :> Post '[JSON] SendMoneyReply
type SendMoneyQuery = "send-money" :> QueryParam' '[Required] "address" Text :> QueryParam' '[Optional] "api_key" Text :> QueryParam' '[Optional] "g-recaptcha-response" Text :> RemoteHost :> Header "X-Forwarded-For" ForwardedFor :> Get '[JSON] SendMoneyReply
type Metrics = "metrics" :> Get '[PlainText] Text
type DelegateStake = "delegate" :> Capture "poolid" PoolId :> Get '[JSON] DelegationReply
type SiteVerify = "recaptcha" :> "api" :> "siteverify" :> ReqBody '[FormUrlEncoded] SiteVerifyRequest :> Post '[JSON] SiteVerifyReply
type GetSiteKey = "get-site-key" :> Get '[PlainText] Text
type GetBasicFaucet = "basic-faucet" :> Get '[HTML] Text

-- faucet root dir
type RootDir = SendMoneyUrl :<|> SendMoneyQuery :<|> Metrics :<|> DelegateStake :<|> GetSiteKey :<|> GetBasicFaucet
-- recaptcha root dir
type CaptchaRootDir = SiteVerify

userAPI :: Proxy RootDir
userAPI = Proxy

recaptchaApi :: Proxy CaptchaRootDir
recaptchaApi = Proxy

siteVerify :: SiteVerifyRequest -> ClientM SiteVerifyReply
siteVerify = client recaptchaApi

doSiteVerify :: Text -> Text -> IO (Either ClientError SiteVerifyReply)
doSiteVerify secret token = do
  manager' <- newTlsManagerWith tlsManagerSettings
  runClientM (siteVerify $ SiteVerifyRequest secret token Nothing) (mkClientEnv manager' (BaseUrl Https "www.google.com" 443 ""))

server :: IsShelleyBasedEra era =>
  CardanoEra era
  -> ShelleyBasedEra era
  -> FaucetState era
  -> Text
  -> Server RootDir
server era sbe faucetState indexHtml = handleSendMoney era sbe faucetState :<|> handleSendMoney era sbe faucetState :<|> handleMetrics faucetState :<|> handleDelegateStake era sbe faucetState :<|> handleGetSiteKey (fcfRecaptchaSiteKey $ fsConfig faucetState) :<|> (handleStaticFile indexHtml)

handleStaticFile :: Text -> Servant.Handler Text
handleStaticFile = pure

getKeyToDelegate :: TMVar ([(Word32, SigningKey StakeExtendedKey, StakeCredential)], [(Word32, Lovelace, PoolId)]) -> PoolId -> STM DelegationAtomicResult
getKeyToDelegate tmvar poolid = do
  (availableKeys, usedKeys) <- takeTMVar tmvar
  case (poolid `elem` map (\(_,_,p) -> p) usedKeys, availableKeys) of
    (True, _) -> do
      putTMVar tmvar (availableKeys, usedKeys)
      pure DelegationAtomicResultPoolAlreadyDelegated
    (False, []) -> do
      putTMVar tmvar (availableKeys, usedKeys)
      pure DelegationAtomicResultNoKeys
    (False, (index, skey, vkey):rest) -> do
      putTMVar tmvar (rest, (index, 0, poolid):usedKeys)
      pure $ DelegationAtomicResult (skey, vkey)

handleDelegateStake :: IsShelleyBasedEra era => CardanoEra era -> ShelleyBasedEra era -> FaucetState era -> PoolId -> Servant.Handler DelegationReply
handleDelegateStake era sbe FaucetState{skey,vkey,network,utxoTMVar,queue,stakeTMVar,fsConfig} poolId = do
  eResult <- liftIO $ runExceptT $ do
    when (fcfMaxStakeKeyIndex fsConfig == Nothing) $ left $ FaucetWebErrorTodo "delegation disabled"
    -- TODO, do the stake and utxo in the same atomic
    res <- liftIO $ atomically $ getKeyToDelegate stakeTMVar poolId
    case res of
      DelegationAtomicResultPoolAlreadyDelegated -> left $ FaucetWebErrorAlreadyDelegated
      DelegationAtomicResultNoKeys -> left $ FaucetWebErrorTodo "no stake keys available"
      DelegationAtomicResult (stake_skey, creds) -> do
        let
          cert = makeStakeAddressDelegationCertificate creds poolId
          stake_witness = AStakeExtendedSigningKey stake_skey
          x = BuildTxWith $ Map.fromList [(creds,KeyWitness KeyWitnessForStakeAddr)]
        addressAny <- withExceptT (FaucetWebErrorTodo . renderShelleyAddressCmdError) $ vkeyToAddr network vkey
        txinout <- findUtxoOfSize utxoTMVar $ Ada $ Lovelace ((fcfDelegationUtxoSize fsConfig) * 1000000)
        supported <- maybe (left $ FaucetWebErrorTodo "cert error") pure $ certificatesSupportedInEra era
        (signedTx, _txid) <- makeAndSignTx sbe txinout addressAny network [skey, stake_witness] (TxCertificates supported [cert] x)
        let
          prettyTx = friendlyTxBS era signedTx
        eraInMode <- convertEra era
        putStrLn $ format ("delegating stake key to pool " % sh) poolId
        liftIO $ atomically $ writeTQueue queue (TxInMode signedTx eraInMode, prettyTx)
        pure $ DelegationReplySuccess
  case eResult of
    Left err -> do
      pure $ DelegationReplyError err
    Right result -> do
      pure $ result

getRateLimits :: ApiKey -> FaucetConfigFile -> Maybe ApiKeyValue
getRateLimits Recaptcha FaucetConfigFile{fcfRecaptchaLimits} = Just fcfRecaptchaLimits
getRateLimits (ApiKey key) FaucetConfigFile{fcfApiKeys} = HM.lookup key fcfApiKeys

insertUsage :: TMVar (Map ApiKey (Map (Either AddressAny IPv4) UTCTime)) -> ApiKey -> Either AddressAny IPv4 -> UTCTime -> STM ()
insertUsage tmvar apikey addr now = do
  mainMap <- takeTMVar tmvar
  let
    apiKeyMap :: Map (Either AddressAny IPv4) UTCTime
    apiKeyMap = fromMaybe mempty (Map.lookup apikey mainMap)
    apiKeyMap' :: Map (Either AddressAny IPv4) UTCTime
    apiKeyMap' = Map.insert addr now apiKeyMap
    mainMap' = Map.insert apikey apiKeyMap' mainMap
  putTMVar tmvar mainMap'

checkRateLimits :: IsCardanoEra era => AddressAny -> IPv4 -> ApiKey -> FaucetState era -> ExceptT FaucetWebError IO (Lovelace, [FaucetToken])
checkRateLimits addr remoteip apikey FaucetState{fsConfig,fsRateLimitState} = do
  now <- liftIO $ getCurrentTime
  let
    mRateLimits = getRateLimits apikey fsConfig
    recordUsage :: STM ()
    recordUsage = do
      insertUsage fsRateLimitState apikey (Left addr) now
      insertUsage fsRateLimitState apikey (Right remoteip) now
    -- Nothing means allow
    -- Just x means you can do it in x time
    checkRateLimitsInternal :: NominalDiffTime -> STM (Maybe NominalDiffTime)
    checkRateLimitsInternal interval = do
      mainMap <- readTMVar fsRateLimitState
      let
        apiKeyMap = fromMaybe mempty (Map.lookup apikey mainMap)
        getLastUsage :: Either AddressAny IPv4 -> Maybe UTCTime
        getLastUsage addr' = Map.lookup addr' apiKeyMap
        lastUsages :: [ Maybe UTCTime]
        lastUsages = [ getLastUsage (Left addr), getLastUsage (Right remoteip) ]
        compareTimes :: Maybe UTCTime -> Maybe UTCTime -> Maybe UTCTime
        compareTimes Nothing Nothing = Nothing
        compareTimes (Just a) Nothing = Just a
        compareTimes Nothing (Just b) = Just b
        compareTimes (Just a) (Just b) = Just (if a > b then a else b)
        lastUsage :: Maybe UTCTime
        lastUsage = Cardano.Prelude.foldl' compareTimes Nothing lastUsages
      disallow <- case lastUsage of
        Nothing -> do
          -- this addr has never been used on this api key
          pure Nothing
        Just lastUsed -> do
          let
            after = addUTCTime interval lastUsed
          pure $ if now > after then Nothing else (Just $ after `diffUTCTime` now)
      if (isNothing disallow) then recordUsage else pure ()
      pure disallow
  case mRateLimits of
    Nothing -> do
      -- api key not found in config
      left $ FaucetWebErrorInvalidApiKey
    Just (ApiKeyValue _ lovelace interval tokens) -> do
      success <- liftIO $ atomically $ checkRateLimitsInternal interval
      case success of
        Nothing -> pure (lovelace,tokens)
        Just time -> left $ FaucetWebErrorRateLimitExeeeded time (serialiseAddress addr)

checkRecaptcha :: Text -> Maybe Text -> IO Bool
checkRecaptcha _ Nothing = pure False
checkRecaptcha secret (Just token) = do
  res <- doSiteVerify secret token
  print res
  case res of
    (Left _err) -> do
      pure False
    (Right (SiteVerifyReply True _ts _host _error)) -> do
      pure True
    _ -> do
      pure False

data MetricValue = MetricValueInt Integer | MetricValueFloat Float | MetricValueStr Text deriving Show

valToString :: MetricValue -> Text
valToString (MetricValueInt i) = show i
valToString (MetricValueFloat f) = show f
valToString (MetricValueStr str) = str

data Metric = Metric (Map Text MetricValue) Text MetricValue deriving Show

attributesToString :: Map Text MetricValue -> Text
attributesToString map' = if (Map.null map') then "" else wrapped
  where
    wrapped = "{" <> joinedAttrs <> "}"
    joinedAttrs = T.intercalate "," $ Map.elems $ Map.mapWithKey (\key val -> key <> "=\"" <> valToString val <> "\"") map'

toMetric :: Metric -> Text
toMetric (Metric attribs key val) = key <> (attributesToString attribs) <> " " <> valToString val

handleMetrics :: IsCardanoEra era => FaucetState era -> Servant.Handler Text
handleMetrics FaucetState{utxoTMVar,fsBucketSizes,fsConfig,stakeTMVar} = do
  liftIO $ do
    (utxo, (stakeUnused, stakeUsed)) <- atomically $ do
      u <- readTMVar utxoTMVar
      stake <- readTMVar stakeTMVar
      pure (u,stake)
    let
      (UtxoStats stats) = computeUtxoStats utxo
      isRequiredSize :: Lovelace -> Maybe (Text, MetricValue)
      isRequiredSize v = if (elem v fsBucketSizes) then Just ("is_valid",MetricValueInt 1) else Nothing
      isForDelegation v = if (v == Lovelace ((fcfDelegationUtxoSize fsConfig) * 1000000)) then Just ("for_delegation",MetricValueInt 1) else Nothing
      toStats :: FaucetValue -> Int -> Metric
      -- TODO, tag the delegation size
      toStats ((Ada l@(Lovelace v))) count = Metric (Map.fromList $ catMaybes $ [Just ("lovelace",MetricValueInt v), isRequiredSize l, isForDelegation l]) "faucet_utxo" (MetricValueInt $ fromIntegral count)
      toStats (FaucetValueMultiAsset _) count = Metric mempty "bucket_todo" (MetricValueInt $ fromIntegral count)
      stakeUnusedToMetric :: Metric
      stakeUnusedToMetric = Metric mempty "faucet_delegation_available" (MetricValueInt $ fromIntegral $ length stakeUnused)
      stakeUsedToMetric :: Metric
      stakeUsedToMetric = Metric mempty "faucet_delegation_pools" (MetricValueInt $ fromIntegral $ length stakeUsed)
      stakeRewardsMetric :: [Metric]
      stakeRewardsMetric = map (\(index, Lovelace reward, pool) -> Metric (Map.fromList [("index", MetricValueInt $ fromIntegral index), ("pool", MetricValueStr $ serialiseToBech32 pool)]) "faucet_delegation_rewards" (MetricValueInt reward)) stakeUsed
      utxoMetrics :: [Metric]
      utxoMetrics = Map.foldlWithKey (\rows value count -> (toStats value count):rows) [] stats
      metrics :: [Metric]
      metrics = utxoMetrics <> [ stakeUnusedToMetric, stakeUsedToMetric ] <> stakeRewardsMetric
      result = Cardano.Prelude.unlines $ Cardano.Prelude.map toMetric metrics
    pure result

pickIp :: Maybe ForwardedFor -> SockAddr -> IPv4
pickIp Nothing (SockAddrInet _port hostaddr) = fromHostAddress hostaddr
pickIp (Just (ForwardedFor (a:_))) _ = a
pickIp _ _ = fromHostAddress 0x100007f -- 127.0.0.1

handleGetSiteKey :: Text -> Servant.Handler Text
handleGetSiteKey sitekey = do
  pure $ sformat ("site_key = \"" % st % "\";") sitekey

handleSendMoney :: IsShelleyBasedEra era =>
  CardanoEra era
  -> ShelleyBasedEra era
  -> FaucetState era
  -> Text
  -> Maybe Text
  -> Maybe Text
  -> SockAddr
  -> Maybe ForwardedFor
  -> Servant.Handler SendMoneyReply
handleSendMoney era sbe fs@FaucetState{network,utxoTMVar,skey,queue,fsConfig} addr mApiKey mRecaptcha remoteip forwardedFor = do
  print mRecaptcha
  let clientIP = pickIp forwardedFor remoteip
  eResult <- liftIO $ runExceptT $ do
    addressAny <- parseAddress addr
    apiKey <- do
      case mApiKey of
        -- TODO, should recaptcha be checked if an apikey is present?
        Just key -> do
          case (key `HM.lookup` (fcfApiKeys fsConfig)) of
            Just _ -> pure $ ApiKey key
            Nothing -> do
              -- api key was not valid, just use recaptcha
              -- TODO, should it give an error instead?
              recaptcha <- liftIO $ checkRecaptcha (fcfRecaptchaSecretKey fsConfig) mRecaptcha
              case recaptcha of
                False -> do
                  left FaucetWebErrorInvalidApiKey
                True -> pure Recaptcha
        Nothing -> do
          recaptcha <- liftIO $ checkRecaptcha (fcfRecaptchaSecretKey fsConfig) mRecaptcha
          case recaptcha of
            False -> do
              left FaucetWebErrorInvalidApiKey
            True -> pure Recaptcha
    -- ratelimits and utxo should be in a single atomically block
    (lovelace,_tokens) <- checkRateLimits addressAny clientIP apiKey fs
    txinout@(txin,_) <- findUtxoOfSize utxoTMVar $ Ada lovelace
    eraInMode <- convertEra era
    (signedTx, txid) <- makeAndSignTx sbe txinout addressAny network [skey] TxCertificatesNone
    putStrLn $ format (sh % ": sending funds to address " % st % " via txid " % sh) clientIP (serialiseAddress addressAny) txid
    let
      prettyTx = friendlyTxBS era signedTx
    liftIO $ atomically $ writeTQueue queue (TxInMode signedTx eraInMode, prettyTx)
    return $ SendMoneyReplySuccess $ SendMoneySent txid txin
  case eResult of
    Right msg -> pure msg
    Left err -> do
      liftIO $ logError clientIP err
      pure $ SendMoneyError err

logError :: IPv4 -> FaucetWebError -> IO ()
logError ip (FaucetWebErrorRateLimitExeeeded secs addr) = putStrLn $ format (sh % ": rate limit exeeded for " % t % " will reset in " % sh) ip (LT.fromStrict addr) secs
logError ip (FaucetWebErrorInvalidAddress addr _) = putStrLn $ format (sh % ": invalid cardano address: " % t) ip (LT.fromStrict addr)
logError ip (FaucetWebErrorInvalidApiKey) = putStrLn $ format (sh % ": invalid api key") ip
logError ip (FaucetWebErrorUtxoNotFound value) = putStrLn $ format (sh % ": faucet out of funds for: " % sh) ip value
logError _ FaucetWebErrorEraConversion = putStr @Text "era conversion error"
logError ip err = putStrLn $ format (sh % ": unsupported error: " % sh) ip err
