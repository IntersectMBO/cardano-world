{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Faucet.Web (userAPI, server, SiteVerifyRequest(..)) where

import Cardano.Api (CardanoEra, IsShelleyBasedEra, ShelleyBasedEra, TxInMode(TxInMode), Lovelace(Lovelace), IsCardanoEra, TxCertificates(TxCertificatesNone), serialiseAddress, SigningKey)
import Cardano.Api.Shelley (StakeCredential, makeStakeAddressDelegationCertificate, PoolId, TxCertificates(TxCertificates), certificatesSupportedInEra, BuildTxWith(BuildTxWith), Witness(KeyWitness), KeyWitnessInCtx(KeyWitnessForStakeAddr), StakeExtendedKey, serialiseToBech32)
import Cardano.CLI.Run.Friendly (friendlyTxBS)
import Cardano.CLI.Shelley.Run.Address (renderShelleyAddressCmdError)
import Cardano.CLI.Shelley.Run.Transaction (SomeWitness(AStakeExtendedSigningKey))
import Cardano.Faucet.Misc (convertEra, parseAddress, toFaucetValue, faucetValueToLovelace)
import Cardano.Faucet.TxUtils (makeAndSignTx)
import Cardano.Faucet.Types (CaptchaToken, ForwardedFor(..), SendMoneyReply(..), DelegationReply(..), SiteVerifyReply(..), SiteVerifyRequest(..), SecretKey, FaucetState(..), ApiKeyValue(..), RateLimitResult(..), ApiKey(..), RateLimitAddress(..), UtxoStats(..), FaucetValue(..), FaucetConfigFile(..), FaucetWebError(..), SiteKey(..), vkeyToAddr, SendMoneySent(..))
import Cardano.Faucet.Utils (findUtxoOfSize, computeUtxoStats)
import Cardano.Prelude hiding ((%))
import Control.Concurrent.STM (writeTQueue, TMVar, takeTMVar, putTMVar, readTMVar)
import Control.Monad.Trans.Except.Extra (left)
import Data.Aeson (eitherDecode)
import Data.ByteString.Lazy qualified as LBS
import Data.IP (IPv4, fromHostAddress)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.Lazy qualified as LT
import Data.Text.Lazy.Encoding qualified as LT
import Data.Time.Clock (UTCTime, getCurrentTime, addUTCTime, diffUTCTime)
import Formatting ((%), format, sformat)
import Formatting.ShortFormatters hiding (x, b, f, l)
import Network.HTTP.Client.TLS (newTlsManagerWith, tlsManagerSettings)
import Network.HTTP.Media.MediaType ((//), (/:))
import Network.Socket (SockAddr(SockAddrInet))
import Prelude qualified (id)
import Servant
import Servant.Client (ClientM, ClientError, client, runClientM, mkClientEnv, BaseUrl(BaseUrl), Scheme(Https))

-- create recaptcha api keys at https://www.google.com/recaptcha/admin/create
-- reCAPTCHA v2, "i am not a robot"

instance FromHttpApiData PoolId where
  parseUrlPiece input = either (Left . T.pack) (Right . Prelude.id) $ eitherDecode (LBS.fromStrict $ encodeUtf8 $ "\"" <> input <> "\"")

data HTML
instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

instance MimeRender HTML Text where
  mimeRender _ = LT.encodeUtf8 . LT.fromStrict

#define ApiKeyProtected QueryParam' '[Optional] "api_key" Text :> QueryParam' '[Optional] "g-recaptcha-response" CaptchaToken :> RemoteHost :> Header "X-Forwarded-For" ForwardedFor :> Header "Origin" Text

type SendMoneyUrl = "send-money" :> Capture "destination_address" Text :> QueryParam' '[Optional] "type" Text :> ApiKeyProtected :> Post '[JSON] (Headers '[Header "Access-Control-Allow-Origin" Text] SendMoneyReply)
type SendMoneyQuery = "send-money" :> QueryParam' '[Required] "address" Text :> QueryParam' '[Optional] "type" Text :> ApiKeyProtected :> Get '[JSON] (Headers '[Header "Access-Control-Allow-Origin" Text] SendMoneyReply)
type Metrics = "metrics" :> Get '[PlainText] Text
type DelegateStakeUrl = "delegate" :> Capture "poolid" PoolId :> ApiKeyProtected :> Get '[JSON] (Headers '[Header "Access-Control-Allow-Origin" Text] DelegationReply)
type DelegateStakeQuery = "delegate" :> QueryParam' '[Required] "poolid" PoolId :> ApiKeyProtected :> Get '[JSON] (Headers '[Header "Access-Control-Allow-Origin" Text] DelegationReply)
type SiteVerify = "recaptcha" :> "api" :> "siteverify" :> ReqBody '[FormUrlEncoded] SiteVerifyRequest :> Post '[JSON] SiteVerifyReply
type GetSiteKey = "get-site-key" :> Get '[PlainText] Text
type GetBasicFaucet = "basic-faucet" :> Get '[HTML] Text

-- faucet root dir
type RootDir = SendMoneyUrl :<|> SendMoneyQuery :<|> Metrics :<|> DelegateStakeUrl :<|> DelegateStakeQuery :<|> GetSiteKey :<|> GetBasicFaucet
-- recaptcha root dir
type CaptchaRootDir = SiteVerify

userAPI :: Proxy RootDir
userAPI = Proxy

recaptchaApi :: Proxy CaptchaRootDir
recaptchaApi = Proxy

siteVerify :: SiteVerifyRequest -> ClientM SiteVerifyReply
siteVerify = client recaptchaApi

doSiteVerify :: SecretKey -> CaptchaToken -> IO (Either ClientError SiteVerifyReply)
doSiteVerify secret token = do
  manager' <- newTlsManagerWith tlsManagerSettings
  runClientM (siteVerify $ SiteVerifyRequest secret token Nothing) (mkClientEnv manager' (BaseUrl Https "www.google.com" 443 ""))

server :: IsShelleyBasedEra era =>
  CardanoEra era
  -> ShelleyBasedEra era
  -> FaucetState era
  -> Text
  -> Server RootDir
server era sbe faucetState indexHtml = handleSendMoney era sbe faucetState :<|> handleSendMoney era sbe faucetState :<|> handleMetrics faucetState :<|> handleDelegateStake era sbe faucetState :<|> handleDelegateStake era sbe faucetState :<|> handleStaticFile site_key_reply :<|> (handleStaticFile indexHtml)
  where
    site_key_reply = sformat ("site_key = \"" % st % "\";") $ unSiteKey $ fcfRecaptchaSiteKey $ fsConfig faucetState

handleStaticFile :: Text -> Servant.Handler Text
handleStaticFile = pure

-- takes a TMVar of used and unused stake kets
-- atomically pops one off the unused list, adds it to the used list, and returns the popped key
getKeyToDelegate :: TMVar ([(Word32, SigningKey StakeExtendedKey, StakeCredential)], [(Word32, Lovelace, PoolId)]) -> PoolId -> STM (SigningKey StakeExtendedKey, StakeCredential)
getKeyToDelegate tmvar poolid = do
  (availableKeys, usedKeys) <- takeTMVar tmvar
  case (poolid `elem` map (\(_,_,p) -> p) usedKeys, availableKeys) of
    (True, _) -> do
      throwSTM FaucetWebErrorAlreadyDelegated
    (False, []) -> do
      throwSTM FaucetWebErrorStakeKeyNotFound
    (False, (index, skey, vkey):rest) -> do
      putTMVar tmvar (rest, (index, 0, poolid):usedKeys)
      pure (skey, vkey)

-- checks if a given origin is in the whitelist, and if so, puts that origin in the header
getCorsReply :: [Text] -> Maybe Text -> (a -> Headers '[Header "Access-Control-Allow-Origin" Text] a)
getCorsReply whitelist mOrigin = case mOrigin of
  Nothing -> noHeader
  (Just origin) -> case (origin `elem` whitelist) of
    True -> addHeader origin
    False -> noHeader

handleDelegateStake :: IsShelleyBasedEra era
  => CardanoEra era
  -> ShelleyBasedEra era
  -> FaucetState era
  -> PoolId
  -> Maybe Text
  -> Maybe CaptchaToken
  -> SockAddr
  -> Maybe ForwardedFor
  -> Maybe Text
  -> Servant.Handler (Headers '[Header "Access-Control-Allow-Origin" Text] DelegationReply)
handleDelegateStake era sbe fs@FaucetState{skey,vkey,network,utxoTMVar,queue,stakeTMVar,fsConfig} poolId mApiKey mToken remoteip mForwardedFor mOrigin = do
  let
    clientIP = pickIp mForwardedFor remoteip
  eResult <- liftIO $ runExceptT $ do
    when (fcfMaxStakeKeyIndex fsConfig == Nothing) $ left $ FaucetWebErrorTodo "delegation disabled"
    mReply <- liftIO $ decideBetweenKeyAndCaptcha Nothing mApiKey mToken fsConfig
    (key, limits) <- case mReply of
      Just (_, ApiKeyValue _ _ _ _ False) -> left FaucetWebErrorKeyCantDelegate
      Just x -> pure x
      Nothing -> left FaucetWebErrorInvalidApiKey
    now <- liftIO $ getCurrentTime
    res <- liftIO $ atomically $ do
      let
        maybeBumpLimitAndGetKey = do
          limitResult <- checkRateLimits now [ RateLimitAddressPool poolId, RateLimitAddressNetwork clientIP ] key fs limits
          case limitResult of
            RateLimitResultAllow -> do
              -- the rate limits allow the action at the current time
              -- get an unused stake key
              stakeKey <- getKeyToDelegate stakeTMVar poolId
              -- and get a txout to fund the delegation tx
              txinout <- findUtxoOfSize utxoTMVar $ Ada $ Lovelace ((fcfDelegationUtxoSize fsConfig) * 1000000)
              pure $ Right (stakeKey, txinout)
            RateLimitResultDeny waitPeriod -> throwSTM $ FaucetWebErrorRateLimitExeeeded waitPeriod (serialiseToBech32 poolId)
      -- getKeyToDelegate and findUtxoOfSize can use throwSTM to report an error, and undo the entire atomic action
      catchSTM maybeBumpLimitAndGetKey $ pure . Left
    case res of
      Left err -> left err
      Right ((stake_skey, creds), txinout) -> do
        let
          cert = makeStakeAddressDelegationCertificate creds poolId
          stake_witness = AStakeExtendedSigningKey stake_skey
          x = BuildTxWith $ Map.fromList [(creds,KeyWitness KeyWitnessForStakeAddr)]
        addressAny <- withExceptT (FaucetWebErrorTodo . renderShelleyAddressCmdError) $ vkeyToAddr network vkey
        supported <- maybe (left $ FaucetWebErrorTodo "cert error") pure $ certificatesSupportedInEra era
        (signedTx, txid) <- makeAndSignTx sbe txinout addressAny network [skey, stake_witness] (TxCertificates supported [cert] x)
        let
          prettyTx = friendlyTxBS era signedTx
        eraInMode <- convertEra era
        putStrLn $ format ("delegating stake key to pool " % sh) poolId
        liftIO $ atomically $ writeTQueue queue (TxInMode signedTx eraInMode, prettyTx)
        pure $ DelegationReplySuccess txid
  let corsHeader = getCorsReply (fcfAllowedCorsOrigins fsConfig) mOrigin
  case eResult of
    Left err -> do
      pure $ corsHeader $ DelegationReplyError err
    Right result -> do
      pure $ corsHeader $ result

insertUsage :: TMVar (Map ApiKey (Map RateLimitAddress UTCTime)) -> ApiKey -> UTCTime -> RateLimitAddress -> STM ()
insertUsage tmvar apikey now addr = do
  mainMap <- takeTMVar tmvar
  let
    apiKeyMap :: Map RateLimitAddress UTCTime
    apiKeyMap = fromMaybe mempty (Map.lookup apikey mainMap)
    apiKeyMap' :: Map RateLimitAddress UTCTime
    apiKeyMap' = Map.insert addr now apiKeyMap
    mainMap' = Map.insert apikey apiKeyMap' mainMap
  putTMVar tmvar mainMap'

-- check the rate limits for the given key, update the tmvar to record the usage of this key, and return if the action is allowed or not
checkRateLimits :: IsCardanoEra era => UTCTime -> [RateLimitAddress] -> ApiKey -> FaucetState era -> ApiKeyValue -> STM RateLimitResult
checkRateLimits now addresses apikey FaucetState{fsRateLimitState} ApiKeyValue{akvRateLimit} = do
  mainMap <- readTMVar fsRateLimitState
  let
    -- when many addresses where last used, for the current key
    apiKeyMap :: Map RateLimitAddress UTCTime
    apiKeyMap = fromMaybe mempty (Map.lookup apikey mainMap)
    -- when an address was last used
    getLastUsage :: RateLimitAddress -> Maybe UTCTime
    getLastUsage addr' = Map.lookup addr' apiKeyMap
    lastUsages :: [ Maybe UTCTime ]
    lastUsages = map getLastUsage addresses
    compareTimes :: Maybe UTCTime -> Maybe UTCTime -> Maybe UTCTime
    compareTimes Nothing Nothing = Nothing
    compareTimes (Just a) Nothing = Just a
    compareTimes Nothing (Just b) = Just b
    compareTimes (Just a) (Just b) = Just (if a > b then a else b)
    -- when any of the addresses given, have last been used
    lastUsage :: Maybe UTCTime
    lastUsage = Cardano.Prelude.foldl' compareTimes Nothing lastUsages
    recordUsage :: STM ()
    recordUsage = do
      mapM_ (insertUsage fsRateLimitState apikey now) addresses
  (allowed, result) <- case lastUsage of
    Nothing -> do
      -- this addr has never been used on this api key
      pure (True, RateLimitResultAllow)
    Just lastUsed -> do
      let after = addUTCTime akvRateLimit lastUsed
      pure $ if now > after then (True, RateLimitResultAllow) else (False, RateLimitResultDeny $ after `diffUTCTime` now)
  when allowed recordUsage
  pure result

checkRecaptcha :: SecretKey -> CaptchaToken -> IO Bool
checkRecaptcha secret token = do
  res <- doSiteVerify secret token
  print res
  case res of
    (Left _err) -> do
      pure False
    (Right (SiteVerifyReply _ts _host)) -> do
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
      -- how many utxo exist at each value
      stats :: Map FaucetValue Int
      (UtxoStats stats) = computeUtxoStats utxo
      -- utxo that are entirely missing but required by something
      missingUtxo :: Map FaucetValue Int
      missingUtxo = Map.difference (Map.fromList $ map (\fv -> (fv,0)) fsBucketSizes) stats
      isRequiredSize :: FaucetValue -> Maybe (Text, MetricValue)
      isRequiredSize v = if (elem v fsBucketSizes) then Just ("is_valid",MetricValueInt 1) else Nothing
      isForDelegation v = if (v == Lovelace ((fcfDelegationUtxoSize fsConfig) * 1000000)) then Just ("for_delegation",MetricValueInt 1) else Nothing
      valueAttribute :: FaucetValue -> [Maybe (Text, MetricValue)]
      valueAttribute fv = [Just ("lovelace", MetricValueInt l), Just ("ada",MetricValueFloat $ (fromIntegral l) / 1000000)]
        where
          Lovelace l = faucetValueToLovelace fv
      toStats :: FaucetValue -> Int -> Metric
      toStats fv@((Ada l)) count = Metric (Map.fromList $ catMaybes $ valueAttribute fv <> [isRequiredSize fv, isForDelegation l]) "faucet_utxo" (MetricValueInt $ fromIntegral count)
      toStats fv@(FaucetValueMultiAsset _) count = Metric (Map.fromList $ catMaybes $ valueAttribute fv <> [ isRequiredSize fv ]) "bucket_todo" (MetricValueInt $ fromIntegral count)
      stakeUnusedToMetric :: Metric
      stakeUnusedToMetric = Metric mempty "faucet_delegation_available" (MetricValueInt $ fromIntegral $ length stakeUnused)
      stakeUsedToMetric :: Metric
      stakeUsedToMetric = Metric mempty "faucet_delegation_pools" (MetricValueInt $ fromIntegral $ length stakeUsed)
      stakeRewardsMetric :: [Metric]
      stakeRewardsMetric = map (\(index, Lovelace reward, pool) -> Metric (Map.fromList [("index", MetricValueInt $ fromIntegral index), ("pool", MetricValueStr $ serialiseToBech32 pool)]) "faucet_delegation_rewards" (MetricValueInt reward)) stakeUsed
      utxoMetrics :: [Metric]
      utxoMetrics = Map.foldlWithKey (\rows value count -> (toStats value count):rows) [] (stats <> missingUtxo)
      metrics :: [Metric]
      metrics = utxoMetrics <> [ stakeUnusedToMetric, stakeUsedToMetric ] <> stakeRewardsMetric
      result = Cardano.Prelude.unlines $ Cardano.Prelude.map toMetric metrics
    pure result

pickIp :: Maybe ForwardedFor -> SockAddr -> IPv4
pickIp (Just (ForwardedFor (a:_))) _ = a
pickIp _ (SockAddrInet _port hostaddr) = fromHostAddress hostaddr
pickIp _ _ = fromHostAddress 0x100007f -- 127.0.0.1

-- if a valid api key is given, return that key and its limits
-- if the apikey is invalid, act like it didnt exist
-- if a recaptcha token exists and is valid, return those limits
-- if all keys are missing or invalid return Nothing
decideBetweenKeyAndCaptcha :: Maybe Text -> Maybe Text -> Maybe CaptchaToken -> FaucetConfigFile -> IO (Maybe (ApiKey, ApiKeyValue))
decideBetweenKeyAndCaptcha mType (Just apiKeyText) mToken config@FaucetConfigFile{fcfApiKeys} = case (apiKeyText `Map.lookup` fcfApiKeys) of
  -- api key was not valid, just use recaptcha
  -- TODO, should it give an error instead?
  Nothing -> decideBetweenKeyAndCaptcha mType Nothing mToken config
  Just limits -> pure $ Just (ApiKey apiKeyText, limits)
decideBetweenKeyAndCaptcha mType Nothing (Just token) FaucetConfigFile{fcfRecaptchaSecretKey,fcfRecaptchaLimits} = do
  let
    captchaType = fromMaybe "default" mType
  valid <- checkRecaptcha fcfRecaptchaSecretKey token
  case (valid, captchaType `Map.lookup` fcfRecaptchaLimits) of
    (True, Just limits) -> pure $ Just (Recaptcha captchaType, limits)
    _ -> pure Nothing
decideBetweenKeyAndCaptcha _ Nothing Nothing _ = pure Nothing

handleSendMoney :: IsShelleyBasedEra era =>
  CardanoEra era
  -> ShelleyBasedEra era
  -> FaucetState era
  -> Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe CaptchaToken
  -> SockAddr
  -> Maybe ForwardedFor
  -> Maybe Text
  -> Servant.Handler (Headers '[Header "Access-Control-Allow-Origin" Text] SendMoneyReply)
handleSendMoney era sbe fs@FaucetState{network,utxoTMVar,skey,queue,fsConfig} addr mType mApiKey mToken remoteip mForwardedFor mOrigin = do
  let clientIP = pickIp mForwardedFor remoteip
  eResult <- liftIO $ runExceptT $ do
    addressAny <- parseAddress addr
    mReply <- liftIO $ decideBetweenKeyAndCaptcha mType mApiKey mToken fsConfig
    (key, limits) <- case mReply of
      Just x -> pure x
      Nothing -> left FaucetWebErrorInvalidApiKey
    now <- liftIO $ getCurrentTime
    result <- liftIO $ atomically $ do
      let
        maybeBumpLimitAndGetUtxo = do
          limitResult <- checkRateLimits now [ RateLimitAddressCardano addressAny, RateLimitAddressNetwork clientIP ] key fs limits
          case limitResult of
            RateLimitResultAllow -> do
              txinout <- findUtxoOfSize utxoTMVar $ toFaucetValue limits
              pure $ Right txinout
            RateLimitResultDeny waitPeriod -> throwSTM $ FaucetWebErrorRateLimitExeeeded waitPeriod (serialiseAddress addressAny)
      -- findUtxoOfSize can use throwSTM to report an error, and undo the entire atomic action
      catchSTM maybeBumpLimitAndGetUtxo $ pure . Left
    txinout@(txin, _) <- case result of
      Left err -> left err
      Right txinout -> pure txinout
    eraInMode <- convertEra era
    (signedTx, txid) <- makeAndSignTx sbe txinout addressAny network [skey] TxCertificatesNone
    putStrLn $ format (sh % ": sending funds to address " % st % " via txid " % sh) clientIP (serialiseAddress addressAny) txid
    let
      prettyTx = friendlyTxBS era signedTx
    liftIO $ atomically $ writeTQueue queue (TxInMode signedTx eraInMode, prettyTx)
    return $ SendMoneyReplySuccess $ SendMoneySent txid txin $ toFaucetValue limits
  let corsHeader = getCorsReply (fcfAllowedCorsOrigins fsConfig) mOrigin
  case eResult of
    Right msg -> pure $ corsHeader msg
    Left err -> do
      liftIO $ logError clientIP err
      pure $ corsHeader $ SendMoneyError err

logError :: IPv4 -> FaucetWebError -> IO ()
logError ip (FaucetWebErrorRateLimitExeeeded secs addr) = putStrLn $ format (sh % ": rate limit exeeded for " % t % " will reset in " % sh) ip (LT.fromStrict addr) secs
logError ip (FaucetWebErrorInvalidAddress addr _) = putStrLn $ format (sh % ": invalid cardano address: " % t) ip (LT.fromStrict addr)
logError ip (FaucetWebErrorInvalidApiKey) = putStrLn $ format (sh % ": invalid api key") ip
logError ip (FaucetWebErrorUtxoNotFound value) = putStrLn $ format (sh % ": faucet out of funds for: " % sh) ip value
logError _ FaucetWebErrorEraConversion = putStr @Text "era conversion error"
logError ip err = putStrLn $ format (sh % ": unsupported error: " % sh) ip err
