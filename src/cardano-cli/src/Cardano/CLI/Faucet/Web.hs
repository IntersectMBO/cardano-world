{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.CLI.Faucet.Web (userAPI, server, run, SiteVerifyRequest(..)) where

import Cardano.Api (CardanoEra, IsShelleyBasedEra, ShelleyBasedEra, TxInMode(TxInMode), getTxId, TxId, AddressAny, Lovelace, IsCardanoEra)
import Cardano.CLI.Faucet.Misc
import Cardano.CLI.Faucet.TxUtils
import Cardano.CLI.Faucet.Types
import Cardano.CLI.Faucet.Utils
import Cardano.CLI.Run.Friendly (friendlyTxBS)
import Cardano.CLI.Types
import Cardano.Prelude
import Control.Concurrent.STM (writeTQueue, TMVar, takeTMVar, putTMVar, readTMVar)
import Control.Monad.Trans.Except.Extra (left)
import Data.Aeson (encode, FromJSON(parseJSON), genericParseJSON)
import Data.ByteString.Lazy     qualified as LBS
import Data.HashMap.Strict as HM
import Data.Map.Strict as Map
import Data.String
import Data.Text qualified as T
import Data.Time.Clock
import Servant
import Servant.Client
import Network.HTTP.Client.TLS (newTlsManagerWith, tlsManagerSettings)
import Web.Internal.FormUrlEncoded (ToForm(toForm), fromEntriesByKey)
import Prelude ()
import Network.Socket (SockAddr)

-- https://faucet.cardano-testnet.iohkdev.io/send-money/addr_test1vr3g684kyarnug89c7p7gqxr5y8t45g7q4ge4u8hghndvugn6yn5s?apiKey=&g-recaptcha-response=03AGdBq24qppnXuY6fIcCG2Hrpqxfp0V9Xd3oDqElSikr38sAuPMmpO4dKke9O0NzhtFnv_-cXVSs8h4loNDBDeM3rIb5UDHmoCsIylCHXmOovfDIOWM7417-9nW_8XegF7murR2CpVGDp8js7L33ygKqbPUus8AQncJ26AikCDiDNOe7_u6pHb20pR_a8a2cjfcRu6Ptrq8uTWxk2QiinvSctAZbnyTRscupJNDVvoJ1l52LNXOFNTFowRuyaRu1K9mLAJvbwy5n1il_05UGWRNvK3raCUA1DKhf0l9yOCfEvoNJNp10rTG5JFWeYaIiI3-ismQITIsR3u4akYy1PPjmNyF12vfcjlgbvXdGOcodyiZvKulnp2XNSQVIu-OHiwERumU5IISD9VRzY804Z1tKkRB7_PxpUvE7SOAKdOqmkvZLMn8ob1Fz8I562qiV8oezkVkSqTfqQbK2Vsqn3dYDd-IY0pjUhnw
-- http[s]://$FQDN:$PORT/send-money/$ADDRESS

type SendMoney = "send-money" :> Capture "destination_address" Text :> QueryParam' '[Optional] "api_key" Text :> RemoteHost :> Post '[OctetStream] LBS.ByteString
-- type MyApi = "books" :> QueryParam' Optional "authors" Text :> Get '[JSON] [Book]
type RootDir = SendMoney

data SiteVerifyRequest = SiteVerifyRequest
  { svrSecret :: Text
  , svrResponse :: Text
  , svrRemoteIP :: Maybe Text
  }

instance ToForm SiteVerifyRequest where
  --toForm (SiteVerifyRequest secret token Nothing) = [ ("secret" @ Text, toQueryParam secret), ("response", toQueryParam token) ]
  toForm (SiteVerifyRequest secret token mRemoteIp) = fromEntriesByKey foo
    where
      foo :: [(Text, [Text])]
      foo = [ ("secret", [secret]), ("response", [token]) ] ++ maybe [] (\x -> [("remoteip",[x])]) mRemoteIp

data SiteVerifyReply = SiteVerifyReply
  { svrSuccess :: Bool
  , svrChallengeTs :: Text
  , svrHostname :: Text
  , svrErrorCodes :: Maybe [Text]
  } deriving (Generic, Show)

instance FromJSON SiteVerifyReply where
  parseJSON = genericParseJSON jsonOptions

--type SiteVerify = "recaptcha" :> "api" :> "siteverify" :> ReqBody '[JSON] SiteVerifyRequest :> Post '[OctetStream] LBS.ByteString
type SiteVerify = "videos" :> "private" :> "test.php" :> ReqBody '[FormUrlEncoded] SiteVerifyRequest :> Post '[JSON] SiteVerifyReply

userAPI :: Proxy RootDir
userAPI = Proxy

recaptchaApi :: Proxy SiteVerify
recaptchaApi = Proxy

siteVerify :: SiteVerifyRequest -> ClientM SiteVerifyReply
siteVerify = client recaptchaApi

doSiteVerify :: Text -> Text -> Maybe Text -> ClientM SiteVerifyReply
doSiteVerify secret token mRemoteIp = do
  res <- siteVerify $ SiteVerifyRequest secret token mRemoteIp
  pure res

run :: IO ()
run = do
  --manager' <- newManager defaultManagerSettings
  manager' <- newTlsManagerWith tlsManagerSettings
  res <- runClientM (doSiteVerify "secret" "token" (Just "1.2.3.4")) (mkClientEnv manager' (BaseUrl Https "ext.earthtools.ca" 443 ""))
  print res

server :: IsShelleyBasedEra era =>
  CardanoEra era
  -> ShelleyBasedEra era
  -> FaucetState era
  -> Server RootDir
server era sbe faucetState = handleSendMoney era sbe faucetState

getRateLimits :: ApiKey -> FaucetConfigFile -> Maybe (Lovelace, Int)
getRateLimits Recaptcha FaucetConfigFile{fcfRecaptchaLimits} = Just fcfRecaptchaLimits
getRateLimits (ApiKey key) FaucetConfigFile{fcfApiKeys} = HM.lookup key fcfApiKeys

insertUsage :: TMVar (Map ApiKey (Map (Either AddressAny SockAddr) UTCTime)) -> ApiKey -> Either AddressAny SockAddr -> UTCTime -> STM ()
insertUsage tmvar apikey addr now = do
  mainMap <- takeTMVar tmvar
  let
    apiKeyMap :: Map (Either AddressAny SockAddr) UTCTime
    apiKeyMap = fromMaybe mempty (Map.lookup apikey mainMap)
    apiKeyMap' :: Map (Either AddressAny SockAddr) UTCTime
    apiKeyMap' = Map.insert addr now apiKeyMap
    mainMap' = Map.insert apikey apiKeyMap' mainMap
  putTMVar tmvar mainMap'

checkRateLimits :: IsCardanoEra era => AddressAny -> SockAddr -> ApiKey -> FaucetState era -> ExceptT FaucetError IO Lovelace
checkRateLimits addr remoteip apikey FaucetState{fsConfig,fsRateLimitState} = do
  now <- liftIO $ getCurrentTime
  let
    mRateLimits = getRateLimits apikey fsConfig
    recordUsage :: STM ()
    recordUsage = do
      insertUsage fsRateLimitState apikey (Left addr) now
      insertUsage fsRateLimitState apikey (Right remoteip) now
    checkRateLimitsInternal :: Int -> STM Bool
    checkRateLimitsInternal interval = do
      mainMap <- readTMVar fsRateLimitState
      let
        apiKeyMap = fromMaybe mempty (Map.lookup apikey mainMap)
        getLastUsage :: Either AddressAny SockAddr -> Maybe UTCTime
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
      case lastUsage of
        Nothing -> do
          -- this addr has never been used on this api key
          recordUsage
          pure True
        Just when -> do
          pure ()
  case mRateLimits of
    Nothing -> do
      -- api key not found in config
      left $ FaucetErrorRateLimit
    Just (lovelace, interval) -> do
      success <- liftIO $ atomically $ checkRateLimitsInternal interval
      case success of
        True -> pure lovelace
        False -> left FaucetErrorRateLimit

checkRecaptcha :: Monad m => m Bool
checkRecaptcha = pure False

handleSendMoney :: IsShelleyBasedEra era =>
  CardanoEra era
  -> ShelleyBasedEra era
  -> FaucetState era
  -> Text
  -> Maybe Text
  -> SockAddr
  -> Servant.Handler LBS.ByteString
handleSendMoney era sbe fs@FaucetState{network,utxoTMVar,skey,queue} addr mApiKey remoteip = do
  eResult <- liftIO $ runExceptT $ do
    addressAny <- parseAddress addr
    apiKey <- do
      case mApiKey of
        Just key -> pure $ ApiKey key
        Nothing -> do
          recaptcha <- checkRecaptcha
          case recaptcha of
            False -> do
              left FaucetErrorRateLimit
            True -> pure Recaptcha
    lovelace <- checkRateLimits addressAny remoteip apiKey fs
    txinout@(txin,_) <- findUtxoOfSize utxoTMVar lovelace
    putStr @Text "selected the following txin: "
    print txin
    eraInMode <- convertEra era
    -- instead of having to specify an output that is exactly equal to input-fees
    -- i specify no outputs, and set the change addr to the end-user
    unsignedTx <- txBuild sbe defaultCModeParams network txinout [] (TxOutChangeAddress addressAny)
    let
      txid :: TxId
      txid = getTxId unsignedTx
    putStrLn @Text $ "new txid: " <> (show txid)
    signedTx <- withExceptT FaucetErrorTodo $ txSign network unsignedTx [skey]
    let
      prettyTx = friendlyTxBS era signedTx
    liftIO $ atomically $ writeTQueue queue (TxInMode signedTx eraInMode, prettyTx)
    return $ SendMoneyReply txid txin
  case eResult of
    Right msg -> pure $ encode msg
    Left err -> pure . fromString . T.unpack . renderFaucetError $ err
