{- HLINT ignore "Reduce duplication" -}
{- HLINT ignore "Use uncurry" -}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-} --
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Benchmarking.Script.Core
where

import           Prelude
import           Control.Monad
import           Control.Monad.Trans.Except
import           Control.Monad.IO.Class
import           Control.Concurrent (threadDelay)
import           Control.Tracer (traceWith, nullTracer)

import           Ouroboros.Network.Protocol.LocalTxSubmission.Type (SubmitResult (..))
import           Cardano.Api ( AlonzoEra, AsType(..), CardanoEra(..), InAnyCardanoEra(..), AnyCardanoEra(..), IsShelleyBasedEra, Tx
                             , Lovelace, NetworkId(..), cardanoEra
                             , CardanoMode, LocalNodeConnectInfo
                             , PaymentKey
                             , PlutusScriptVersion(..)
                             , QueryInMode(..)
                             , EraInMode(..)
                             , QueryInEra(..)
                             , QueryInShelleyBasedEra(..)
                             , Script(PlutusScript)
                             , ShelleyBasedEra(..)
                             , SigningKey
                             , TxInMode
                             , TxValidationErrorInMode
                             , TxMetadataInEra (..)
                             , getLocalChainTip, queryNodeLocalState, QueryInMode( QueryCurrentEra), ConsensusModeIsMultiEra( CardanoModeIsMultiEra )
                             , chainTipToChainPoint )
import           Cardano.Api.Shelley ( ProtocolParameters)

import qualified Cardano.Benchmarking.FundSet as FundSet
import           Cardano.Benchmarking.FundSet (AllowRecycle(..), FundInEra(..), Validity(..), Variant(..), liftAnyEra )
import qualified Cardano.Benchmarking.GeneratorTx as GeneratorTx
                   (asyncBenchmark, waitBenchmark, walletBenchmark
                   , readSigningKey, secureGenesisFund, splitFunds, txGenerator)
import           Cardano.Benchmarking.GeneratorTx as GeneratorTx
                   (AsyncBenchmarkControl, TxGenError)

import           Cardano.Benchmarking.GeneratorTx.LocalProtocolDefinition as Core (startProtocol)
import           Cardano.Benchmarking.GeneratorTx.NodeToNode (ConnectClient, benchmarkConnectTxSubmit)
import           Cardano.Benchmarking.GeneratorTx.SizedMetadata (mkMetadata)
import           Cardano.Benchmarking.GeneratorTx.Tx as Core (keyAddress, mkFee, txInModeCardano)

import           Cardano.Benchmarking.OuroborosImports as Core
                   (LocalSubmitTx, SigningKeyFile
                   , getGenesis, protocolToNetworkId, protocolToCodecConfig, makeLocalConnectInfo, submitTxToNodeLocal)
import           Cardano.Benchmarking.PlutusExample as PlutusExample
import           Cardano.Benchmarking.Tracer as Core
                   ( TraceBenchTxSubmit (..)
                   , createTracers, btTxSubmit_, btN2N_, btConnect_, btSubmission2_)
import           Cardano.Benchmarking.Types as Core
                   (NumberOfInputsPerTx(..), NumberOfOutputsPerTx(..),NumberOfTxs(..), SubmissionErrorPolicy(..)
                   , TPSRate, TxAdditionalSize(..))
import           Cardano.Benchmarking.Wallet as Wallet

import           Cardano.Benchmarking.Script.Env
import           Cardano.Benchmarking.Script.Setters
import           Cardano.Benchmarking.Script.Store as Store

liftCoreWithEra :: (forall era. IsShelleyBasedEra era => AsType era -> ExceptT TxGenError IO x) -> ActionM (Either TxGenError x)
liftCoreWithEra coreCall = withEra ( liftIO . runExceptT . coreCall)

withEra :: (forall era. IsShelleyBasedEra era => AsType era -> ActionM x) -> ActionM x
withEra action = do
  era <- get $ User TEra
  case era of
    AnyCardanoEra AlonzoEra  -> action AsAlonzoEra
    AnyCardanoEra MaryEra    -> action AsMaryEra
    AnyCardanoEra AllegraEra -> action AsAllegraEra
    AnyCardanoEra ShelleyEra -> action AsShelleyEra
    AnyCardanoEra ByronEra   -> error "byron not supported"

startProtocol :: FilePath -> ActionM ()
startProtocol filePath = do
  liftIO (runExceptT $ Core.startProtocol filePath) >>= \case
    Left err -> throwE $ CliError err
    Right (loggingLayer, protocol) -> do
      set LoggingLayer loggingLayer
      set Protocol protocol
      set BenchTracers $ Core.createTracers loggingLayer
      set Genesis $ Core.getGenesis protocol
      set NetworkId $ protocolToNetworkId protocol

readSigningKey :: KeyName -> SigningKeyFile -> ActionM ()
readSigningKey name filePath =
  liftIO ( runExceptT $ GeneratorTx.readSigningKey filePath) >>= \case
    Left err -> liftTxGenError err
    Right key -> setName name key

getLocalSubmitTx :: ActionM LocalSubmitTx
getLocalSubmitTx = submitTxToNodeLocal <$> getLocalConnectInfo

--obsolete use importGenesisFund
secureGenesisFund
   :: FundName
   -> KeyName
   -> KeyName
   -> ActionM ()
secureGenesisFund fundName destKey genesisKeyName = do
  tracer <- btTxSubmit_ <$> get BenchTracers
  localSubmit <- getLocalSubmitTx
  networkId <- get NetworkId
  genesis  <- get Genesis
  fee      <- getUser TFee
  ttl      <- getUser TTTL
  fundKey  <- getName destKey
  genesisKey  <- getName genesisKeyName
  let
    coreCall :: forall era. IsShelleyBasedEra era => AsType era -> ExceptT TxGenError IO Store.Fund
    coreCall _proxy = do
      let addr = Core.keyAddress @ era networkId fundKey
      f <- GeneratorTx.secureGenesisFund tracer localSubmit networkId genesis fee ttl genesisKey addr
      return (f, fundKey)
  liftCoreWithEra coreCall >>= \case
    Left err -> liftTxGenError err
    Right fund -> do
      -- Todo : user only of two methods
      setName fundName fund -- Old method

splitFundN
   :: NumberOfTxs
   -> KeyName
   -> FundName
   -> ActionM [Store.Fund]
splitFundN count destKeyName sourceFund = do
  tracer <- btTxSubmit_ <$> get BenchTracers
  localSubmit <- getLocalSubmitTx
  networkId <- get NetworkId
  fee      <- getUser TFee
  destKey  <- getName destKeyName
  (fund, fundKey) <- consumeName sourceFund
  txIn     <- getUser TNumberOfInputsPerTx
  let
    coreCall :: forall era. IsShelleyBasedEra era => AsType era -> ExceptT TxGenError IO [Store.Fund]
    coreCall _proxy = do
      let addr = Core.keyAddress @ era networkId fundKey
      f <- GeneratorTx.splitFunds tracer localSubmit fee count txIn fundKey addr fund
      return $ zip f $ repeat destKey
  liftCoreWithEra coreCall >>= \case
    Left err -> liftTxGenError err
    Right funds -> return funds

splitFund
   :: [FundName]
   -> KeyName
   -> FundName
   -> ActionM ()
splitFund newFunds destKey sourceFund = do
  funds <- splitFundN (NumberOfTxs $ fromIntegral $ length newFunds) destKey sourceFund
  forM_ (zip newFunds funds) $ \(name, f) -> setName name f

splitFundToList
   :: FundListName
   -> KeyName
   -> FundName
   -> ActionM ()
splitFundToList newFunds destKey sourceFund = do
  count <- getUser TNumberOfTxs
  funds <- splitFundN count destKey sourceFund
  setName newFunds funds

delay :: Double -> ActionM ()
delay t = liftIO $ threadDelay $ floor $ 1000000 * t

prepareTxList
   :: TxListName
   -> KeyName
   -> FundListName
   -> ActionM ()
prepareTxList name destKey srcFundName = do
  tracer   <- btTxSubmit_ <$> get BenchTracers
  networkId <- get NetworkId
  fee      <- getUser TFee
  fundList <- consumeName srcFundName
  key      <- getName destKey
  txIn     <- getUser TNumberOfInputsPerTx
  txOut    <- getUser TNumberOfOutputsPerTx
  count    <- getUser TNumberOfTxs
  payload  <- getUser TTxAdditionalSize
  let
    coreCall :: forall era. IsShelleyBasedEra era => AsType era -> ExceptT TxGenError IO (InAnyCardanoEra TxList)
    coreCall _proxy = do
      let addr = Core.keyAddress @ era networkId key
      ----------------------------------------------------TODO : Constant 1 ???
      l <- GeneratorTx.txGenerator tracer fee count txIn txOut payload addr (snd $ head fundList) 1 (map fst fundList)
      return $ InAnyCardanoEra cardanoEra $ TxList l
  liftCoreWithEra coreCall >>= \case
    Left err -> liftTxGenError err
    Right l -> setName name l

waitBenchmarkCore :: AsyncBenchmarkControl ->  ActionM ()
waitBenchmarkCore ctl = do
  tracers  <- get BenchTracers
  _ <- liftIO $ runExceptT $ GeneratorTx.waitBenchmark (btTxSubmit_ tracers) ctl
  return ()

getConnectClient :: ActionM ConnectClient
getConnectClient = do
  tracers  <- get BenchTracers
  (Testnet networkMagic) <- get NetworkId
  protocol <- get Protocol
  void $ return $(btSubmission2_ tracers)
  ioManager <- askIOManager
  return $ benchmarkConnectTxSubmit
                       ioManager
                       (btConnect_ tracers)
                       nullTracer -- (btSubmission2_ tracers)
                       (protocolToCodecConfig protocol)
                       networkMagic

-- This the benchmark based on transaction lists.
-- It is obsolte when the tx-list are replaced with the wallet data type.
asyncBenchmarkCore :: ThreadName -> TxListName -> TPSRate -> ActionM AsyncBenchmarkControl
asyncBenchmarkCore (ThreadName threadName) transactions tps = do
  tracers  <- get BenchTracers
  txs      <- getName transactions
  targets  <- getUser TTargets
  connectClient <- getConnectClient
  let
    coreCall :: forall era. IsShelleyBasedEra era => [Tx era] -> ExceptT TxGenError IO AsyncBenchmarkControl
    coreCall l = GeneratorTx.asyncBenchmark (btTxSubmit_ tracers) (btN2N_ tracers) connectClient threadName targets tps LogErrors l
  ret <- liftIO $ runExceptT $ case txs of
    InAnyCardanoEra AlonzoEra  (TxList l) -> coreCall l
    InAnyCardanoEra MaryEra    (TxList l) -> coreCall l
    InAnyCardanoEra AllegraEra (TxList l) -> coreCall l
    InAnyCardanoEra ShelleyEra (TxList l) -> coreCall l
    InAnyCardanoEra ByronEra   _ -> error "byron not supported"
  case ret of
    Left err -> liftTxGenError err
    Right ctl -> return ctl

asyncBenchmark :: ThreadName -> TxListName -> TPSRate -> ActionM ()
asyncBenchmark controlName txList tps = asyncBenchmarkCore controlName txList tps >>= setName controlName

waitBenchmark :: ThreadName -> ActionM ()
waitBenchmark n = getName n >>= waitBenchmarkCore

cancelBenchmark :: ThreadName -> ActionM ()
cancelBenchmark n = do
  ctl@(_, _ , _ , shutdownAction) <- getName n
  liftIO shutdownAction
  waitBenchmarkCore ctl

getLocalConnectInfo :: ActionM  (LocalNodeConnectInfo CardanoMode)
getLocalConnectInfo = makeLocalConnectInfo <$> get NetworkId <*> getUser TLocalSocket

queryEra :: ActionM AnyCardanoEra
queryEra = do
  localNodeConnectInfo <- getLocalConnectInfo
  chainTip  <- liftIO $ getLocalChainTip localNodeConnectInfo
  ret <- liftIO $ queryNodeLocalState localNodeConnectInfo (Just $ chainTipToChainPoint chainTip) $ QueryCurrentEra CardanoModeIsMultiEra
  case ret of
    Right era -> return era
    Left err -> throwE $ ApiError $ show err

queryProtocolParameters :: ActionM ProtocolParameters
queryProtocolParameters = do
  localNodeConnectInfo <- getLocalConnectInfo
  chainTip  <- liftIO $ getLocalChainTip localNodeConnectInfo
  ret <- liftIO $ queryNodeLocalState localNodeConnectInfo (Just $ chainTipToChainPoint chainTip)
                    $ QueryInEra AlonzoEraInCardanoMode $ QueryInShelleyBasedEra ShelleyBasedEraAlonzo QueryProtocolParameters
  case ret of
    Right (Right pp) -> return pp
    Right (Left err) -> throwE $ ApiError $ show err
    Left err -> throwE $ ApiError $ show err

waitForEra :: AnyCardanoEra -> ActionM ()
waitForEra era = do
  currentEra <- queryEra
  if currentEra == era
    then return ()
    else do
      traceError $ "Current era: " ++ show currentEra ++ " Waiting for: " ++ show era
      liftIO $ threadDelay 1_000_000
      waitForEra era

localTestWalletScript :: forall era.
     IsShelleyBasedEra era
  => WalletScript era
  -> ActionM ()
localTestWalletScript s = do
  step <- liftIO $ runWalletScript s
  case step of
    Done -> return ()
    Error err -> throwE $ ApiError $ show err
    NextTx nextScript tx -> do
      void $ localSubmitTx $ txInModeCardano tx
      localTestWalletScript nextScript

localSubmitTx :: TxInMode CardanoMode -> ActionM (SubmitResult (TxValidationErrorInMode CardanoMode))
localSubmitTx tx = do
  submitTracer <- btTxSubmit_ <$> get BenchTracers
  submit <- getLocalSubmitTx
  ret <- liftIO $ submit tx
  let
    msg = case ret of
      SubmitSuccess -> "local submit success."
      SubmitFail e -> mconcat
        [ "local submit failed: " , show e , " (" , show tx , ")"]
  liftIO $ traceWith submitTracer $ TraceBenchTxSubDebug msg
  return ret

makeMetadata :: forall era. IsShelleyBasedEra era => ActionM (TxMetadataInEra era)
makeMetadata = do
  payloadSize <- getUser TTxAdditionalSize
  case mkMetadata $ unTxAdditionalSize payloadSize of
    Right m -> return m
    Left err -> throwE $ MetadataError err

-- TODO use withEra here!
runBenchmark :: ThreadName -> NumberOfTxs -> TPSRate -> ActionM ()
runBenchmark threadName txCount tps
  = withEra $ runBenchmarkInEra threadName txCount tps

runBenchmarkInEra :: forall era. IsShelleyBasedEra era => ThreadName -> NumberOfTxs -> TPSRate -> AsType era -> ActionM ()
runBenchmarkInEra (ThreadName threadName) txCount tps era = do
  tracers  <- get BenchTracers
  networkId <- get NetworkId
  fundKey <- getName $ KeyName "pass-partout" -- should be walletkey
  targets  <- getUser TTargets
  (NumberOfInputsPerTx   numInputs) <- getUser TNumberOfInputsPerTx
  (NumberOfOutputsPerTx numOutputs) <- getUser TNumberOfOutputsPerTx
  fee <- getUser TFee
  minValuePerUTxO <- getUser TMinValuePerUTxO
  walletRef <- get GlobalWallet
  metadata <- makeMetadata
  connectClient <- getConnectClient
  let
    minTxValue :: Lovelace
    minTxValue = fromIntegral numOutputs * minValuePerUTxO + fee

    selector :: FundSet.Target -> FundSet.FundSelector
    selector = FundSet.selectInputs ReuseAny numInputs minTxValue PlainOldFund

    inToOut :: [Lovelace] -> [Lovelace]
    inToOut = FundSet.inputsToOutputsWithFee fee numOutputs

    txGenerator = genTx fundKey networkId (mkFee fee) metadata

    walletScript :: FundSet.Target -> WalletScript era
    walletScript = benchmarkWalletScript walletRef txGenerator txCount selector inToOut

    coreCall :: AsType era -> ExceptT TxGenError IO AsyncBenchmarkControl
    coreCall eraProxy = GeneratorTx.walletBenchmark (btTxSubmit_ tracers) (btN2N_ tracers) connectClient
                                               threadName targets tps LogErrors eraProxy txCount walletScript
  ret <- liftIO $ runExceptT $ coreCall era
  case ret of
    Left err -> liftTxGenError err
    Right ctl -> do
      setName (ThreadName threadName) ctl

runPlutusBenchmark :: ThreadName -> NumberOfTxs -> TPSRate -> ActionM ()
runPlutusBenchmark (ThreadName threadName) txCount tps = do
  tracers  <- get BenchTracers
  targets  <- getUser TTargets
  networkId <- get NetworkId
  protocolParameters <- queryProtocolParameters
  walletRef <- get GlobalWallet
  fundKey <- getName $ KeyName "pass-partout"
  (PlutusScript PlutusScriptV1 script) <- liftIO $ PlutusExample.readScript "bench/script/sum1ToN.plutus"
  collateral <- liftIO ( askWalletRef walletRef (FundSet.selectCollateral . walletFunds)) >>= \case
    -- TODO !! FIX THIS BUG !
    -- This just selects one UTxO as colleteral, but I have to also remove it from the wallet.
    -- Otherwise it may be spend accidentially
    Right c -> return c
    Left err -> throwE $ WalletError err
  connectClient <- getConnectClient
  let
    walletScript :: FundSet.Target -> WalletScript AlonzoEra
    walletScript = plutusWalletScript fundKey script networkId protocolParameters collateral walletRef txCount

--  TODO: this is useful for debugging: add to JSON scripting language
--  localTestWalletScript $ walletScript $ FundSet.Target "local"

  ret <- liftIO $ runExceptT $
           GeneratorTx.walletBenchmark (btTxSubmit_ tracers) (btN2N_ tracers) connectClient
                                       threadName targets tps LogErrors AsAlonzoEra txCount walletScript
  case ret of
    Left err -> liftTxGenError err
    Right ctl -> do
      setName (ThreadName threadName) ctl

-- Todo: make it possible to import several funds
-- (Split init and import)
importGenesisFund
   :: KeyName
   -> KeyName
   -> ActionM ()
importGenesisFund genesisKeyName destKey= do
  tracer <- btTxSubmit_ <$> get BenchTracers
  localSubmit <- getLocalSubmitTx
  networkId <- get NetworkId
  genesis  <- get Genesis
  fee      <- getUser TFee
  ttl      <- getUser TTTL
  fundKey  <- getName destKey
  genesisKey  <- getName genesisKeyName
  let
    coreCall :: forall era. IsShelleyBasedEra era => AsType era -> ExceptT TxGenError IO Store.Fund
    coreCall _proxy = do
      let addr = Core.keyAddress @ era networkId fundKey
      f <- GeneratorTx.secureGenesisFund tracer localSubmit networkId genesis fee ttl genesisKey addr
      return (f, fundKey)
  liftCoreWithEra coreCall >>= \case
    Left err -> liftTxGenError err
    Right fund -> initGlobalWallet networkId fundKey fund

-- Todo split init and import of funds
initGlobalWallet :: NetworkId -> SigningKey PaymentKey -> Fund -> ActionM ()
initGlobalWallet networkId key ((txIn, outVal), skey) = do
  wallet <- liftIO $ initWallet networkId key
  liftIO (walletRefInsertFund wallet (FundSet.Fund $ mkFund outVal))
  set GlobalWallet wallet
 where
  mkFund = liftAnyEra $ \value -> FundInEra {
    _fundTxIn = txIn
  , _fundVal = value
  , _fundSigningKey = skey
  , _fundValidity = Confirmed
  , _fundVariant = PlainOldFund
  }

localCreateScriptFunds :: Lovelace -> Int -> ActionM ()
localCreateScriptFunds value count = do
  walletRef <- get GlobalWallet
  networkId <- get NetworkId
  fundKey <- getName $ KeyName "pass-partout"
  fee <- getUser TFee  
  let scriptData = PlutusExample.toScriptHash "e88bd757ad5b9bedf372d8d3f0cf6c962a469db61a265f6418e1ffed86da29ec"
  script <- liftIO $ PlutusExample.readScript "bench/script/sum1ToN.plutus"
  let
    createCoins coins = do
      let
        selector :: FundSet.FundSelector
        selector = FundSet.selectMinValue $ sum coins + fee
        inOut :: [Lovelace] -> [Lovelace]
        inOut = Wallet.includeChange fee coins        
      tx <- liftIO $ modifyWalletRefEither walletRef (walletCreateCoins (PlutusExample.payToScript fundKey (script, scriptData) networkId) selector inOut)
      return $ fmap txInModeCardano tx
  createChangeGeneric createCoins value count

createChange :: Lovelace -> Int -> ActionM ()
createChange value count = withEra $ createChangeInEra value count

createChangeInEra :: forall era. IsShelleyBasedEra era => Lovelace -> Int -> AsType era -> ActionM ()
createChangeInEra value count _proxy = do
  networkId <- get NetworkId
  fundKey <- getName $ KeyName "pass-partout"
  fee <- getUser TFee
  walletRef <- get GlobalWallet
  let
    createCoins :: [Lovelace] -> ActionM (Either String (TxInMode CardanoMode))
    createCoins coins = do
      let
        selector :: FundSet.FundSelector
        selector = FundSet.selectMinValue $ sum coins + fee
        inOut :: [Lovelace] -> [Lovelace]
        inOut = Wallet.includeChange fee coins
        
      (tx :: Either String (Tx era)) <- liftIO $ modifyWalletRefEither walletRef (walletCreateCoins (genTx fundKey networkId (mkFee fee) TxMetadataNone) selector inOut)
      return $ fmap txInModeCardano tx
  createChangeGeneric createCoins value count

createChangeGeneric :: ([Lovelace] -> ActionM (Either String (TxInMode CardanoMode))) -> Lovelace -> Int -> ActionM ()
createChangeGeneric createCoins value count = do
  let
    coinsList = replicate count value
    maxTxSize = 30
    chunks = chunkList maxTxSize coinsList
  forM_ chunks $ \coins -> do
    gen <- createCoins coins
    case gen of
      Left err -> throwE $ WalletError err
      Right tx -> void $ localSubmitTx tx
 where
  chunkList :: Int -> [a] -> [[a]]
  chunkList _ [] = []
  chunkList n xs = as : chunkList n bs where (as,bs) = splitAt n xs

{-
This is for dirty hacking and testing and quick-fixes.
Its a function that can be called from the JSON scripts
and for which the JSON encoding is "reserved".
-}
{-
reserved :: [String] -> ActionM ()
reserved _ = do
  throwE $ UserError "no dirty hack is implemented"

-}
reserved :: [String] -> ActionM ()
reserved _ = do
  -- create some regular change first
  -- genesis holds  100000000000000
  createChange            800000000000 100
  createChange              1492000000 1 -- magic value to tag collateral UTxO
   -- max-tx-size 30 => ca 66 transcaction to create 2000 outputs
  localCreateScriptFunds   20000000000 2000
  delay 60
  runPlutusBenchmark (ThreadName "plutusBenchmark") 1000 10
  waitBenchmark (ThreadName "plutusBenchmark")
  return ()
