{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TupleSections #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{- HLINT ignore "Use let" -}

module Cardano.Faucet (main) where

import Cardano.Address.Derivation (Depth(AccountK), XPrv)
import Cardano.Address.Style.Shelley (getKey, Shelley)
import Cardano.Api (TxInMode, CardanoMode, AddressAny, EraInMode, IsShelleyBasedEra, QueryInMode(QueryInEra, QueryCurrentEra), UTxO(unUTxO), QueryUTxOFilter(QueryUTxOByAddress), BlockInMode, ChainPoint, AnyCardanoEra(AnyCardanoEra), CardanoEraStyle(ShelleyBasedEra), LocalNodeConnectInfo(LocalNodeConnectInfo), LocalNodeClientProtocols(LocalNodeClientProtocols, localChainSyncClient, localStateQueryClient, localTxSubmissionClient, localTxMonitoringClient), toEraInMode, ConsensusMode(CardanoMode), QueryInEra(QueryInShelleyBasedEra), QueryInShelleyBasedEra(QueryUTxO, QueryStakeAddresses), LocalStateQueryClient(LocalStateQueryClient), ConsensusModeIsMultiEra(CardanoModeIsMultiEra), cardanoEraStyle, connectToLocalNode, LocalChainSyncClient(NoLocalChainSyncClient), SigningKey(PaymentExtendedSigningKey), getVerificationKey, Lovelace, serialiseAddress)
import Cardano.Api.Byron ()
--import Cardano.CLI.Run.Friendly (friendlyTxBS)
import Cardano.Api.Shelley (makeStakeAddress, StakeCredential(StakeCredentialByKey), verificationKeyHash, castVerificationKey, SigningKey(StakeExtendedSigningKey), StakeAddress, PoolId, NetworkId, StakeExtendedKey, queryExpr, LocalStateQueryExpr, determineEraExpr, CardanoEra, CardanoEra(ShelleyEra, AllegraEra, AlonzoEra, MaryEra, BabbageEra, ByronEra), shelleyBasedEra, IsCardanoEra, LocalTxMonitorClient(..), SlotNo)
import Cardano.CLI.Environment (readEnvSocketPath)
import Cardano.CLI.Shelley.Run.Address
import Cardano.CLI.Shelley.Run.Transaction
import Cardano.CLI.Types
import Cardano.Faucet.Misc
import Cardano.Faucet.Types
import Cardano.Faucet.Utils
import Cardano.Faucet.Web
import Cardano.Prelude hiding ((%))
import Control.Concurrent.STM (newTQueueIO, newEmptyTMVarIO, putTMVar, readTQueue, newTMVarIO, TQueue)
import Control.Monad.Trans.Except.Extra (left)
import Data.List.Utils (uniq)
import Data.Map qualified as Map
import Data.Map.Merge.Lazy as Map
import Data.Set qualified as Set
import Formatting ((%), format)
import Formatting.ShortFormatters hiding (x, b, f, l)
import Network.Wai.Handler.Warp
import Ouroboros.Consensus.HardFork.Combinator.AcrossEras (EraMismatch)
import Ouroboros.Network.Protocol.LocalStateQuery.Client qualified as Net.Query
import Ouroboros.Network.Protocol.LocalStateQuery.Type ()
import Ouroboros.Network.Protocol.LocalTxMonitor.Client qualified as CTxMon
import Ouroboros.Network.Protocol.LocalTxSubmission.Client qualified as Net.Tx
import Paths_cardano_faucet (getDataFileName)
import Prelude qualified
import Servant
import System.Environment (lookupEnv)
import System.IO (hSetBuffering, BufferMode(LineBuffering))

app :: IsShelleyBasedEra era =>
  CardanoEra era
  -> FaucetState era
  -> Text
  -> Application
app era faucetState indexHtml = serve userAPI $ server era faucetState indexHtml

startApiServer :: IsShelleyBasedEra era =>
  CardanoEra era
  -> FaucetState era
  -> Port
  -> IO ()
startApiServer era faucetState port = do
  let
    settings = setTimeout 600 $ setPort port $ defaultSettings
  index_path <- getDataFileName "index.html"
  print index_path
  index_html <- readFile index_path
  runSettings settings (app era faucetState index_html)

findAllSizes :: FaucetConfigFile -> [FaucetValue]
findAllSizes FaucetConfigFile{fcfRecaptchaLimits,fcfApiKeys} = uniq $ values
  where
    values :: [FaucetValue]
    values = map toFaucetValue $ (Map.elems fcfApiKeys) ++ (Map.elems fcfRecaptchaLimits)

deriveSingleKey :: NetworkId -> Shelley 'AccountK XPrv -> Word32 -> (SigningKey StakeExtendedKey, StakeCredential, StakeAddress)
deriveSingleKey net acctK stakeIndex = (stake_skey, y, x)
  where
    stakeK = accountKeyToStakeKey acctK stakeIndex
    stake_skey = StakeExtendedSigningKey $ getKey stakeK
    stake_vkey = getVerificationKey stake_skey
    stake_vkey_hash = verificationKeyHash $ castVerificationKey stake_vkey
    y = StakeCredentialByKey stake_vkey_hash
    x = makeStakeAddress net y

createManyStakeKeys :: Shelley 'AccountK XPrv -> NetworkId -> Word32 -> Map StakeAddress (Word32, SigningKey StakeExtendedKey, StakeCredential)
createManyStakeKeys acctK net count = Map.fromList $ map f indexRange
  where
    indexRange = [0..count]
    f :: Word32 -> (StakeAddress, (Word32, SigningKey StakeExtendedKey, StakeCredential))
    f x = (address, (x, skey, vkey))
      where
        (skey, vkey, address) = deriveSingleKey net acctK x

_populateStakes :: IO ()
_populateStakes = do
  eResult <- runExceptT $ do
    configFilePath <- liftIO $ lookupEnv "CONFIG_FILE";
    bar <- unmaybe configFilePath
    config <- parseConfig bar
    rootK <- mnemonicToRootKey $ fcfMnemonic config
    let
      acctK = rootKeytoAcctKey rootK 0x80000000
      net = fcfNetwork config
    print $ map (deriveSingleKey net acctK) [0..10]
    pure ()
  case eResult of
    Right _ -> pure ()
    Left err -> putStrLn $ renderFaucetError err

unmaybe :: Maybe Prelude.String -> ExceptT FaucetError IO Prelude.String
unmaybe (Just path) = pure path
unmaybe Nothing = left FaucetErrorConfigFileNotSet

getUtxoQuery :: forall era2 mode . IsShelleyBasedEra era2 => AddressAny -> Maybe (EraInMode era2 mode) ->  QueryInMode mode (Either EraMismatch (UTxO era2))
getUtxoQuery _address Nothing = Prelude.error "not handled"
getUtxoQuery address (Just eInMode) = QueryInEra eInMode query
  where
    sbe = shelleyBasedEra @era2
    qfilter :: QueryUTxOFilter
    qfilter = QueryUTxOByAddress $ Set.singleton address
    query   = QueryInShelleyBasedEra sbe (QueryUTxO qfilter)

aquireConnection :: Applicative f => m (Net.Query.ClientStAcquired block point query m a) -> f (Net.Query.ClientStIdle block point query m a)
aquireConnection aquireComplete = do
  pure $ Net.Query.SendMsgAcquire Nothing $ Net.Query.ClientStAcquiring
    { Net.Query.recvMsgAcquired = aquireComplete
    , Net.Query.recvMsgFailure = Prelude.error "not implemented"
    }

runQueryThen :: query t -> (t -> IO (Net.Query.ClientStAcquired block point query IO a)) -> IO (Net.Query.ClientStAcquired block point query IO a)
runQueryThen query queryDone = do
  pure $ Net.Query.SendMsgQuery query $
    Net.Query.ClientStQuerying {
      Net.Query.recvMsgResult = \result -> do
        queryDone result
    }

sortStakeKeys :: (Map StakeAddress Lovelace, Map StakeAddress PoolId) -> Map StakeAddress (Word32, SigningKey StakeExtendedKey, StakeCredential) -> ([Word32],[(Word32, SigningKey StakeExtendedKey, StakeCredential)],[(Word32, Lovelace, PoolId)])
sortStakeKeys (registeredStakeKeys, delegatedStakeKeys) manyStakeKeys = do
  let
    intermediateMerge :: Map StakeAddress StakeKeyIntermediateState
    intermediateMerge = Map.merge
      (mapMissing $ \_ (index, _skey, _vkey) -> StakeKeyIntermediateStateNotRegistered index)
      dropMissing
      (zipWithMaybeAMatched $ \_key (index, skey, vkey) reward -> pure $ Just $ StakeKeyIntermediateStateRegistered (index, skey, vkey, reward))
      manyStakeKeys registeredStakeKeys

    finalMerge :: Map StakeAddress StakeKeyState
    finalMerge = Map.merge
      (mapMissing $ onlyRegistered)
      dropMissing
      (zipWithMaybeAMatched registeredAndDelegated)
      intermediateMerge
      delegatedStakeKeys

    finalMergeValues = Map.elems finalMerge

    notRegistered :: [Word32]
    notRegistered = sort $ mapMaybe filterOnlyNotRegistered finalMergeValues

    notDelegated :: [(Word32, SigningKey StakeExtendedKey, StakeCredential)]
    notDelegated = mapMaybe filterOnlyRegistered finalMergeValues

    delegated :: [(Word32, Lovelace, PoolId)]
    delegated = mapMaybe filterOnlyDelegated finalMergeValues
  (notRegistered,notDelegated,delegated)
  where
    -- this key is not delegated
    onlyRegistered :: StakeAddress -> StakeKeyIntermediateState -> StakeKeyState
    -- and is registered, then we can use it
    onlyRegistered _key (StakeKeyIntermediateStateRegistered (index, skey, vkey, reward)) = StakeKeyRegistered index skey vkey reward
    -- but isnt registered
    onlyRegistered _key (StakeKeyIntermediateStateNotRegistered index) = StakeKeyNotRegistered index
    -- this key is delegated
    registeredAndDelegated :: StakeAddress -> StakeKeyIntermediateState -> PoolId -> Identity (Maybe StakeKeyState)
    -- and registered
    registeredAndDelegated _key (StakeKeyIntermediateStateRegistered (index, _skey, _vkey, rewards)) poolid = pure $ Just $ StakeKeyDelegated index rewards poolid
    -- delegated but not registered!?
    registeredAndDelegated _key (StakeKeyIntermediateStateNotRegistered _) _ = pure Nothing

    filterOnlyNotRegistered :: StakeKeyState -> Maybe Word32
    filterOnlyNotRegistered (StakeKeyNotRegistered index) = Just index
    filterOnlyNotRegistered _ = Nothing
    filterOnlyRegistered :: StakeKeyState -> Maybe (Word32, SigningKey StakeExtendedKey, StakeCredential)
    filterOnlyRegistered (StakeKeyRegistered index skey vkey _rewards) = Just (index, skey, vkey)
    filterOnlyRegistered _ = Nothing
    filterOnlyDelegated :: StakeKeyState -> Maybe (Word32, Lovelace, PoolId)
    filterOnlyDelegated (StakeKeyDelegated index reward poolid) = Just (index, reward, poolid)
    filterOnlyDelegated _ = Nothing

submissionClient :: Bool -> TQueue (TxInMode CardanoMode, ByteString) -> Net.Tx.LocalTxSubmissionClient (TxInMode CardanoMode) reject IO a2
submissionClient dryRun txQueue = Net.Tx.LocalTxSubmissionClient waitForTxAndLoop
  where
    waitForTxAndLoop :: IO (Net.Tx.LocalTxClientStIdle (TxInMode CardanoMode) reject IO a)
    waitForTxAndLoop = do
      (tx, prettyTx) <- atomically $ readTQueue txQueue
      case dryRun of
        True -> do
          putStrLn @Text "dry-run, not sending the following tx:"
          putStrLn prettyTx
          waitForTxAndLoop
        False -> pure $ Net.Tx.SendMsgSubmitTx tx $ \_result -> do
          --print result
          waitForTxAndLoop

withEra :: AnyCardanoEra -> (forall era. IsShelleyBasedEra era => CardanoEra era -> a) -> a
withEra (AnyCardanoEra ByronEra) _ = Prelude.error "byron not supported"
withEra (AnyCardanoEra AllegraEra) action = action AllegraEra
withEra (AnyCardanoEra AlonzoEra) action = action AlonzoEra
withEra (AnyCardanoEra BabbageEra) action = action BabbageEra
withEra (AnyCardanoEra MaryEra) action = action MaryEra
withEra (AnyCardanoEra ShelleyEra) action = action ShelleyEra

queryManyStakeAddr :: forall era mode . IsShelleyBasedEra era => NetworkId -> Maybe (EraInMode era mode) -> [StakeCredential] -> QueryInMode mode (Either EraMismatch (Map StakeAddress Lovelace, Map StakeAddress PoolId))
queryManyStakeAddr _ Nothing _ = Prelude.error "not handled"
queryManyStakeAddr network (Just eInMode) creds = QueryInEra eInMode (QueryInShelleyBasedEra sbe (QueryStakeAddresses (Set.fromList creds) network))
  where
    sbe = shelleyBasedEra @era

newFaucetState :: IsCardanoEra era => FaucetConfigFile -> TQueue (TxInMode CardanoMode, ByteString) -> ExceptT FaucetError IO (FaucetState era)
newFaucetState fsConfig fsTxQueue = do
  (fsUtxoTMVar,fsStakeTMVar,fsSendMoneyRateLimitState,fsDelegationRateLimitState) <- liftIO $ (,,,) <$> newEmptyTMVarIO <*> newEmptyTMVarIO <*> newTMVarIO mempty <*> newTMVarIO mempty
  fsRootKey <- mnemonicToRootKey $ fcfMnemonic fsConfig
  let
    fsAcctKey = rootKeytoAcctKey fsRootKey 0x80000000
    addrK = accountKeyToPaymentKey fsAcctKey (fcfAddressIndex fsConfig)
    pay_skey = PaymentExtendedSigningKey $ getKey addrK
    pay_vkey = getVerificationKey pay_skey
    fsPaymentSkey = APaymentExtendedSigningKey pay_skey
    fsPaymentVkey = APaymentExtendedVerificationKey pay_vkey
    fsBucketSizes = findAllSizes fsConfig
    fsNetwork = fcfNetwork fsConfig
  fsOwnAddress <- withExceptT FaucetErrorShelleyAddr $ vkeyToAddr fsNetwork fsPaymentVkey
  pure $ FaucetState{..}

_newQueryClient :: Port -> FaucetConfigFile -> TQueue (TxInMode CardanoMode, ByteString) -> LocalStateQueryExpr block point (QueryInMode CardanoMode) r IO ()
_newQueryClient port config txQueue = do
  rawEra <- determineEraExpr defaultCModeParams
  withEra rawEra $ \era -> do
    eFaucetState <- liftIO $ runExceptT $ newFaucetState config txQueue
    let
      faucetState = fromRight (Prelude.error "cant create state") eFaucetState
    putStrLn $ "faucet address: " <> serialiseAddress (fsOwnAddress faucetState)
    _child <- liftIO $ forkIO $ startApiServer era faucetState port
    eUtxoResult <- queryExpr $ getUtxoQuery (fsOwnAddress faucetState) $ toEraInMode era CardanoMode
    case eUtxoResult of
      Right result -> do
        let stats = computeUtxoStats (unUTxO result)
        print stats
        liftIO $ atomically $ putTMVar (fsUtxoTMVar faucetState) (unUTxO result)
        putStrLn @Text "utxo set initialized"
      Left err -> print err
    case fcfMaxStakeKeyIndex config of
      Just count -> do
        let
          manyStakeKeys :: Map StakeAddress (Word32, SigningKey StakeExtendedKey, StakeCredential)
          manyStakeKeys = createManyStakeKeys (fsAcctKey faucetState) (fcfNetwork config) count
          x :: [StakeCredential]
          x = Map.elems $ map (\(_,_,v) -> v) manyStakeKeys
        eResult <- queryExpr (queryManyStakeAddr (fcfNetwork config) (toEraInMode era CardanoMode) x)
        print eResult
        case eResult of
          Right result -> do
            let
              (notRegistered, notDelegated, delegated) = sortStakeKeys result manyStakeKeys
            case fcfDebug config of
              True -> do
                putStrLn $ format ("these stake key indexes are not registered: " % sh) notRegistered
                putStrLn $ format ("these stake keys are registered and ready for use: " % sh) $ sort $ map (\(index,_skey,_vkey) -> index) notDelegated
                putStrLn $ format ("these stake keys are delegated: " % sh) $ sort delegated
              False -> do
                putStrLn $ format (d % " stake keys not registered, " % d % " stake keys registered and ready for use, "%d%" stake keys delegated to pools") (length notRegistered) (length notDelegated) (length delegated)
            liftIO $ atomically $ putTMVar (fsStakeTMVar faucetState) (notDelegated, delegated)
          Left err -> print err
      Nothing -> pure ()
    pure ()
  pure ()

finish :: IO (Net.Query.ClientStAcquired block point query IO ())
finish = do
  void . forever $ threadDelay 43200 {- day in seconds -}
  pure $ Net.Query.SendMsgRelease $
    pure $ Net.Query.SendMsgDone ()

queryClient :: FaucetConfigFile -> TQueue (TxInMode CardanoMode, ByteString) -> Port -> Net.Query.LocalStateQueryClient (BlockInMode CardanoMode) ChainPoint (QueryInMode CardanoMode) IO ()
queryClient config txQueue port = LocalStateQueryClient $ do
  aquireConnection $ do
    runQueryThen (QueryCurrentEra CardanoModeIsMultiEra) $ \(AnyCardanoEra era3) -> do
      eFaucetState <- liftIO $ runExceptT $ newFaucetState config txQueue
      let
        faucetState = fromRight (Prelude.error "cant create state") eFaucetState
      putStrLn $ format ("lovelace values for api keys " % sh) $ fsBucketSizes faucetState
      putStrLn $ "faucet address: " <> serialiseAddress (fsOwnAddress faucetState)
      case cardanoEraStyle era3 of
        ShelleyBasedEra _ -> do
          _child <- forkIO $ startApiServer era3 faucetState port
          runQueryThen (getUtxoQuery (fsOwnAddress faucetState) (toEraInMode era3 CardanoMode)) $ \case
            Right result -> do
              let
                --reduceTxo :: TxOut ctx era -> (Lovelace, TxOut ctx era)
                --reduceTxo out@(TxOut _ value _ _) = (getValue value, out)
                --reducedUtxo :: Map TxIn (Lovelace, TxOut CtxUTxO era)
                --reducedUtxo = Map.map reduceTxo $ unUTxO result
              --atomically $ putTMVar utxoTMVar $ unUTxO result
              let stats = computeUtxoStats (unUTxO result)
              print stats
              atomically $ putTMVar (fsUtxoTMVar faucetState) (unUTxO result)
              putStrLn @Text "utxo set initialized"

              case fcfMaxStakeKeyIndex config of
                Nothing -> finish
                Just count -> do
                  let
                    manyStakeKeys :: Map StakeAddress (Word32, SigningKey StakeExtendedKey, StakeCredential)
                    manyStakeKeys = createManyStakeKeys (fsAcctKey faucetState) (fcfNetwork config) count
                    x :: [StakeCredential]
                    x = Map.elems $ map (\(_,_,v) -> v) manyStakeKeys
                  runQueryThen (queryManyStakeAddr (fcfNetwork config) (toEraInMode era3 CardanoMode) x) $ \case
                    Right stakeKeyResults -> do
                      let (notRegistered,notDelegated,delegated) = sortStakeKeys stakeKeyResults manyStakeKeys

                      case fcfDebug config of
                        True -> do
                          putStrLn $ format ("these stake key indexes are not registered: " % sh) notRegistered
                          putStrLn $ format ("these stake keys are registered and ready for use: " % sh) $ sort $ map (\(index,_skey,_vkey) -> index) notDelegated
                          putStrLn $ format ("these stake keys are delegated: " % sh) $ sort delegated
                        False -> do
                          putStrLn $ format (d % " stake keys not registered, " % d % " stake keys registered and ready for use, "%d%" stake keys delegated to pools") (length notRegistered) (length notDelegated) (length delegated)
                      atomically $ putTMVar (fsStakeTMVar faucetState) (notDelegated, delegated)
                      finish
                    Left _ -> Prelude.error "not handled"
            Left _e -> Prelude.error "not handled"
        _ -> Prelude.error "not handled"

txMonitor :: LocalTxMonitorClient txid (TxInMode CardanoMode) SlotNo IO a
txMonitor = LocalTxMonitorClient $ return $ CTxMon.SendMsgAcquire getSnapshot
  where
    getSnapshot :: SlotNo -> IO (CTxMon.ClientStAcquired txid1 (TxInMode CardanoMode) SlotNo IO a1)
    getSnapshot slot = do
      putStrLn $ format ("got mempool snapshot at slot " % sh) $ slot
      return $ CTxMon.SendMsgNextTx getNextTx
    getNextTx :: Show tx => Maybe tx -> IO (CTxMon.ClientStAcquired txid1 (TxInMode CardanoMode) SlotNo IO a1)
    getNextTx (Just tx) = do
      putStrLn $ format ("found tx in snapshot: " % sh) $ tx
      return $ CTxMon.SendMsgNextTx getNextTx
    getNextTx Nothing = do
      return $ CTxMon.SendMsgAwaitAcquire getSnapshot

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  fsTxQueue <- newTQueueIO
  dryRun <- maybe False (== "1") <$> lookupEnv "DRY_RUN"
  eResult <- runExceptT $ do
    configFilePath <- liftIO $ lookupEnv "CONFIG_FILE";
    mportString <- liftIO $ lookupEnv "PORT"
    let
      portString = maybe "8090" Prelude.id mportString
      port = Prelude.read $ portString
    bar <- unmaybe configFilePath
    fsConfig <- parseConfig bar
    SocketPath sockPath <- withExceptT FaucetErrorSocketNotFound readEnvSocketPath
    let
      localNodeConnInfo :: LocalNodeConnectInfo CardanoMode
      localNodeConnInfo = LocalNodeConnectInfo defaultCModeParams (fcfNetwork fsConfig) sockPath

    liftIO $ connectToLocalNode
      localNodeConnInfo
      LocalNodeClientProtocols
        { localChainSyncClient    = NoLocalChainSyncClient
        , localStateQueryClient   = Just (queryClient fsConfig fsTxQueue port)
        , localTxSubmissionClient = Just (submissionClient dryRun fsTxQueue)
        , localTxMonitoringClient = Just txMonitor
      }
  case eResult of
    Right msg -> print msg
    Left err -> putStrLn $ renderFaucetError err
