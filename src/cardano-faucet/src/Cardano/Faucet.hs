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
import Cardano.Api (TxInMode, CardanoMode, AddressAny, EraInMode, IsShelleyBasedEra, QueryInMode(QueryInEra, QueryCurrentEra), UTxO(unUTxO), QueryUTxOFilter(QueryUTxOByAddress, QueryUTxOWhole), BlockInMode, ChainPoint, AnyCardanoEra(AnyCardanoEra), CardanoEraStyle(ShelleyBasedEra), LocalNodeConnectInfo(LocalNodeConnectInfo), LocalNodeClientProtocols(LocalNodeClientProtocols, localChainSyncClient, localStateQueryClient, localTxSubmissionClient, localTxMonitoringClient), toEraInMode, ConsensusMode(CardanoMode), QueryInEra(QueryInShelleyBasedEra), QueryInShelleyBasedEra(QueryUTxO, QueryStakeAddresses), LocalStateQueryClient(LocalStateQueryClient), ConsensusModeIsMultiEra(CardanoModeIsMultiEra), cardanoEraStyle, connectToLocalNode, LocalChainSyncClient(NoLocalChainSyncClient), SigningKey(PaymentExtendedSigningKey), getVerificationKey, Lovelace, serialiseAddress)
--import Cardano.CLI.Run.Friendly (friendlyTxBS)
import Cardano.Api.Shelley (makeStakeAddress, StakeCredential(StakeCredentialByKey), verificationKeyHash, castVerificationKey, SigningKey(StakeExtendedSigningKey), StakeAddress, PoolId, NetworkId, StakeExtendedKey, queryExpr, LocalStateQueryExpr, determineEraExpr, CardanoEra, CardanoEra(ShelleyEra, AllegraEra, AlonzoEra, MaryEra, BabbageEra, ByronEra), shelleyBasedEra, IsCardanoEra, LocalTxMonitorClient(..), SlotNo, makeStakeAddressRegistrationCertificate, executeLocalStateQueryExpr, TxOut(TxOut), AddressInEra(AddressInEra), Address(ShelleyAddress), TxOutValue(TxOutValue, TxOutAdaOnly), fromShelleyStakeCredential, selectLovelace)
import Cardano.CLI.Environment (readEnvSocketPath)
import Cardano.CLI.Shelley.Commands
import Cardano.CLI.Shelley.Key
import Cardano.CLI.Shelley.Run.Address
import Cardano.CLI.Shelley.Run.Transaction
import Cardano.CLI.Types
import Cardano.Faucet.Misc
import Cardano.Faucet.Types
import Cardano.Faucet.Utils
import Cardano.Faucet.Web
import Cardano.Ledger.Credential qualified as Shelley
import Cardano.Prelude hiding ((%))
import Control.Concurrent.STM (newTQueueIO, newEmptyTMVarIO, putTMVar, readTQueue, newTMVarIO, TQueue)
import Control.Monad.Trans.Except.Exit (orDie)
import Control.Monad.Trans.Except.Extra (left)
import Data.List.Utils (uniq)
import Data.Map qualified as Map
import Data.Map.Merge.Lazy as Map
import Data.Maybe
import Data.Set qualified as Set
import Data.Text qualified as T
import Formatting ((%), format)
import Formatting.ShortFormatters hiding (x, b, f, l)
import Network.Wai.Handler.Warp
import Ouroboros.Consensus.HardFork.Combinator.AcrossEras (EraMismatch)
import Ouroboros.Network.Protocol.LocalStateQuery.Client qualified as Net.Query
import Ouroboros.Network.Protocol.LocalStateQuery.Type (AcquireFailure)
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

createManyStakeKeys :: Shelley 'AccountK XPrv -> NetworkId -> Word32 -> ManyStakeKeys
createManyStakeKeys acctK net count = ManyStakeKeys $ Map.fromList $ map f indexRange
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
    SocketPath sockPath <- withExceptT FaucetErrorSocketNotFound readEnvSocketPath

    rich_vkey <- liftIO $ loadvkey "/home/clever/iohk/cardano-node/pay.vkey"
    rich_addr <- withExceptT FaucetErrorShelleyAddr $ vkeyToAddr (fcfNetwork config) rich_vkey
    putStrLn $ format  ("rich addr: " % sh) $ serialiseAddress rich_addr
    let
      acctK = rootKeytoAcctKey rootK 0x80000000
      net = fcfNetwork config
      localNodeConnInfo = LocalNodeConnectInfo defaultCModeParams net sockPath
    case fcfMaxStakeKeyIndex config of
      Just count -> do
        let
          manyStakeKeys = createManyStakeKeys acctK net count
        eResult <- liftIO $ runTest localNodeConnInfo net manyStakeKeys rich_addr
        case eResult of
          Right (notRegistered, notDelegated, delegated, stakeDistrib) -> do
            putStrLn $ format ("these stake key indexes are not registered: " % sh) $ sort $ stakeAddressesToIndex manyStakeKeys notRegistered
            putStrLn $ format ("these stake keys are registered and ready for use: " % sh) $ sort $ stakeAddressesToIndex manyStakeKeys notDelegated
            liftIO $ showDelegatedStakeKeys manyStakeKeys delegated
            --mapM_ (putStrLn . encode) notRegistered
            when True $ mapM_ (printStakeKey stakeDistrib) $ sortOn (\(_,idx,_,_) -> idx) $ stakeAddressLookup manyStakeKeys Prelude.id notDelegated
            pure ()
          Left _ -> do
            putStrLn @Text "error querying node"
            pure ()
      Nothing -> do
        putStrLn @Text "staking disabled in config"
    pure ()
  case eResult of
    Right _ -> pure ()
    Left err -> putStrLn $ renderFaucetError err
  where
    convertValue :: TxOutValue era -> Lovelace
    convertValue (TxOutValue _ val) = selectLovelace val
    convertValue (TxOutAdaOnly _ lovelace) = lovelace

    convertKey :: (TxOutValue era, StakeCredential) -> (StakeCredential, Lovelace)
    convertKey (val, cred) = (cred, convertValue val)

    totalUpStake :: [(StakeCredential, Lovelace)] -> Map StakeCredential Lovelace
    totalUpStake input = Map.fromListWith (<>) input

    hasStakeKey :: TxOut ctx era -> Maybe (TxOutValue era, StakeCredential)
    hasStakeKey (TxOut (AddressInEra _ addr) val _ _) = do
      ShelleyAddress _ _ (Shelley.StakeRefBase cred) <- Just addr
      Just (val, fromShelleyStakeCredential cred)

    printStakeKey :: Map StakeCredential Lovelace -> (StakeAddress, Word32, SigningKey StakeExtendedKey, StakeCredential) -> ExceptT FaucetError IO ()
    printStakeKey stakeDistrib (addr, index, _skey, creds) = do
      putStrLn $ format ("index: "%sh%" addr: "%sh%" stake: "%sh) index (serialiseAddress addr) (Map.lookup creds stakeDistrib)

    runTest :: LocalNodeConnectInfo CardanoMode -> NetworkId -> ManyStakeKeys -> AddressAny -> IO (Either AcquireFailure ([StakeAddress], [StakeAddress], [(StakeAddress, Lovelace, PoolId)], Map StakeCredential Lovelace))
    runTest localNodeConnInfo net manyStakeKeys rich_addr = do
      executeLocalStateQueryExpr localNodeConnInfo Nothing $ \_ntcVersion -> populateQuery net manyStakeKeys rich_addr
    populateQuery :: NetworkId -> ManyStakeKeys -> AddressAny -> LocalStateQueryExpr block point (QueryInMode CardanoMode) r IO ([StakeAddress], [StakeAddress], [(StakeAddress, Lovelace, PoolId)], Map StakeCredential Lovelace)
    populateQuery net manyStakeKeys rich_addr = do
      rawEra <- determineEraExpr defaultCModeParams
      withEra rawEra $ \era -> do
        let
          allCredentials :: [StakeCredential]
          allCredentials = map (\(_index, _skey, cred) -> cred) $ Map.elems $ unMany manyStakeKeys
          sbe = shelleyBasedEra
          eraInMode = fromJust $ toEraInMode era CardanoMode
        eRichUtxo <- queryExpr $ QueryInEra eraInMode $ QueryInShelleyBasedEra sbe $ QueryUTxO $ QueryUTxOByAddress $ Set.singleton rich_addr
        print eRichUtxo
        eEntireUtxo <- queryExpr $ QueryInEra eraInMode $ QueryInShelleyBasedEra sbe $ QueryUTxO QueryUTxOWhole
        stakeDistrib <- case eEntireUtxo of
          Right entireUtxo -> do
            let
              utxoWithStake = mapMaybe hasStakeKey $ Map.elems $ unUTxO entireUtxo
              converted = map convertKey utxoWithStake
              stakeDistrib :: Map StakeCredential Lovelace
              stakeDistrib = totalUpStake converted
            pure stakeDistrib
          Left _ -> do
            putStrLn @Text "failed to query utxo"
            pure mempty
        eResult <- queryExpr (queryManyStakeAddr net (toEraInMode era CardanoMode) allCredentials)
        (notRegistered, notDelegated, delegated) <- case eResult of
          Right result -> do
            let
              (notRegistered, notDelegated, delegated) = sortStakeKeys result manyStakeKeys
            pure (notRegistered, notDelegated, delegated)
          Left _ -> do
            putStrLn @Text "failed to query stake keys"
            pure $ ([], [], [])
        pure (notRegistered, notDelegated, delegated, stakeDistrib)

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

stakeAddressesToIndex :: ManyStakeKeys -> [StakeAddress] -> [Word32]
stakeAddressesToIndex (ManyStakeKeys m) ks = map (\(a,_,_) -> a) $ mapMaybe (\k -> Map.lookup k m) ks

stakeAddressLookup :: ManyStakeKeys -> ((StakeAddress, Word32, SigningKey StakeExtendedKey, StakeCredential) -> a) -> [StakeAddress] -> [a]
stakeAddressLookup (ManyStakeKeys m) fn ks = map fn $ mapMaybe fixMaybe $ map (\k -> (k, Map.lookup k m)) ks
  where
    fixMaybe :: (StakeAddress, Maybe (Word32, SigningKey StakeExtendedKey, StakeCredential)) -> Maybe (StakeAddress, Word32, SigningKey StakeExtendedKey, StakeCredential)
    fixMaybe (_, Nothing) = Nothing
    fixMaybe (addr, Just (index, skey, cred)) = Just (addr, index, skey, cred)

showDelegatedStakeKeys :: ManyStakeKeys -> [(StakeAddress, Lovelace, PoolId)] -> IO ()
showDelegatedStakeKeys manyStakeKeys delegated = putStrLn $ format ("these stake keys are delegated: " % sh) $ sort $ stakeAddressesToIndex manyStakeKeys $ map (\(a,_,_) -> a) delegated

sortStakeKeys :: (Map StakeAddress Lovelace, Map StakeAddress PoolId) -> ManyStakeKeys -> ([StakeAddress],[StakeAddress],[(StakeAddress, Lovelace, PoolId)])
sortStakeKeys (registeredStakeKeys, delegatedStakeKeys) (ManyStakeKeys manyStakeKeys) = do
  let
    intermediateMerge :: Map StakeAddress StakeKeyIntermediateState
    intermediateMerge = Map.merge
      (mapMissing $ \_ _ -> StakeKeyIntermediateStateNotRegistered)
      dropMissing
      (zipWithMaybeAMatched $ \_key (_index, _skey, _vkey) reward -> pure $ Just $ StakeKeyIntermediateStateRegistered reward)
      manyStakeKeys registeredStakeKeys

    finalMerge :: Map StakeAddress StakeKeyState
    finalMerge = Map.merge
      (mapMissing $ onlyRegistered)
      dropMissing
      (zipWithMaybeAMatched registeredAndDelegated)
      intermediateMerge
      delegatedStakeKeys

    finalMergeValues = Map.elems finalMerge

    notRegistered :: [StakeAddress]
    notRegistered = sort $ mapMaybe filterOnlyNotRegistered finalMergeValues

    notDelegated :: [StakeAddress]
    notDelegated = mapMaybe filterOnlyRegistered finalMergeValues

    delegated :: [(StakeAddress, Lovelace, PoolId)]
    delegated = mapMaybe filterOnlyDelegated finalMergeValues
  (notRegistered,notDelegated,delegated)
  where
    -- this key is not delegated
    onlyRegistered :: StakeAddress -> StakeKeyIntermediateState -> StakeKeyState
    -- and is registered, then we can use it
    onlyRegistered key (StakeKeyIntermediateStateRegistered _) = StakeKeyRegistered key
    -- but isnt registered
    onlyRegistered key StakeKeyIntermediateStateNotRegistered = StakeKeyNotRegistered key
    -- this key is delegated
    registeredAndDelegated :: StakeAddress -> StakeKeyIntermediateState -> PoolId -> Identity (Maybe StakeKeyState)
    -- and registered
    registeredAndDelegated key (StakeKeyIntermediateStateRegistered rewards) poolid = pure $ Just $ StakeKeyDelegated key rewards poolid
    -- delegated but not registered!?
    registeredAndDelegated _key StakeKeyIntermediateStateNotRegistered _ = pure Nothing

    filterOnlyNotRegistered :: StakeKeyState -> Maybe StakeAddress
    filterOnlyNotRegistered (StakeKeyNotRegistered addr) = Just addr
    filterOnlyNotRegistered _ = Nothing
    filterOnlyRegistered :: StakeKeyState -> Maybe StakeAddress
    filterOnlyRegistered (StakeKeyRegistered addr) = Just addr
    filterOnlyRegistered _ = Nothing
    filterOnlyDelegated :: StakeKeyState -> Maybe (StakeAddress, Lovelace, PoolId)
    filterOnlyDelegated (StakeKeyDelegated addr reward poolid) = Just (addr, reward, poolid)
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
          manyStakeKeys = createManyStakeKeys (fsAcctKey faucetState) (fcfNetwork config) count
          x :: [StakeCredential]
          x = Map.elems $ map (\(_,_,v) -> v) $ unMany manyStakeKeys
        eResult <- queryExpr (queryManyStakeAddr (fcfNetwork config) (toEraInMode era CardanoMode) x)
        print eResult
        case eResult of
          Right result -> do
            let
              (notRegistered, notDelegated, delegated) = sortStakeKeys result manyStakeKeys
            case fcfDebug config of
              True -> do
                putStrLn $ format ("these stake key indexes are not registered: " % sh) $ stakeAddressesToIndex manyStakeKeys notRegistered
                putStrLn $ format ("these stake keys are registered and ready for use: " % sh) $ sort $ stakeAddressesToIndex manyStakeKeys notDelegated
                liftIO $ showDelegatedStakeKeys manyStakeKeys delegated
              False -> do
                putStrLn $ format (d % " stake keys not registered, " % d % " stake keys registered and ready for use, "%d%" stake keys delegated to pools") (length notRegistered) (length notDelegated) (length delegated)
            let
              delegatedIndexes :: [Word32]
              delegatedIndexes = stakeAddressesToIndex manyStakeKeys (map (\(a,_,_) -> a) delegated)
              delegationInfo :: [(Lovelace, PoolId)]
              delegationInfo = map (\(_, a, b) -> (a,b)) delegated
              reassembled = map (\(a, (b, pool)) -> (a,b,pool)) $ zip delegatedIndexes delegationInfo
            liftIO $ atomically $ putTMVar (fsStakeTMVar faucetState) (stakeAddressLookup manyStakeKeys (\(_,a,b,cred) -> (a,b,cred)) notDelegated, reassembled)
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
                    manyStakeKeys = createManyStakeKeys (fsAcctKey faucetState) (fcfNetwork config) count
                    x :: [StakeCredential]
                    x = Map.elems $ map (\(_,_,v) -> v) $ unMany manyStakeKeys
                  print $ map (\(idx,_skey,cred) -> (idx,makeStakeAddressRegistrationCertificate cred)) $ Map.elems $ unMany manyStakeKeys
                  runQueryThen (queryManyStakeAddr (fcfNetwork config) (toEraInMode era3 CardanoMode) x) $ \case
                    Right stakeKeyResults -> do
                      let (notRegistered,notDelegated,delegated) = sortStakeKeys stakeKeyResults manyStakeKeys

                      case fcfDebug config of
                        True -> do
                          putStrLn $ format ("these stake key indexes are not registered: " % sh) $ stakeAddressesToIndex manyStakeKeys notRegistered
                          putStrLn $ format ("these stake keys are registered and ready for use: " % sh) $ sort $ stakeAddressesToIndex manyStakeKeys notDelegated
                          liftIO $ showDelegatedStakeKeys manyStakeKeys delegated
                        False -> do
                          putStrLn $ format (d % " stake keys not registered, " % d % " stake keys registered and ready for use, "%d%" stake keys delegated to pools") (length notRegistered) (length notDelegated) (length delegated)
                      let
                        delegatedIndexes :: [Word32]
                        delegatedIndexes = stakeAddressesToIndex manyStakeKeys (map (\(a,_,_) -> a) delegated)
                        delegationInfo :: [(Lovelace, PoolId)]
                        delegationInfo = map (\(_, a, b) -> (a,b)) delegated
                        reassembled = map (\(a, (b, pool)) -> (a,b,pool)) $ zip delegatedIndexes delegationInfo
                      atomically $ putTMVar (fsStakeTMVar faucetState) (stakeAddressLookup manyStakeKeys (\(_,a,b,cred) -> (a,b,cred)) notDelegated, reassembled)
                      finish
                    Left _ -> Prelude.error "not handled"
            Left _e -> Prelude.error "not handled"
        _ -> Prelude.error "not handled"

txMonitor :: FaucetConfigFile -> LocalTxMonitorClient txid (TxInMode CardanoMode) SlotNo IO a
txMonitor FaucetConfigFile{fcfDebug} = LocalTxMonitorClient $ return $ CTxMon.SendMsgAcquire getSnapshot
  where
    getSnapshot :: SlotNo -> IO (CTxMon.ClientStAcquired txid1 (TxInMode CardanoMode) SlotNo IO a1)
    getSnapshot slot = do
      when fcfDebug $ do
        putStrLn $ format ("got mempool snapshot at slot " % sh) $ slot
      return $ CTxMon.SendMsgNextTx getNextTx
    getNextTx :: Show tx => Maybe tx -> IO (CTxMon.ClientStAcquired txid1 (TxInMode CardanoMode) SlotNo IO a1)
    getNextTx (Just tx) = do
      when fcfDebug $ do
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
        , localTxMonitoringClient = Just (txMonitor fsConfig)
      }
  case eResult of
    Right msg -> print msg
    Left err -> putStrLn $ renderFaucetError err

loadvkey :: Prelude.String -> IO SomeAddressVerificationKey
loadvkey filepath = do
  orDie (T.pack . Prelude.show) $ (readAddressVerificationKeyTextOrFile . VktofVerificationKeyFile . VerificationKeyFile) filepath

_loadskey :: IO SomeWitness
_loadskey = do
  let
    foo :: WitnessSigningData
    foo = KeyWitnessSigningData (SigningKeyFile "/home/clever/iohk/cardano-node/pay.skey") Nothing
  orDie (T.pack . Prelude.show) $ readWitnessSigningData foo
