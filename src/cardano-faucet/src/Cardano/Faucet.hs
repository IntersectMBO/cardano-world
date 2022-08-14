{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TupleSections #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{- HLINT ignore "Use let" -}

module Cardano.Faucet (main, populateStakes) where

import Cardano.Api (TxInMode, CardanoMode, AddressAny, CardanoEra, EraInMode, IsShelleyBasedEra, ShelleyBasedEra, QueryInMode(QueryInEra, QueryCurrentEra), UTxO(unUTxO), QueryUTxOFilter(QueryUTxOByAddress), BlockInMode, ChainPoint, AnyCardanoEra(AnyCardanoEra), CardanoEraStyle(ShelleyBasedEra), LocalNodeConnectInfo(LocalNodeConnectInfo), LocalNodeClientProtocols(LocalNodeClientProtocols, localChainSyncClient, localStateQueryClient, localTxSubmissionClient, localTxMonitoringClient), toEraInMode, ConsensusMode(CardanoMode), QueryInEra(QueryInShelleyBasedEra), QueryInShelleyBasedEra(QueryUTxO, QueryStakeAddresses), LocalStateQueryClient(LocalStateQueryClient), ConsensusModeIsMultiEra(CardanoModeIsMultiEra), cardanoEraStyle, connectToLocalNode, LocalChainSyncClient(NoLocalChainSyncClient), SigningKey(PaymentExtendedSigningKey), getVerificationKey, Lovelace, serialiseAddress)
import Cardano.Api.Byron ()
import Cardano.Api.Shelley (makeStakeAddress, StakeCredential(StakeCredentialByKey), verificationKeyHash, castVerificationKey, SigningKey(StakeExtendedSigningKey), StakeAddress, PoolId, NetworkId, StakeExtendedKey)
import Cardano.CLI.Environment (readEnvSocketPath)
import Cardano.CLI.Shelley.Run.Address
import Cardano.CLI.Shelley.Run.Transaction
import Cardano.CLI.Types
import Cardano.Faucet.Misc
import Cardano.Faucet.Types
import Cardano.Faucet.Utils
import Cardano.Faucet.Web
import Cardano.Prelude hiding ((%))
import Control.Concurrent.STM (newTQueueIO, newEmptyTMVarIO, putTMVar, readTQueue, newTMVarIO)
import Control.Monad.Trans.Except.Exit (orDie)
import Control.Monad.Trans.Except.Extra (left)
import Data.HashMap.Strict qualified as HM
import Data.List.Utils (uniq)
import Data.Set qualified as Set
import qualified Data.Map as Map
import Data.Map.Merge.Lazy as Map
import Data.Text qualified as T
import Network.Wai.Handler.Warp
import Ouroboros.Consensus.HardFork.Combinator.AcrossEras (EraMismatch)
import Ouroboros.Network.Protocol.LocalStateQuery.Client qualified as Net.Query
import Ouroboros.Network.Protocol.LocalStateQuery.Type ()
import Ouroboros.Network.Protocol.LocalTxSubmission.Client qualified as Net.Tx
import Prelude qualified
import Servant
import System.Environment (lookupEnv)
import Cardano.Address.Style.Shelley (getKey, Shelley)
import System.IO (hSetBuffering, BufferMode(LineBuffering))
import Cardano.Address.Derivation (Depth(AccountK), XPrv)
import Formatting ((%), format)
import Formatting.ShortFormatters hiding (x, b, f, l)
import Paths_cardano_faucet (getDataFileName)

app :: IsShelleyBasedEra era =>
  CardanoEra era
  -> ShelleyBasedEra era
  -> FaucetState era
  -> Text
  -> Application
app era sbe faucetState indexHtml = serve userAPI $ server era sbe faucetState indexHtml

startApiServer :: IsShelleyBasedEra era =>
  CardanoEra era
  -> ShelleyBasedEra era
  -> FaucetState era
  -> Port
  -> IO ()
startApiServer era sbe faucetState port = do
  let
    settings = setTimeout 600 $ setPort port $ defaultSettings
  index_path <- getDataFileName "index.html"
  print index_path
  index_html <- readFile index_path
  runSettings settings (app era sbe faucetState index_html)

findAllSizes :: FaucetConfigFile -> [Lovelace]
findAllSizes FaucetConfigFile{fcfRecaptchaLimits,fcfApiKeys} = uniq $ values ++ [v]
  where
    v = akvLovelace fcfRecaptchaLimits
    values :: [Lovelace]
    values = map akvLovelace $ HM.elems fcfApiKeys

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

populateStakes :: IO ()
populateStakes = do
  eResult <- runExceptT $ do
    configFilePath <- liftIO $ lookupEnv "CONFIG_FILE";
    let
      unmaybe :: Maybe Prelude.String -> ExceptT FaucetError IO Prelude.String
      unmaybe (Just path) = pure path
      unmaybe Nothing = left FaucetErrorConfigFileNotSet
    bar <- unmaybe configFilePath
    config <- parseConfig bar
    rootK <- mnemonicToRootKey $ fcfMnemonic config
    acctK <- rootKeytoAcctKey rootK 0x80000000
    let
      net = fcfNetwork config
    print $ map (deriveSingleKey net acctK) [0..10]
    pure ()
  case eResult of
    Right _ -> pure ()
    Left err -> putStrLn $ renderFaucetError err
  pure ()

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  txQueue <- newTQueueIO
  dryRun <- maybe False (== "1") <$> lookupEnv "DRY_RUN"
  eResult <- runExceptT $ do
    let
      unmaybe :: Maybe Prelude.String -> ExceptT FaucetError IO Prelude.String
      unmaybe (Just path) = pure path
      unmaybe Nothing = left FaucetErrorConfigFileNotSet
    configFilePath <- liftIO $ lookupEnv "CONFIG_FILE";
    mportString <- liftIO $ lookupEnv "PORT"
    let
      portString = maybe "8090" Prelude.id mportString
      port = Prelude.read $ portString
    bar <- unmaybe configFilePath
    config <- parseConfig bar
    rootK <- mnemonicToRootKey $ fcfMnemonic config
    acctK <- rootKeytoAcctKey rootK 0x80000000
    addrK <- accountKeyToPaymentKey acctK 0x14
    let
      pay_skey = PaymentExtendedSigningKey $ getKey addrK
      pay_vkey = getVerificationKey pay_skey
      net = fcfNetwork config
    SocketPath sockPath <- withExceptT FaucetErrorSocketNotFound readEnvSocketPath
    let
      localNodeConnInfo :: LocalNodeConnectInfo CardanoMode
      localNodeConnInfo = LocalNodeConnectInfo defaultCModeParams net sockPath
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
      getUtxoQuery :: AddressAny -> ShelleyBasedEra era2 -> Maybe (EraInMode era2 mode) ->  QueryInMode mode (Either EraMismatch (UTxO era2))
      getUtxoQuery _address _sbe Nothing = Prelude.error "not handled"
      getUtxoQuery address sbe (Just eInMode) = QueryInEra eInMode query
        where
          qfilter :: QueryUTxOFilter
          qfilter = QueryUTxOByAddress $ Set.singleton address
          query   = QueryInShelleyBasedEra sbe (QueryUTxO qfilter)

      queryManyStakeAddr :: ShelleyBasedEra era -> Maybe (EraInMode era mode) -> [StakeCredential] -> QueryInMode mode (Either EraMismatch (Map StakeAddress Lovelace, Map StakeAddress PoolId))
      queryManyStakeAddr _sbe Nothing _ = Prelude.error "not handled"
      queryManyStakeAddr sbe (Just eInMode) creds = QueryInEra eInMode (QueryInShelleyBasedEra sbe (QueryStakeAddresses (Set.fromList creds) net))

      finish = do
        void . forever $ threadDelay 43200 {- day in seconds -}
        pure $ Net.Query.SendMsgRelease $
          pure $ Net.Query.SendMsgDone ()

      queryClient :: Net.Query.LocalStateQueryClient (BlockInMode CardanoMode) ChainPoint (QueryInMode CardanoMode) IO ()
      queryClient = LocalStateQueryClient $ do
        aquireConnection $ do
          runQueryThen (QueryCurrentEra CardanoModeIsMultiEra) $ \(AnyCardanoEra era3) -> do
            tmvar <- newEmptyTMVarIO
            stakeTmvar <- newEmptyTMVarIO
            rateLimitTmvar <- newTMVarIO mempty
            let
              faucetState = FaucetState
                { utxoTMVar = tmvar
                , stakeTMVar = stakeTmvar
                , network = net
                , queue = txQueue
                , skey = APaymentExtendedSigningKey pay_skey
                , vkey = APaymentExtendedVerificationKey pay_vkey
                , fsAcctKey = acctK
                , fsConfig = config
                , fsRateLimitState = rateLimitTmvar
                , fsBucketSizes = findAllSizes config
                }
            putStrLn @Text "lovelace values for api keys"
            print $ fsBucketSizes faucetState
            addressAny <- orDie (T.pack . Prelude.show) $ vkeyToAddr (network faucetState) (vkey faucetState)
            putStrLn $ "faucet address: " <> serialiseAddress addressAny
            case cardanoEraStyle era3 of
              ShelleyBasedEra sbe -> do
                _child <- forkIO $ startApiServer era3 sbe faucetState port
                runQueryThen (getUtxoQuery addressAny sbe (toEraInMode era3 CardanoMode)) $ \case
                  Right result -> do
                    let
                      --reduceTxo :: TxOut ctx era -> (Lovelace, TxOut ctx era)
                      --reduceTxo out@(TxOut _ value _ _) = (getValue value, out)
                      --reducedUtxo :: Map TxIn (Lovelace, TxOut CtxUTxO era)
                      --reducedUtxo = Map.map reduceTxo $ unUTxO result
                    --atomically $ putTMVar utxoTMVar $ unUTxO result
                    let stats = computeUtxoStats (unUTxO result)
                    print stats
                    atomically $ putTMVar (utxoTMVar faucetState) (unUTxO result)
                    putStrLn @Text "utxo set initialized"

                    case fcfMaxStakeKeyIndex config of
                      Nothing -> finish
                      Just count -> do
                        let
                          manyStakeKeys :: Map StakeAddress (Word32, SigningKey StakeExtendedKey, StakeCredential)
                          manyStakeKeys = createManyStakeKeys acctK net count
                          x :: [StakeCredential]
                          x = Map.elems $ map (\(_,_,v) -> v) manyStakeKeys
                        runQueryThen (queryManyStakeAddr sbe (toEraInMode era3 CardanoMode) x) $ \case
                          Right (registeredStakeKeys, delegatedStakeKeys) -> do
                            let
                              -- this key is not delegated
                              onlyRegistered :: StakeAddress -> StakeKeyIntermediateState -> StakeKeyState
                              -- and is registered, then we can use it
                              onlyRegistered _key (StakeKeyIntermediateStateRegistered (index, skey, vkey, reward)) = StakeKeyRegistered index skey vkey reward
                              -- but isnt registered
                              onlyRegistered _key (StakeKeyIntermediateStateNotRegistered index) = StakeKeyNotRegistered index
                              --registeredAndDelegated = zipWithMaybeAMatched $ \_ v1 v2 -> pure $ Just $ StakeKeyDelegated v1 v2
                              -- this key is delegated
                              registeredAndDelegated :: StakeAddress -> StakeKeyIntermediateState -> PoolId -> Identity (Maybe StakeKeyState)
                              -- and registered
                              registeredAndDelegated _key (StakeKeyIntermediateStateRegistered (index, _skey, _vkey, rewards)) poolid = pure $ Just $ StakeKeyDelegated index rewards poolid
                              -- delegated but not registered!?
                              registeredAndDelegated _key (StakeKeyIntermediateStateNotRegistered _) _ = pure Nothing
                              --z :: Map StakeAddress StakeKeyState
                              --z = Map.merge onlyRegistered onlyDelegated registeredAndDelegated registeredStakeKeys delegatedStakeKeys

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
                              filterOnlyNotRegistered :: StakeKeyState -> Maybe Word32
                              filterOnlyNotRegistered (StakeKeyNotRegistered index) = Just index
                              filterOnlyNotRegistered _ = Nothing
                              filterOnlyRegistered :: StakeKeyState -> Maybe (Word32, SigningKey StakeExtendedKey, StakeCredential)
                              filterOnlyRegistered (StakeKeyRegistered index skey vkey _rewards) = Just (index, skey, vkey)
                              filterOnlyRegistered _ = Nothing
                              filterOnlyDelegated :: StakeKeyState -> Maybe (Word32, Lovelace, PoolId)
                              filterOnlyDelegated (StakeKeyDelegated index reward poolid) = Just (index, reward, poolid)
                              filterOnlyDelegated _ = Nothing
                              finalMergeValues = Map.elems finalMerge
                              notRegistered :: [Word32]
                              notRegistered = sort $ mapMaybe filterOnlyNotRegistered finalMergeValues
                              notDelegated :: [(Word32, SigningKey StakeExtendedKey, StakeCredential)]
                              notDelegated = mapMaybe filterOnlyRegistered finalMergeValues
                              delegated :: [(Word32, Lovelace, PoolId)]
                              delegated = mapMaybe filterOnlyDelegated finalMergeValues
                            case fcfDebug config of
                              True -> do
                                putStrLn $ format ("these stake key indexes are not registered: " % sh) notRegistered
                                putStrLn $ format ("these stake keys are registered and ready for use: " % sh) $ sort $ map (\(index,_skey,_vkey) -> index) notDelegated
                                putStrLn $ format ("these stake keys are delegated: " % sh) $ sort delegated
                              False -> do
                                putStrLn $ format (d % " stake keys not registered, " % d % " stake keys registered and ready for use, "%d%" stake keys delegated to pools") (length notRegistered) (length notDelegated) (length delegated)
                            atomically $ putTMVar stakeTmvar (notDelegated, map (\(idx,reward,pool) -> (idx,reward,pool)) delegated)
                            finish
                          Left _ -> Prelude.error "not handled"
                  Left _e -> Prelude.error "not handled"
              _ -> Prelude.error "not handled"
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
      submissionClient = Net.Tx.LocalTxSubmissionClient waitForTxAndLoop

    liftIO $ connectToLocalNode
      localNodeConnInfo
      LocalNodeClientProtocols
        { localChainSyncClient    = NoLocalChainSyncClient
        , localStateQueryClient   = Just queryClient
        , localTxSubmissionClient = Just submissionClient
        , localTxMonitoringClient = Nothing
      }
  case eResult of
    Right msg -> print msg
    Left err -> putStrLn $ renderFaucetError err

