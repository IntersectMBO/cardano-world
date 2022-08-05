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

module Cardano.Faucet (main) where

import Cardano.Api (TxInMode, CardanoMode, AddressAny, CardanoEra, EraInMode, IsShelleyBasedEra, ShelleyBasedEra, QueryInMode(QueryInEra, QueryCurrentEra), UTxO(unUTxO), QueryUTxOFilter(QueryUTxOByAddress), BlockInMode, ChainPoint, AnyCardanoEra(AnyCardanoEra), CardanoEraStyle(ShelleyBasedEra), LocalNodeConnectInfo(LocalNodeConnectInfo), LocalNodeClientProtocols(LocalNodeClientProtocols, localChainSyncClient, localStateQueryClient, localTxSubmissionClient, localTxMonitoringClient), toEraInMode, ConsensusMode(CardanoMode), QueryInEra(QueryInShelleyBasedEra), QueryInShelleyBasedEra(QueryUTxO), LocalStateQueryClient(LocalStateQueryClient), ConsensusModeIsMultiEra(CardanoModeIsMultiEra), cardanoEraStyle, connectToLocalNode, LocalChainSyncClient(NoLocalChainSyncClient), SigningKey(PaymentExtendedSigningKey), getVerificationKey, Lovelace, serialiseAddress)
import Cardano.Api.Byron ()
import Cardano.Api.Shelley ()
import Cardano.CLI.Environment (readEnvSocketPath)
import Cardano.CLI.Shelley.Run.Address
import Cardano.CLI.Shelley.Run.Transaction
import Cardano.CLI.Types
import Cardano.Faucet.Misc
import Cardano.Faucet.Types
import Cardano.Faucet.Utils
import Cardano.Faucet.Web
import Cardano.Prelude
import Control.Concurrent.STM (newTQueueIO, newEmptyTMVarIO, putTMVar, readTQueue, newTMVarIO)
import Control.Monad.Trans.Except.Exit (orDie)
import Control.Monad.Trans.Except.Extra (left)
import Data.HashMap.Strict qualified as HM
import Data.List.Utils (uniq)
import Data.Set qualified as Set
import Data.Text qualified as T
import Network.Wai.Handler.Warp
import Ouroboros.Consensus.HardFork.Combinator.AcrossEras (EraMismatch)
import Ouroboros.Network.Protocol.LocalStateQuery.Client qualified as Net.Query
import Ouroboros.Network.Protocol.LocalStateQuery.Type ()
import Ouroboros.Network.Protocol.LocalTxSubmission.Client qualified as Net.Tx
import Prelude qualified
import Servant
import System.Environment (lookupEnv)
import Cardano.Address.Style.Shelley (getKey)
import System.IO (hSetBuffering, BufferMode(LineBuffering))

app :: IsShelleyBasedEra era =>
  CardanoEra era
  -> ShelleyBasedEra era
  -> FaucetState era
  -> Application
app era sbe faucetState = serve userAPI $ server era sbe faucetState

startApiServer :: IsShelleyBasedEra era =>
  CardanoEra era
  -> ShelleyBasedEra era
  -> FaucetState era
  -> Port
  -> IO ()
startApiServer era sbe faucetState port = do
  let
    settings = setTimeout 600 $ setPort port $ defaultSettings
  runSettings settings (app era sbe faucetState)

findAllSizes :: FaucetConfigFile -> [Lovelace]
findAllSizes FaucetConfigFile{fcfRecaptchaLimits,fcfApiKeys} = uniq $ values ++ [v]
  where
    v = akvLovelace fcfRecaptchaLimits
    values :: [Lovelace]
    values = map akvLovelace $ HM.elems fcfApiKeys

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

      queryClient :: Net.Query.LocalStateQueryClient (BlockInMode CardanoMode) ChainPoint (QueryInMode CardanoMode) IO ()
      queryClient = LocalStateQueryClient $ do
        aquireConnection $ do
          runQueryThen (QueryCurrentEra CardanoModeIsMultiEra) $ \(AnyCardanoEra era3) -> do
            tmvar <- newEmptyTMVarIO
            rateLimitTmvar <- newTMVarIO mempty
            let
              faucetState = FaucetState
                { utxoTMVar = tmvar
                , network = net
                , queue = txQueue
                , skey = APaymentExtendedSigningKey pay_skey
                , vkey = APaymentExtendedVerificationKey pay_vkey
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
                    void . forever $ threadDelay 43200 {- day in seconds -}
                    pure $ Net.Query.SendMsgRelease $
                      pure $ Net.Query.SendMsgDone ()
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

