{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Cardano.CLI.Ops
  ( CardanoEra(..)
  , cardanoEraForProtocol
  , decodeCBOR
  , deserialiseSigningKey
  , getGenesisHashText
  , getLocalTip
  , pPrintCBOR
  , readCBOR
  , readGenesis
  , readProtocolMagicId
  , serialiseDelegationCert
  , serialiseDelegateKey
  , serialiseGenesis
  , serialisePoorKey
  , serialiseSigningKey
  , validateCBOR
  , withRealPBFT
  , CliError(..)
  , RealPBFTError(..)
  ) where

import           Prelude (show, unlines)
import           Cardano.Prelude hiding (catch, option, show)

import           Codec.CBOR.Pretty (prettyHexEnc)
import           Codec.CBOR.Read (DeserialiseFailure, deserialiseFromBytes)
import           Codec.CBOR.Term (decodeTerm, encodeTerm)
import           Codec.CBOR.Write (toLazyByteString)
import           Control.Monad.Trans.Except.Extra
                   (firstExceptT, handleIOExceptT, left, right)
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import qualified Formatting as F
import           System.Directory (canonicalizePath, makeAbsolute)
import qualified Text.JSON.Canonical as CanonicalJSON

import           Cardano.Binary
                   (Decoder, DecoderError, fromCBOR)
import           Cardano.Chain.Block (fromCBORABlockOrBoundary)
import qualified Cardano.Chain.Delegation as Delegation
import qualified Cardano.Chain.Genesis as Genesis
import qualified Cardano.Crypto.Signing as Crypto
import           Cardano.Chain.Slotting (EpochSlots(..))
import qualified Cardano.Chain.Update as Update
import qualified Cardano.Chain.UTxO as UTxO
import           Cardano.Crypto (RequiresNetworkMagic, SigningKey (..))
import qualified Cardano.Crypto.Hashing as Crypto
import           Cardano.Crypto.ProtocolMagic as Crypto
import           Control.Monad.Class.MonadTimer
import           Control.Monad.Class.MonadThrow
import           Control.Tracer (nullTracer, stdoutTracer, traceWith)
import           Network.Mux (MuxError)
import           Ouroboros.Consensus.Block (BlockProtocol)
import           Ouroboros.Consensus.Byron.Ledger (ByronBlock, GenTx)
import qualified Ouroboros.Consensus.Cardano as Consensus
import           Ouroboros.Consensus.Mempool.API (ApplyTxErr)
import           Ouroboros.Consensus.NodeNetwork (ProtocolCodecs(..), protocolCodecs)
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
                   (NodeToClientVersion, mostRecentNetworkProtocolVersion)
import           Ouroboros.Consensus.Config (TopLevelConfig)
import           Ouroboros.Consensus.Node.ProtocolInfo (ProtocolInfo(..))
import           Ouroboros.Consensus.Node.Run
                   (RunNode(..))
import           Ouroboros.Consensus.Util.Condense (Condense(..))
import           Ouroboros.Consensus.Util.IOLike (IOLike)
import           Ouroboros.Network.Block
import           Ouroboros.Network.Codec (Codec)
import           Ouroboros.Network.Mux
                   (AppType(InitiatorApp), OuroborosApplication(..),
                    MuxPeer(..), RunMiniProtocol(..))
import           Ouroboros.Network.NodeToClient
                   (IOManager, NetworkConnectTracers(..), NodeToClientProtocols(..),
                    nodeToClientProtocols, NodeToClientVersionData(..),
                    NodeToClientVersion(NodeToClientV_1),
                    connectTo, localSnocket,
                    localTxSubmissionClientNull, nodeToClientCodecCBORTerm)
import           Ouroboros.Network.Protocol.ChainSync.Client
                   (ChainSyncClient(..), ClientStIdle(..), ClientStNext(..)
                   , chainSyncClientPeer, recvMsgRollForward)
import           Ouroboros.Network.Protocol.ChainSync.Type (ChainSync)
import           Ouroboros.Network.Protocol.Handshake.Version
                   (DictVersion(..), Versions, simpleSingletonVersions)
import           Ouroboros.Network.Protocol.LocalTxSubmission.Type
                   (LocalTxSubmission)
import           Ouroboros.Network.Protocol.LocalTxSubmission.Client
                   (localTxSubmissionClientPeer)

import           Cardano.Common.LocalSocket (chooseSocketPath)
import           Cardano.Config.Protocol
                   (Protocol(..), ProtocolInstantiationError
                   , SomeProtocol(..), fromProtocol, renderProtocolInstantiationError)
import           Cardano.Config.Types
import qualified Cardano.CLI.Legacy.Byron as Legacy

-- | Many commands have variants or file formats that depend on the era.
--
data CardanoEra = ByronEraLegacy | ByronEra | ShelleyEra
  deriving Show

cardanoEraForProtocol :: Protocol -> CardanoEra
cardanoEraForProtocol BFT      = ShelleyEra
cardanoEraForProtocol Praos    = ShelleyEra
cardanoEraForProtocol MockPBFT = ShelleyEra
cardanoEraForProtocol RealPBFT = ByronEra

decodeCBOR
  :: LByteString
  -> (forall s. Decoder s a)
  -> Either CliError (LB.ByteString, a)
decodeCBOR bs decoder =
  first CBORDecodingError $ deserialiseFromBytes decoder bs


deserialiseSigningKey :: CardanoEra -> FilePath -> LB.ByteString
                      -> Either CliError SigningKey
deserialiseSigningKey ByronEraLegacy fp delSkey =
  case deserialiseFromBytes Legacy.decodeLegacyDelegateKey delSkey of
    Left deSerFail -> Left $ SigningKeyDeserialisationFailed fp deSerFail
    Right (_, Legacy.LegacyDelegateKey sKey ) -> pure sKey

deserialiseSigningKey ByronEra fp delSkey =
  case deserialiseFromBytes Crypto.fromCBORXPrv delSkey of
    Left deSerFail -> Left $ SigningKeyDeserialisationFailed fp deSerFail
    Right (_, sKey) -> Right $ SigningKey sKey

deserialiseSigningKey ShelleyEra _ _ = Left $ CardanoEraNotSupported ShelleyEra


getGenesisHashText :: GenesisFile -> ExceptT CliError IO Text
getGenesisHashText (GenesisFile genFile) = do
  canonGenFile <- liftIO $ canonicalizePath genFile
  gFile <- liftIO $ makeAbsolute canonGenFile
  (_, Genesis.GenesisHash gHash) <- readGenesis $ GenesisFile gFile
  return $ F.sformat Crypto.hashHexF gHash

readProtocolMagicId :: GenesisFile -> ExceptT CliError IO Crypto.ProtocolMagicId
readProtocolMagicId gFile = do
  (genData, _) <- readGenesis gFile
  pure $ Genesis.gdProtocolMagicId genData

-- | Read genesis from a file.
readGenesis :: GenesisFile -> ExceptT CliError IO (Genesis.GenesisData, Genesis.GenesisHash)
readGenesis (GenesisFile fp) = firstExceptT (GenesisReadError fp) $ Genesis.readGenesisData fp

validateCBOR :: CBORObject -> LByteString -> Either CliError Text
validateCBOR cborObject bs =
  case cborObject of
    CBORBlockByron -> do
      (const () ) <$> decodeCBOR bs (fromCBORABlockOrBoundary $ EpochSlots 21600)
      Right "Valid Byron block."

    CBORDelegationCertificateByron -> do
      (const () ) <$> decodeCBOR bs (fromCBOR :: Decoder s Delegation.Certificate)
      Right "Valid Byron delegation certificate."

    CBORTxByron -> do
      (const () ) <$> decodeCBOR bs (fromCBOR :: Decoder s UTxO.Tx)
      Right "Valid Byron Tx."

    CBORUpdateProposalByron -> do
      (const () ) <$> decodeCBOR bs (fromCBOR :: Decoder s Update.Proposal)
      Right "Valid Byron update proposal."

pPrintCBOR :: LByteString -> ExceptT CliError IO ()
pPrintCBOR bs = do
  case deserialiseFromBytes decodeTerm bs of
    Left err -> left $ CBORPrettyPrintError err
    Right (_, decodedVal) -> do liftIO . putTextLn . toS . prettyHexEnc $ encodeTerm decodedVal
                                right ()

readCBOR :: FilePath -> ExceptT CliError IO LByteString
readCBOR fp =
  handleIOExceptT
    (ReadCBORFileFailure fp . toS . displayException)
    (LB.readFile fp)


serialiseDelegationCert :: CanonicalJSON.ToJSON Identity a
                        => CardanoEra -> a -> Either CliError LB.ByteString
serialiseDelegationCert ByronEraLegacy dlgCert = pure $ canonicalEncodePretty dlgCert
serialiseDelegationCert ByronEra       dlgCert = pure $ canonicalEncodePretty dlgCert
serialiseDelegationCert ShelleyEra     _       = Left $ CardanoEraNotSupported ShelleyEra


serialiseDelegateKey :: CardanoEra -> SigningKey -> Either CliError LB.ByteString
serialiseDelegateKey ByronEraLegacy sk = pure
                                       . toLazyByteString
                                       . Legacy.encodeLegacyDelegateKey
                                       $ Legacy.LegacyDelegateKey sk
serialiseDelegateKey ByronEra  sk = serialiseSigningKey ByronEra sk
serialiseDelegateKey ShelleyEra _ = Left $ CardanoEraNotSupported ShelleyEra


serialiseGenesis :: CardanoEra -> Genesis.GenesisData
                 -> Either CliError LB.ByteString
serialiseGenesis ByronEraLegacy gData = pure $ canonicalEncodePretty gData
serialiseGenesis ByronEra       gData = pure $ canonicalEncodePretty gData
serialiseGenesis ShelleyEra     _     = Left $ CardanoEraNotSupported ShelleyEra


serialisePoorKey :: CardanoEra -> Genesis.PoorSecret
                 -> Either CliError LB.ByteString
serialisePoorKey ByronEraLegacy ps = serialiseSigningKey ByronEraLegacy $
                                       Genesis.poorSecretToKey ps
serialisePoorKey ByronEra       ps = serialiseSigningKey ByronEra $
                                       Genesis.poorSecretToKey ps
serialisePoorKey ShelleyEra     _  = Left $ CardanoEraNotSupported ShelleyEra


serialiseSigningKey :: CardanoEra -> SigningKey
                    -> Either CliError LB.ByteString
serialiseSigningKey ByronEraLegacy (SigningKey k) = pure $ toLazyByteString (Crypto.toCBORXPrv k)
serialiseSigningKey ByronEra       (SigningKey k) = pure $ toLazyByteString (Crypto.toCBORXPrv k)
serialiseSigningKey ShelleyEra     _              = Left $ CardanoEraNotSupported ShelleyEra

-- | Exception type for all errors thrown by the CLI.
--   Well, almost all, since we don't rethrow the errors from readFile & such.
data CliError
  = CBORDecodingError !DeserialiseFailure
  | CBORPrettyPrintError !DeserialiseFailure
  | CertificateValidationErrors !FilePath ![Text]
  | DelegationError !Genesis.GenesisDelegationError
  | DlgCertificateDeserialisationFailed !FilePath !Text
  | GenerateTxsError !RealPBFTError
  | GenesisGenerationError !Genesis.GenesisDataGenerationError
  | GenesisReadError !FilePath !Genesis.GenesisDataError
  | GenesisSpecError !Text
  | IssueUtxoError !RealPBFTError
  | NoBlocksFound !FilePath
  | NodeSubmitTxError !RealPBFTError
  | NotEnoughTxInputs
  | NotEnoughTxOutputs
  | NoGenesisDelegationForKey !Text
  | OutputMustNotAlreadyExist !FilePath
  | ProtocolError !ProtocolInstantiationError
  | CardanoEraNotSupported !CardanoEra
  | ProtocolParametersParseFailed !FilePath !Text
  | ReadCBORFileFailure !FilePath !Text
  | ReadSigningKeyFailure !FilePath !Text
  | ReadVerificationKeyFailure !FilePath !Text
  | TxDeserialisationFailed !FilePath !DecoderError
  -- TODO:  sadly, VerificationKeyParseError isn't exported from Cardano.Crypto.Signing/*
  | SigningKeyDeserialisationFailed !FilePath !DeserialiseFailure
  | SpendGenesisUTxOError !RealPBFTError
  | UpdateProposalDecodingError !DecoderError
  | UpdateProposalSubmissionError !RealPBFTError
  | VerificationKeyDeserialisationFailed !FilePath !Text
  | FileNotFoundError !FilePath


instance Show CliError where
  show (CBORDecodingError e)
    = "Error with CBOR decoding: " <> show e
  show (CBORPrettyPrintError e)
    = "Error with CBOR decoding: " <> show e
  show (CertificateValidationErrors fp errs)
    = unlines $
      "Errors while validating certificate '" <> fp <> "':":
      (("  " <>) . T.unpack <$> errs)
  show (GenesisReadError fp err)
    = "Genesis file '" <> fp <> "' read failure: "<> show err
  show (DelegationError err)
    = "Error while issuing delegation: " <> show err
  show (DlgCertificateDeserialisationFailed fp err)
    = "Delegation certificate '" <> fp <> "' read failure: "<> T.unpack err
  show (GenerateTxsError err)
    = "Error in GenerateTxs command: " <> show err
  show (GenesisGenerationError err)
    = "Genesis generation failed in mkGenesis: " <> show err
  show (GenesisSpecError err)
    = "Error in genesis specification: " <> T.unpack err
  show (IssueUtxoError err)
    = "Error SpendUTxO command: " <> show err
  show (NoBlocksFound fp)
    = "Error while creating update proposal, no blocks found in: " <> fp
  show (NodeSubmitTxError err)
    = "Error in SubmitTx command: " <> show err
  show (NoGenesisDelegationForKey key)
    = "Newly-generated genesis doesn't delegate to operational key: " <> T.unpack key
  show NotEnoughTxInputs
    = "Transactions must have at least one input."
  show NotEnoughTxOutputs
    = "Transactions must have at least one output."
  show (OutputMustNotAlreadyExist fp)
    = "Output file/directory must not already exist: " <> fp
  show (ProtocolError err)
    = "Protocol Instantiation Error " <> (T.unpack $ renderProtocolInstantiationError err)
  show (CardanoEraNotSupported era)
    = "Unsupported Cardano era " <> show era
  show (ProtocolParametersParseFailed fp err)
    = "Protocol parameters file '" <> fp <> "' read failure: "<> T.unpack err
  show (ReadSigningKeyFailure fp expt)
    = "Exception encountered while trying to read the signing key file at: " <> fp
       <> "Exception: " <> T.unpack expt
  show (ReadCBORFileFailure fp expt)
    = "Exception encountered while trying to read the CBOR file at: " <> fp
       <> "Exception: " <> T.unpack expt
  show (ReadVerificationKeyFailure fp expt)
    = "Exception encountered while trying to read the verification key file at: " <> fp
       <> "Exception: " <> T.unpack expt
  show (SigningKeyDeserialisationFailed fp err)
    = "Signing key '" <> fp <> "' read failure: "<> show err
  show (SpendGenesisUTxOError err)
    = "Error in SpendGenesisUTxO command: " <> show err
  show (TxDeserialisationFailed fp err)
    = "Transaction file '" <> fp <> "' read failure: "<> show err
  show (UpdateProposalDecodingError err)
    = "Error decoding update proposal: " <> show err
  show (UpdateProposalSubmissionError pbftErr)
    = "Error submitting update proposal: " <> show pbftErr
  show (VerificationKeyDeserialisationFailed fp err)
    = "Verification key '" <> fp <> "' read failure: "<> T.unpack err
  show (FileNotFoundError fp)
    = "File '" <> fp <> "' not found!"

data RealPBFTError
  = IncorrectProtocolSpecified !Protocol
  | FromProtocolError !ProtocolInstantiationError
  | InvariantViolation !Text
  | TransactionTypeNotHandledYet !Text
  deriving Show

-- | Perform an action that expects ProtocolInfo for Byron/PBFT,
--   with attendant configuration.
withRealPBFT
  :: GenesisFile
  -> RequiresNetworkMagic
  -> Maybe Double
  -> Maybe DelegationCertFile
  -> Maybe SigningKeyFile
  -> Update
  -> Protocol
  -> (RunNode ByronBlock
        => Consensus.Protocol ByronBlock Consensus.ProtocolRealPBFT
        -> ExceptT RealPBFTError IO a)
  -> ExceptT RealPBFTError IO a
withRealPBFT genFile nMagic sigThresh delCertFp sKeyFp update ptcl action = do
  SomeProtocol p <- firstExceptT
                      FromProtocolError
                      $ fromProtocol
                          ptcl
                          Nothing
                          Nothing
                          (Just genFile)
                          nMagic
                          sigThresh
                          delCertFp
                          sKeyFp
                          update
  case p of
    proto@Consensus.ProtocolRealPBFT{} -> action proto
    _ -> left $ IncorrectProtocolSpecified ptcl

--------------------------------------------------------------------------------
-- Query local node's chain tip
--------------------------------------------------------------------------------

getLocalTip
  :: ConfigYamlFilePath
  -> Maybe CLISocketPath
  -> IOManager
  -> IO ()
getLocalTip configFp mSockPath iocp = do
  nc <- parseNodeConfigurationFP configFp
  sockPath <- return $ chooseSocketPath (ncSocketPath nc) mSockPath

  frmPtclRes <- runExceptT . firstExceptT ProtocolError
                           $ fromProtocol
                               (ncProtocol nc)
                               (ncNodeId nc)
                               (ncNumCoreNodes nc)
                               (Just $ ncGenesisFile nc)
                               (ncReqNetworkMagic nc)
                               (ncPbftSignatureThresh nc)
                               Nothing
                               Nothing
                               (ncUpdate nc)

  SomeProtocol p <- case frmPtclRes of
                        Right (SomeProtocol p) -> pure (SomeProtocol p)
                        Left err -> do putTextLn . toS $ show err
                                       exitFailure

  createNodeConnection (Proxy) p iocp sockPath


createNodeConnection
  :: forall blk . (Condense (HeaderHash blk), RunNode blk)
  => Proxy blk
  -> Consensus.Protocol blk (BlockProtocol blk)
  -> IOManager
  -> SocketPath
  -> IO ()
createNodeConnection proxy ptcl iocp (SocketFile path) =
    let ProtocolInfo{pInfoConfig} = Consensus.protocolInfo ptcl in
    connectTo
      (localSnocket iocp path)
      (NetworkConnectTracers nullTracer nullTracer)
      (localInitiatorNetworkApplication proxy pInfoConfig)
      path
    `catch` handleMuxError

handleMuxError :: MuxError -> IO ()
handleMuxError err = print err

localInitiatorNetworkApplication
  :: forall blk m peer.
     ( RunNode blk
     , Condense (HeaderHash blk)
     , IOLike m
     , MonadIO m
     , MonadTimer m
     )
  => Proxy blk
  -> TopLevelConfig blk
  -> Versions NodeToClientVersion DictVersion
              (peer -> OuroborosApplication InitiatorApp LB.ByteString m () Void)
localInitiatorNetworkApplication proxy cfg =
    simpleSingletonVersions
      NodeToClientV_1
      (NodeToClientVersionData { networkMagic = nodeNetworkMagic proxy cfg })
      (DictVersion nodeToClientCodecCBORTerm) $ \_peerid ->

      nodeToClientProtocols
        NodeToClientProtocols {
          localChainSyncProtocol =
            InitiatorProtocolOnly $
              MuxPeer
                nullTracer
                localChainSyncCodec
                (chainSyncClientPeer chainSyncClient)

        , localTxSubmissionProtocol =
            InitiatorProtocolOnly $
              MuxPeer
                nullTracer
                localTxSubmissionCodec
                (localTxSubmissionClientPeer localTxSubmissionClientNull)
        }
 where
  localChainSyncCodec :: Codec (ChainSync (Serialised blk) (Tip blk)) DeserialiseFailure m LB.ByteString
  localChainSyncCodec = pcLocalChainSyncCodec . protocolCodecs cfg $ mostRecentNetworkProtocolVersion proxy

  localTxSubmissionCodec :: Codec (LocalTxSubmission (GenTx blk) (ApplyTxErr blk)) DeserialiseFailure m LB.ByteString
  localTxSubmissionCodec = pcLocalTxSubmissionCodec . protocolCodecs cfg $ mostRecentNetworkProtocolVersion proxy

chainSyncClient
  :: forall blk m . (Condense (HeaderHash blk), MonadIO m)
  => ChainSyncClient (Serialised blk) (Tip blk) m ()
chainSyncClient = ChainSyncClient $ pure $
  SendMsgRequestNext
    clientStNext
    (pure $ ClientStNext
              { recvMsgRollForward = \_ _ -> ChainSyncClient $ pure clientStIdle
              , recvMsgRollBackward = \_ _ -> ChainSyncClient $ pure clientStIdle
              }
    )
 where
  clientStIdle :: ClientStIdle (Serialised blk) (Tip blk) m ()
  clientStIdle =
    SendMsgRequestNext clientStNext (pure clientStNext)
  --TODO: we should be able to simply return the tip as the result with
  -- SendMsgDone and collect this as the result of the overall protocol.
  -- While currently we can have protocols return things, the current OuroborosApplication
  -- stuff gets in the way of returning an overall result, but that's being worked on,
  -- and this can be improved when that's ready.
  clientStNext :: ClientStNext (Serialised blk) (Tip blk) m ()
  clientStNext = ClientStNext
    { recvMsgRollForward = \_blk tip -> ChainSyncClient $ do
        traceWith stdoutTracer . toS $ getTipOutput tip
        pure $ SendMsgDone ()
    , recvMsgRollBackward = \_point tip -> ChainSyncClient $ do
        traceWith stdoutTracer . toS $ getTipOutput tip
        pure $ SendMsgDone ()
    }

  getTipOutput :: Tip blk -> Text
  getTipOutput (TipGenesis) = "Current tip: genesis (origin)"
  getTipOutput (Tip slotNo headerHash blkNo) =
     T.pack $ unlines [ "\n"
                      , "Current tip: "
                      , "Block hash: " <> condense headerHash
                      , "Slot: " <> condense slotNo
                      , "Block number: " <> condense blkNo
                      ]
