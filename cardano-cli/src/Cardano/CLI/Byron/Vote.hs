{-# LANGUAGE GADTs #-}

module Cardano.CLI.Byron.Vote
  ( runVoteCreation
  , submitByronVote
  ) where

import           Cardano.Prelude

import           Control.Monad.Trans.Except.Extra (firstExceptT, hoistEither)
import           Control.Tracer (nullTracer, stdoutTracer, traceWith)
import qualified Data.ByteString.Lazy as LB


import qualified Cardano.Binary as Binary
import           Cardano.Config.Types
import           Cardano.Chain.Genesis (GenesisData(..))
import           Cardano.Chain.Update
                   (AVote(..), UpId, Vote, mkVote, recoverUpId, recoverVoteId)
import           Cardano.CLI.Byron.UpdateProposal
                   (deserialiseByronUpdateProposal, readByronUpdateProposal)
import           Cardano.CLI.Key (readSigningKey)
import           Cardano.CLI.Ops (CardanoEra(..), ensureNewFileLBS)
import           Cardano.Crypto.Signing (SigningKey)
import           Ouroboros.Consensus.Byron.Ledger.Block (ByronBlock)
import qualified Ouroboros.Consensus.Byron.Ledger.Mempool as Mempool
import qualified Ouroboros.Consensus.Cardano as Consensus
import qualified Ouroboros.Consensus.Mempool as Mempool
import           Ouroboros.Consensus.Node.ProtocolInfo (pInfoConfig)
import           Ouroboros.Consensus.Util.Condense (condense)
import           Ouroboros.Network.IOManager (IOManager)
import           Cardano.Node.Submission (submitGeneralTx)

import           Cardano.CLI.Ops (CliError(..), readGenesis, withRealPBFT)
import           Cardano.Common.LocalSocket (chooseSocketPath)

runVoteCreation
  :: ConfigYamlFilePath
  -> SigningKeyFile
  -> FilePath
  -> Bool
  -> FilePath
  -> ExceptT CliError IO ()
runVoteCreation configFp sKey upPropFp voteBool outputFp = do
  sK <- readSigningKey ByronEra sKey
  upProp <- readByronUpdateProposal upPropFp
  proposal <- hoistEither $ deserialiseByronUpdateProposal upProp
  let updatePropId = recoverUpId proposal
  vote <- createByronVote configFp sK updatePropId voteBool
  ensureNewFileLBS outputFp (serialiseByronVote vote)

convertVoteToGenTx :: AVote ByteString -> Mempool.GenTx ByronBlock
convertVoteToGenTx vote = Mempool.ByronUpdateVote (recoverVoteId vote) vote

createByronVote
  :: ConfigYamlFilePath
  -> SigningKey
  -> UpId
  -> Bool
  -> ExceptT CliError IO Vote
createByronVote config sKey upId voteChoice = do
  nc <- liftIO $ parseNodeConfigurationFP config
  (genData, _) <- readGenesis $ ncGenesisFile nc
  let pmId = gdProtocolMagicId genData
  pure $ mkVote pmId sKey upId voteChoice

deserialiseByronVote :: LByteString -> Either CliError (AVote ByteString)
deserialiseByronVote bs =
  case Binary.decodeFull bs of
    Left deserFail -> Left $ ByronVoteDecodingError deserFail
    Right vote -> Right $ annotateVote vote
 where
  annotateVote :: AVote Binary.ByteSpan -> AVote ByteString
  annotateVote vote = Binary.annotationBytes bs vote


serialiseByronVote :: Vote -> LByteString
serialiseByronVote = Binary.serialize

submitByronVote
  :: IOManager
  -> ConfigYamlFilePath
  -> FilePath
  -> Maybe CLISocketPath
  -> ExceptT CliError IO ()
submitByronVote iocp config voteFp mSocket = do
  nc <- liftIO $ parseNodeConfigurationFP config

  voteBs <- liftIO $ LB.readFile voteFp
  vote <- hoistEither $ deserialiseByronVote voteBs
  let genTx = convertVoteToGenTx vote
      skt = chooseSocketPath (ncSocketPath nc) mSocket

  firstExceptT ByronVoteSubmissionError $ withRealPBFT nc $
              \p@Consensus.ProtocolRealPBFT{} -> liftIO $ do
                 traceWith stdoutTracer ("Vote TxId: " ++ condense (Mempool.txId genTx))
                 submitGeneralTx iocp skt
                                 (pInfoConfig (Consensus.protocolInfo p))
                                 genTx
                                 nullTracer -- stdoutTracer
