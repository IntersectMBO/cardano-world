{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE RecordWildCards  #-}

module Main (main) where

import           Control.Monad.Except
import           Data.Coerce
import           Data.Foldable (asum)
import           Data.IORef
import           Data.List (foldl', intercalate)
import           Data.Word
import           Options.Applicative

import           Cardano.Binary (unAnnotated)
import qualified Cardano.Chain.Block as Chain
import qualified Cardano.Chain.Genesis as Genesis
import           Cardano.Chain.Slotting (EpochSlots (..))
import qualified Cardano.Chain.UTxO as Chain
import qualified Cardano.Crypto as Crypto

import           Ouroboros.Network.Block (HasHeader (..), SlotNo (..),
                     genesisPoint)

import           Ouroboros.Consensus.Ledger.Byron (ByronBlock, ByronHash)
import qualified Ouroboros.Consensus.Ledger.Byron as Byron
import           Ouroboros.Consensus.Node.Run.Abstract
import           Ouroboros.Consensus.Node.Run.Byron ()
import           Ouroboros.Consensus.Util.ResourceRegistry

import           Ouroboros.Storage.ChainDB.API (StreamFrom (..))
import           Ouroboros.Storage.ChainDB.Impl.ImmDB
import           Ouroboros.Storage.Common
import           Ouroboros.Storage.EpochInfo
import qualified Ouroboros.Storage.ImmutableDB.API as ImmDB

main :: IO ()
main = do
    CmdLine{..}   <- getCmdLine
    genesisConfig <- openGenesis clConfig clIsMainNet
    let epochSlots = Genesis.configEpochSlots genesisConfig
        epochInfo  = fixedSizeEpochInfo (coerce epochSlots)
    immDB <- openImmDB clImmDB epochSlots epochInfo
    withRegistry $ runAnalysis clAnalysis immDB epochInfo
    putStrLn "Done"

{-------------------------------------------------------------------------------
  Run the requested analysis
-------------------------------------------------------------------------------}

data AnalysisName =
    ShowSlotBlockNo
  | CountTxOutputs

type Analysis = ImmDB IO ByronBlock
             -> EpochInfo IO
             -> ResourceRegistry IO
             -> IO ()

runAnalysis :: AnalysisName -> Analysis
runAnalysis ShowSlotBlockNo = showSlotBlockNo
runAnalysis CountTxOutputs  = countTxOutputs

{-------------------------------------------------------------------------------
  Analysis: show block and slot number for all blocks
-------------------------------------------------------------------------------}

showSlotBlockNo :: Analysis
showSlotBlockNo immDB _epochInfo rr =
    processAll immDB rr go
  where
    go :: Either EpochNo SlotNo -> ByronBlock -> IO ()
    go isEBB blk = putStrLn $ intercalate "\t" [
                       show isEBB
                     , show (blockNo   blk)
                     , show (blockSlot blk)
                     ]

{-------------------------------------------------------------------------------
  Analysis: show total number of tx outputs per block
-------------------------------------------------------------------------------}

countTxOutputs :: Analysis
countTxOutputs immDB epochInfo rr = do
    cumulative <- newIORef 0
    processAll immDB rr (go cumulative)
  where
    go :: IORef Int -> Either EpochNo SlotNo -> ByronBlock -> IO ()
    go cumulative isEBB blk =
        case (isEBB, blk) of
          (Right slotNo, Byron.ByronBlock (Chain.ABOBBlock regularBlk) _ _) ->
            go' cumulative slotNo regularBlk
          _otherwise ->
            return () -- Skip EBBs

    go' :: IORef Int -> SlotNo -> Chain.ABlock a -> IO ()
    go' cumulative slotNo Chain.ABlock{..} = do
        countCum  <- atomicModifyIORef cumulative $ \c ->
                       let c' = c + count in (c', c')
        epochSlot <- relativeSlotNo epochInfo slotNo
        putStrLn $ intercalate "\t" [
            show slotNo
          , show epochSlot
          , show count
          , show countCum
          ]
      where
        Chain.AHeader{..} = blockHeader
        Chain.ABody{..}   = blockBody

        count :: Int
        count = countTxPayload bodyTxPayload

    countTxPayload :: Chain.ATxPayload a -> Int
    countTxPayload = sum'
                   . map (countTx . unAnnotated . Chain.aTaTx)
                   . Chain.aUnTxPayload

    countTx :: Chain.Tx -> Int
    countTx = length . Chain.txOutputs

    sum' :: [Int] -> Int
    sum' = foldl' (+) 0

-- | Convert 'SlotNo' to relative 'EpochSlot'
--
-- NOTE: Unlike 'epochInfoBlockRelative', which puts the EBB at relative slot 0,
-- this puts the first real block at relative slot 0.
relativeSlotNo :: Monad m => EpochInfo m -> SlotNo -> m (EpochNo, Word64)
relativeSlotNo epochInfo (SlotNo absSlot) = do
    epoch        <- epochInfoEpoch epochInfo (SlotNo absSlot)
    SlotNo first <- epochInfoFirst epochInfo epoch
    return (epoch, absSlot - first)

{-------------------------------------------------------------------------------
  Auxiliary: processing all blocks in the imm DB
-------------------------------------------------------------------------------}

processAll :: ImmDB IO ByronBlock
           -> ResourceRegistry IO
           -> (Either EpochNo SlotNo -> ByronBlock -> IO ())
           -> IO ()
processAll immDB rr callback = do
    Right itr <- streamBlocksFrom immDB rr $ StreamFromExclusive genesisPoint
    go itr
  where
    go :: Iterator ByronHash IO ByronBlock -> IO ()
    go itr = do
        itrResult <- ImmDB.iteratorNext itr
        case itrResult of
          IteratorExhausted                -> return ()
          IteratorResult slotNo  _hash blk -> callback (Right slotNo) blk >> go itr
          IteratorEBB    epochNo _hash blk -> callback (Left epochNo) blk >> go itr

{-------------------------------------------------------------------------------
  Command line args
-------------------------------------------------------------------------------}

data CmdLine = CmdLine {
      clConfig    :: FilePath
    , clIsMainNet :: Bool
    , clImmDB     :: FilePath
    , clAnalysis  :: AnalysisName
    }

parseCmdLine :: Parser CmdLine
parseCmdLine = CmdLine
    <$> (strOption $ mconcat [
            long "config"
          , help "Path to config file"
          , metavar "PATH"
          ])
    <*> (flag True False $ mconcat [
            long "testnet"
          , help "The DB contains blocks from testnet rather than mainnet"
          ])
    <*> (strOption $ mconcat [
            long "db"
          , help "Path to the chain DB (parent of \"immutable\" directory)"
          , metavar "PATH"
          ])
    <*> parseAnalysis

parseAnalysis :: Parser AnalysisName
parseAnalysis = asum [
      flag' ShowSlotBlockNo $ mconcat [
          long "show-slot-block-no"
        , help "Show slot and block number of all blocks"
        ]
    , flag' CountTxOutputs $ mconcat [
          long "count-tx-outputs"
        , help "Show number of transaction outputs per block"
        ]
    ]

getCmdLine :: IO CmdLine
getCmdLine = execParser opts
  where
    opts = info (parseCmdLine <**> helper) (mconcat [
          fullDesc
        , progDesc "Simple framework for running analysis over the immutable DB"
        ])

{-------------------------------------------------------------------------------
  Genesis config
-------------------------------------------------------------------------------}

openGenesis :: FilePath -> Bool -> IO Genesis.Config
openGenesis configFile onMainNet = do
    Right genesisHash <- runExceptT $
      snd <$> Genesis.readGenesisData configFile
    Right genesisConfig <- runExceptT $
      Genesis.mkConfigFromFile
        (if onMainNet -- transactions on testnet include magic number
          then Crypto.RequiresNoMagic
          else Crypto.RequiresMagic)
        configFile
        (Genesis.unGenesisHash genesisHash)
    return genesisConfig

{-------------------------------------------------------------------------------
  Interface with the ImmDB
-------------------------------------------------------------------------------}

openImmDB :: FilePath -> EpochSlots -> EpochInfo IO -> IO (ImmDB IO ByronBlock)
openImmDB fp epochSlots epochInfo = openDB args
  where
    args :: ImmDbArgs IO ByronBlock
    args = (defaultArgs fp) {
          immDecodeHash  = Byron.decodeByronHeaderHash
        , immDecodeBlock = Byron.decodeByronBlock epochSlots
        , immEncodeHash  = Byron.encodeByronHeaderHash
        , immEncodeBlock = Byron.encodeByronBlockWithInfo
        , immEpochInfo   = epochInfo
        , immValidation  = ValidateMostRecentEpoch
        , immIsEBB       = nodeIsEBB
        }
