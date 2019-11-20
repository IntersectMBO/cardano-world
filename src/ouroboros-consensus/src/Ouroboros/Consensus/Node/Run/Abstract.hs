{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE RankNTypes              #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- | Infrastructure required to run a node
--
-- The definitions in this module are independent from any specific protocol.
module Ouroboros.Consensus.Node.Run.Abstract
  ( RunNode (..)
  ) where

import           Codec.CBOR.Decoding (Decoder)
import           Codec.CBOR.Encoding (Encoding)
import           Crypto.Random (MonadRandom)
import qualified Data.ByteString.Lazy as Lazy
import           Data.Proxy (Proxy)

import           Cardano.Crypto (ProtocolMagicId)

import           Ouroboros.Network.Block (BlockNo, ChainHash (..), HeaderHash,
                     SlotNo)
import           Ouroboros.Network.BlockFetch (SizeInBytes)
import           Ouroboros.Network.Magic (NetworkMagic)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.BlockchainTime (SystemStart)
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Byron
import           Ouroboros.Consensus.Mempool
import           Ouroboros.Consensus.Protocol.Abstract

import           Ouroboros.Storage.Common (EpochNo, EpochSize)
import           Ouroboros.Storage.ImmutableDB (BinaryInfo (..), HashInfo)

class (ProtocolLedgerView blk, ApplyTx blk) => RunNode blk where

  nodeForgeBlock          :: (HasNodeState (BlockProtocol blk) m, MonadRandom m)
                          => NodeConfig (BlockProtocol blk)
                          -> SlotNo         -- ^ Current slot
                          -> BlockNo        -- ^ Current block number
                          -> ChainHash blk  -- ^ Previous hash
                          -> [GenTx blk]    -- ^ Txs to add in the block
                          -> IsLeader (BlockProtocol blk)
                          -> m blk

  nodeBlockMatchesHeader  :: Header blk -> blk -> Bool
  nodeBlockFetchSize      :: Header blk -> SizeInBytes
  nodeIsEBB               :: blk -> Maybe EpochNo
  nodeEpochSize           :: Monad m
                          => Proxy blk
                          -> NodeConfig (BlockProtocol blk)
                          -> EpochNo -> m EpochSize
  nodeStartTime           :: Proxy blk
                          -> NodeConfig (BlockProtocol blk)
                          -> SystemStart
  nodeNetworkMagic        :: Proxy blk
                          -> NodeConfig (BlockProtocol blk)
                          -> NetworkMagic
  nodeProtocolMagicId     :: Proxy blk
                          -> NodeConfig (BlockProtocol blk)
                          -> ProtocolMagicId
  nodeHashInfo            :: Proxy blk
                          -> HashInfo (HeaderHash blk)

  -- Encoders
  nodeEncodeBlockWithInfo :: NodeConfig (BlockProtocol blk) -> blk -> BinaryInfo Encoding
  nodeEncodeBlock         :: NodeConfig (BlockProtocol blk) -> blk -> Encoding
  nodeEncodeBlock cfg blk =  binaryBlob $ nodeEncodeBlockWithInfo cfg blk
  nodeEncodeHeader        :: NodeConfig (BlockProtocol blk) -> Header blk -> Encoding
  nodeEncodeGenTx         :: GenTx  blk -> Encoding
  nodeEncodeGenTxId       :: GenTxId blk -> Encoding
  nodeEncodeHeaderHash    :: Proxy blk -> HeaderHash blk -> Encoding
  nodeEncodeLedgerState   :: NodeConfig (BlockProtocol blk) -> LedgerState blk -> Encoding
  nodeEncodeChainState    :: Proxy blk -> ChainState (BlockProtocol blk) -> Encoding
  nodeEncodeApplyTxError  :: Proxy blk -> ApplyTxErr blk -> Encoding

  -- Decoders
  nodeDecodeHeader        :: forall s. NodeConfig (BlockProtocol blk) -> Decoder s (Lazy.ByteString -> Header blk)
  nodeDecodeBlock         :: forall s. NodeConfig (BlockProtocol blk) -> Decoder s (Lazy.ByteString -> blk)
  nodeDecodeGenTx         :: forall s. Decoder s (GenTx blk)
  nodeDecodeGenTxId       :: forall s. Decoder s (GenTxId blk)
  nodeDecodeHeaderHash    :: forall s. Proxy blk -> Decoder s (HeaderHash blk)
  nodeDecodeLedgerState   :: forall s. NodeConfig (BlockProtocol blk) -> Decoder s (LedgerState blk)
  nodeDecodeChainState    :: forall s. Proxy blk -> Decoder s (ChainState (BlockProtocol blk))
  nodeDecodeApplyTxError  :: forall s. Proxy blk -> Decoder s (ApplyTxErr blk)
