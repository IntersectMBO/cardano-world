{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE EmptyCase                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

module Ouroboros.Consensus.HardFork.Combinator.Degenerate (
    DegenFork(..)
  , DegenForkProtocol
    -- * Type families
  , Header(..)
  , BlockConfig(..)
  , ConsensusConfig(..)
  , LedgerState(..)
  , GenTx(..)
  , TxId(..)
  , CodecConfig(..)
    -- * Newtype wrappers
  , DegenForkConsensusState(..)
  , DegenForkHeaderHash(..)
  , DegenForkApplyTxErr(..)
    -- * Test support
  , projCfg
  ) where

import           Cardano.Prelude (NoUnexpectedThunks (..))
import           Codec.CBOR.Decoding (Decoder)
import           Codec.CBOR.Encoding (Encoding)
import           Codec.Serialise (Serialise (..))
import           Control.Monad.Except
import qualified Data.ByteString.Lazy as Lazy
import           Data.Coerce
import           Data.FingerTree.Strict (Measured (..))
import           Data.Proxy
import           Data.SOP.BasicFunctors
import           Data.Type.Equality
import           Data.Typeable
import           Data.Void

import           Ouroboros.Network.Block

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Config.SupportsNode
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
import           Ouroboros.Consensus.Node.Run
import           Ouroboros.Consensus.Node.Serialisation
import           Ouroboros.Consensus.Protocol.Abstract
import qualified Ouroboros.Consensus.Storage.ChainDB.Init as InitChainDB
import           Ouroboros.Consensus.Storage.ChainDB.Serialisation
import           Ouroboros.Consensus.TypeFamilyWrappers

import           Ouroboros.Consensus.HardFork.Abstract
import           Ouroboros.Consensus.HardFork.Combinator.Abstract
import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras
import           Ouroboros.Consensus.HardFork.Combinator.Basics
import           Ouroboros.Consensus.HardFork.Combinator.Block
import           Ouroboros.Consensus.HardFork.Combinator.Ledger ()
import           Ouroboros.Consensus.HardFork.Combinator.Ledger.Query ()
import           Ouroboros.Consensus.HardFork.Combinator.Mempool
import           Ouroboros.Consensus.HardFork.Combinator.Node ()
import           Ouroboros.Consensus.HardFork.Combinator.Unary

-- | Degenerate hard fork with a single era
--
-- NOTE: It is important to realize that in general
--
-- > HardForkBlock '[b]
--
-- and
--
-- > DegenFork b
--
-- may behave differently. Crucially, they might have
--
-- * different serialization formats, where the former uses a serialization
--   format that is forward-compatible with hard fork transitions, whereas
--   the latter may well not be
-- * related to the previous point, it will have its own network protocol
--   versioning
--
-- The main use of 'DegenFork' is for testing, and as evidence that all
-- type class instances that are required for the hard fork are present.
newtype DegenFork b = DBlk {
      unDBlk :: HardForkBlock '[b]
    }
  deriving (Eq, Show)

{-------------------------------------------------------------------------------
  Data family instances
-------------------------------------------------------------------------------}

instance NoHardForks b => GetHeader (DegenFork b) where
  newtype Header (DegenFork b) = DHdr {
        unDHdr :: Header (HardForkBlock '[b])
      }
    deriving (Show, NoUnexpectedThunks)

  getHeader (DBlk b) = DHdr (getHeader b)

  blockMatchesHeader (DHdr hdr) (DBlk blk) =
      blockMatchesHeader (project hdr) (project' (Proxy @(I b)) blk)

  headerIsEBB (DHdr hdr) = headerIsEBB (project hdr)

newtype instance BlockConfig (DegenFork b) = DBCfg {
      unDBCfg :: BlockConfig (HardForkBlock '[b])
    }
  deriving (NoUnexpectedThunks)

instance SingleEraBlock b => HasCodecConfig (DegenFork b) where
  newtype CodecConfig (DegenFork b) = DCCfg {
        unDCCfg :: CodecConfig (HardForkBlock '[b])
      }
    deriving (NoUnexpectedThunks)

  getCodecConfig = DCCfg . getCodecConfig . unDBCfg

newtype instance ConsensusConfig (DegenForkProtocol b) = DConCfg {
      unDConCfg :: ConsensusConfig (HardForkProtocol '[b])
    }
  deriving (NoUnexpectedThunks)

newtype instance LedgerState (DegenFork b) = DLgr {
      unDLgr :: LedgerState (HardForkBlock '[b])
    }
  deriving (Eq, Show, NoUnexpectedThunks)

instance ConfigSupportsNode b => ConfigSupportsNode (DegenFork b) where
  getSystemStart     = getSystemStart     . unDBCfg
  getNetworkMagic    = getNetworkMagic    . unDBCfg
  getProtocolMagicId = getProtocolMagicId . unDBCfg

{-------------------------------------------------------------------------------
  Forward HasHeader instances
-------------------------------------------------------------------------------}

newtype DegenForkHeaderHash b = DHash {
      unDHash :: HeaderHash (HardForkBlock '[b])
    }
  deriving (Eq, Ord, Show, Typeable, NoUnexpectedThunks, Serialise)

type instance HeaderHash (DegenFork b) = DegenForkHeaderHash b

instance SingleEraBlock b => StandardHash (DegenFork b)

instance SingleEraBlock b => Measured BlockMeasure (DegenFork b) where
  measure = blockMeasure

instance SingleEraBlock b => HasHeader (DegenFork b) where
    blockHash      = DHash    . blockHash     . unDBlk
    blockPrevHash  = castHash . blockPrevHash . unDBlk
    blockSlot      =            blockSlot     . unDBlk
    blockNo        =            blockNo       . unDBlk
    blockInvariant = const True

instance SingleEraBlock b => HasHeader (Header (DegenFork b)) where
  blockHash      = DHash    . blockHash     . unDHdr
  blockPrevHash  = castHash . blockPrevHash . unDHdr
  blockSlot      =            blockSlot     . unDHdr
  blockNo        =            blockNo       . unDHdr
  blockInvariant = const True

{-------------------------------------------------------------------------------
  Forward the 'ConsensusProtocol' instance
-------------------------------------------------------------------------------}

data DegenForkProtocol b

type instance BlockProtocol (DegenFork b) = DegenForkProtocol b

newtype DegenForkConsensusState b = DCSt {
      unDCSt :: ConsensusState (HardForkProtocol '[b])
    }
deriving instance SingleEraBlock b => Eq                 (DegenForkConsensusState b)
deriving instance SingleEraBlock b => Show               (DegenForkConsensusState b)
deriving instance SingleEraBlock b => NoUnexpectedThunks (DegenForkConsensusState b)

instance SingleEraBlock b => ChainSelection (DegenForkProtocol b) where
  type ChainSelConfig (DegenForkProtocol b) = ChainSelConfig (HardForkProtocol '[b])
  type SelectView     (DegenForkProtocol b) = SelectView     (HardForkProtocol '[b])
  preferCandidate   _ = preferCandidate   (Proxy @(HardForkProtocol '[b]))
  compareCandidates _ = compareCandidates (Proxy @(HardForkProtocol '[b]))

instance SingleEraBlock b => ConsensusProtocol (DegenForkProtocol b) where
  -- The reason for introducing a separate 'DegenForkProtocol' instead of:
  --
  -- > type instance BlockProtocol (DegenFork b) = BlockProtocol (HardForkBlock '[b])
  --
  -- is that we need to wrap the 'ConsensusState' in a newtype so that we can
  -- define non-orphan serialisation instances for it. The orphan instances
  -- would be /bad orphans/, i.e., for @HardForkConsensusState '[b]@.
  type ConsensusState (DegenForkProtocol b) = DegenForkConsensusState b
  type ValidationErr  (DegenForkProtocol b) = ValidationErr (HardForkProtocol '[b])
  type LedgerView     (DegenForkProtocol b) = LedgerView    (HardForkProtocol '[b])
  type CanBeLeader    (DegenForkProtocol b) = CanBeLeader   (HardForkProtocol '[b])
  type CannotLead     (DegenForkProtocol b) = CannotLead    (HardForkProtocol '[b])
  type IsLeader       (DegenForkProtocol b) = IsLeader      (HardForkProtocol '[b])
  type ValidateView   (DegenForkProtocol b) = ValidateView  (HardForkProtocol '[b])

  -- Operations on the state
  checkIsLeader (DConCfg cfg) canBeLeader tickedLedgerView (DCSt consensusState) =
    castLeaderCheck <$>
      checkIsLeader cfg canBeLeader tickedLedgerView consensusState
  updateConsensusState (DConCfg cfg) tickedLedgerView valView (DCSt consensusState) =
    DCSt <$> updateConsensusState cfg tickedLedgerView valView consensusState
  rewindConsensusState _ secParam pt (DCSt consensusState) =
    DCSt <$>
      rewindConsensusState
        (Proxy @(HardForkProtocol '[b]))
        secParam
        pt
        consensusState

  -- Straight-forward extensions
  protocolSecurityParam = protocolSecurityParam . unDConCfg

  -- Extract 'ChainSelConfig'
  chainSelConfig = chainSelConfig . unDConCfg

{-------------------------------------------------------------------------------
  Forward 'HardForkBlock' instances
-------------------------------------------------------------------------------}

type instance LedgerCfg (LedgerState (DegenFork b)) = LedgerCfg (LedgerState (HardForkBlock '[b]))

instance SingleEraBlock b => IsLedger (LedgerState (DegenFork b)) where
  type LedgerErr (LedgerState (DegenFork b)) = LedgerErr (LedgerState (HardForkBlock '[b]))

  applyChainTick cfg slot (DLgr lgr) = DLgr <$> applyChainTick cfg slot lgr

instance SingleEraBlock b => ApplyBlock (LedgerState (DegenFork b)) (DegenFork b) where
  applyLedgerBlock cfg (DBlk b) (Ticked slot (DLgr lgr)) =
    DLgr <$> applyLedgerBlock cfg b (Ticked slot lgr)
  reapplyLedgerBlock cfg (DBlk b) (Ticked slot (DLgr lgr)) =
    DLgr $ reapplyLedgerBlock cfg b (Ticked slot lgr)
  ledgerTipPoint (DLgr l) =
    (castPoint :: Point (HardForkBlock '[b]) -> Point (DegenFork b)) $ ledgerTipPoint l

instance SingleEraBlock b => UpdateLedger (DegenFork b)

instance SingleEraBlock b => HasHardForkHistory (DegenFork b) where
  type HardForkIndices (DegenFork b) = '[b]

  hardForkSummary cfg (DLgr lgr) = hardForkSummary cfg lgr

instance SingleEraBlock b => HasAnnTip (DegenFork b) where
  type TipInfo (DegenFork b) = TipInfo (HardForkBlock '[b])

  tipInfoHash _ = DHash . tipInfoHash (Proxy @(HardForkBlock '[b]))
  getTipInfo (DHdr hdr) = getTipInfo hdr

instance SingleEraBlock b => BasicEnvelopeValidation (DegenFork b) where
  expectedFirstBlockNo  _ = expectedFirstBlockNo  (Proxy @(HardForkBlock '[b]))
  minimumPossibleSlotNo _ = minimumPossibleSlotNo (Proxy @(HardForkBlock '[b]))
  expectedNextBlockNo   _ = expectedNextBlockNo   (Proxy @(HardForkBlock '[b]))
  minimumNextSlotNo     _ = minimumNextSlotNo     (Proxy @(HardForkBlock '[b]))

instance NoHardForks b => ValidateEnvelope (DegenFork b) where
  type OtherHeaderEnvelopeError (DegenFork b) = OtherHeaderEnvelopeError (HardForkBlock '[b])

  additionalEnvelopeChecks cfg view (DHdr hdr) =
      withExcept (inject' (Proxy @(WrapEnvelopeErr b))) $
        additionalEnvelopeChecks
          (projCfg cfg)
          (project' (Proxy @(WrapLedgerView b)) <$> view)
          (project hdr)

instance NoHardForks b => BlockSupportsProtocol (DegenFork b) where
  validateView (DBCfg cfg) (DHdr hdr) = validateView cfg hdr
  selectView   (DBCfg cfg) (DHdr hdr) = selectView   cfg hdr

instance NoHardForks b => LedgerSupportsProtocol (DegenFork b) where
  protocolLedgerView   cfg (DLgr lgr) = protocolLedgerView   cfg lgr
  ledgerViewForecastAt cfg (DLgr lgr) = ledgerViewForecastAt cfg lgr

newtype DegenForkApplyTxErr b = DApplyTxErr {
      unDApplyTxErr :: ApplyTxErr (HardForkBlock '[b])
    }
  deriving (Show)

instance NoHardForks b => LedgerSupportsMempool (DegenFork b) where
  newtype GenTx (DegenFork b) = DTx {
        unDTx :: GenTx (HardForkBlock '[b])
      }
    deriving (Show, NoUnexpectedThunks)

  type ApplyTxErr (DegenFork b) = DegenForkApplyTxErr b

  txInvariant = txInvariant . unDTx

  applyTx cfg (DTx tx) (Ticked slot (DLgr lgr)) =
    withExcept DApplyTxErr $
      fmap DLgr <$> applyTx cfg tx (Ticked slot lgr)
  reapplyTx cfg (DTx tx) (Ticked slot (DLgr lgr)) =
    withExcept DApplyTxErr $
      fmap DLgr <$> reapplyTx cfg tx (Ticked slot lgr)

  maxTxCapacity (Ticked slot (DLgr lgr)) =
    maxTxCapacity (Ticked slot (project lgr))

  maxTxSize (DLgr lgr) = maxTxSize (project lgr)

  txInBlockSize (DTx tx) = txInBlockSize (project tx)


instance SingleEraBlock b => HasTxId (GenTx (DegenFork b)) where
  newtype TxId (GenTx (DegenFork b)) = DTxId {
        unDTxId :: TxId (GenTx (HardForkBlock '[b]))
      }
    deriving (Show, Eq, Ord, NoUnexpectedThunks)

  txId (DTx tx) = DTxId (txId tx)

instance SingleEraBlock b => ShowQuery (Query (DegenFork b)) where
  showResult (DQry qry) = showResult qry

instance SingleEraBlock b => QueryLedger (DegenFork b) where
  newtype Query (DegenFork b) result = DQry {
        unDQry :: Query (HardForkBlock '[b]) result
      }
    deriving (Show)

  answerQuery cfg (DQry qry) (DLgr lgr) = answerQuery cfg qry lgr
  eqQuery (DQry qry1) (DQry qry2) = eqQuery qry1 qry2

instance NoHardForks b => CanForge (DegenFork b) where
  type ForgeState (DegenFork b) = ForgeState (HardForkBlock '[b])

  forgeBlock cfg upd block (Ticked slot (DLgr lgr)) txs proof =
      (DBlk . inject' (Proxy @(I b))) <$>
        forgeBlock
          (projCfg cfg)
          (project' (Proxy @(WrapForgeState b)) upd)
          block
          (Ticked slot (project lgr))
          (map (project . unDTx) txs)
          (project' (Proxy @(WrapIsLeader b)) proof)

instance HasTxs b => HasTxs (DegenFork b) where
  extractTxs = map DTx . extractTxs . unDBlk

instance SingleEraBlock b => ConvertRawHash (DegenFork b) where
  toRawHash   _ =         toRawHash   (Proxy @(HardForkBlock '[b])) . unDHash
  fromRawHash _ = DHash . fromRawHash (Proxy @(HardForkBlock '[b]))
  hashSize    _ =          hashSize   (Proxy @(HardForkBlock '[b]))

{-------------------------------------------------------------------------------
  Serialisation instances

  As discussed in the module header, for this we delegate to @b@, rather than
  to @HardForkBlock '[b]@
-------------------------------------------------------------------------------}

-- Disk

instance (SerialiseDiskConstraints b, NoHardForks b) => ImmDbSerialiseConstraints (DegenFork b)
instance (SerialiseDiskConstraints b, NoHardForks b) => LgrDbSerialiseConstraints (DegenFork b)
instance (SerialiseDiskConstraints b, NoHardForks b) => VolDbSerialiseConstraints (DegenFork b)
instance (SerialiseDiskConstraints b, NoHardForks b) => SerialiseDiskConstraints  (DegenFork b)

defaultEncodeDisk
  :: forall x f b.
     ( Coercible x (f (HardForkBlock '[b]))
     , Isomorphic f
     , NoHardForks b
     , EncodeDisk b (f b)
     )
  => Proxy (f b) -> CodecConfig (DegenFork b) -> x -> Encoding
defaultEncodeDisk p (DCCfg ccfg) x =
    encodeDisk (project ccfg) (project' p x :: f b)

defaultDecodeDisk
  :: forall x f b.
     ( Coercible x (f (HardForkBlock '[b]))
     , Isomorphic f
     , NoHardForks b
     , DecodeDisk b (f b)
     )
  => Proxy (f b) -> CodecConfig (DegenFork b) -> forall s. Decoder s x
defaultDecodeDisk _ (DCCfg ccfg) =
    coerce . inject <$> decodeDisk @b @(f b) (project ccfg)

instance (SerialiseDiskConstraints b, NoHardForks b)
      => EncodeDisk (DegenFork b) (DegenFork b) where
  encodeDisk = defaultEncodeDisk (Proxy @(I b))

instance (SerialiseDiskConstraints b, NoHardForks b)
      => DecodeDisk (DegenFork b) (Lazy.ByteString -> DegenFork b) where
  decodeDisk = defaultDecodeDisk (Proxy @(Lazy.ByteString -> b))

instance (SerialiseDiskConstraints b, NoHardForks b)
       => EncodeDisk (DegenFork b) (Header (DegenFork b)) where
  encodeDisk = defaultEncodeDisk (Proxy @(Header b))

instance (SerialiseDiskConstraints b, NoHardForks b)
       => DecodeDisk (DegenFork b) (Lazy.ByteString -> Header (DegenFork b)) where
  decodeDisk = defaultDecodeDisk (Proxy @(((->) Lazy.ByteString :.: Header) b))

instance (SerialiseDiskConstraints b, NoHardForks b)
      => EncodeDisk (DegenFork b) (DegenForkConsensusState b) where
  encodeDisk = defaultEncodeDisk (Proxy @(WrapConsensusState b))

instance (SerialiseDiskConstraints b, NoHardForks b)
      => DecodeDisk (DegenFork b) (DegenForkConsensusState b) where
  decodeDisk = defaultDecodeDisk (Proxy @(WrapConsensusState b))

instance (SerialiseDiskConstraints b, NoHardForks b)
      => EncodeDisk (DegenFork b) (LedgerState (DegenFork b)) where
  encodeDisk = defaultEncodeDisk (Proxy @(LedgerState b))

instance (SerialiseDiskConstraints b, NoHardForks b)
      => DecodeDisk (DegenFork b) (LedgerState (DegenFork b)) where
  decodeDisk = defaultDecodeDisk (Proxy @(LedgerState b))

instance (SerialiseDiskConstraints b, NoHardForks b)
      => EncodeDisk (DegenFork b) (AnnTip (DegenFork b)) where
  encodeDisk cfg =
        defaultEncodeDisk (Proxy @(AnnTip b)) cfg
      . (castAnnTip :: AnnTip (DegenFork b) -> AnnTip (HardForkBlock '[b]))

instance (SerialiseDiskConstraints b, NoHardForks b)
      => DecodeDisk (DegenFork b) (AnnTip (DegenFork b)) where
  decodeDisk =
      fmap (castAnnTip :: AnnTip (HardForkBlock '[b]) -> AnnTip (DegenFork b))
    . defaultDecodeDisk (Proxy @(AnnTip b))

-- NodeToNode

instance (SerialiseNodeToNodeConstraints b, NoHardForks b)
       => SerialiseNodeToNodeConstraints (DegenFork b)

defaultEncodeNodeToNode
  :: forall x f b.
     ( Coercible x (f (HardForkBlock '[b]))
     , Isomorphic f
     , NoHardForks b
     , SerialiseNodeToNode b (f b)
     )
  => Proxy (f b)
  -> CodecConfig (DegenFork b) -> NodeToNodeVersion (DegenFork b)
  -> x -> Encoding
defaultEncodeNodeToNode p (DCCfg ccfg) version x =
    encodeNodeToNode (project ccfg) version (project' p x :: f b)

defaultDecodeNodeToNode
  :: forall x f b.
     ( Coercible x (f (HardForkBlock '[b]))
     , Isomorphic f
     , NoHardForks b
     , SerialiseNodeToNode b (f b)
     )
  => Proxy (f b)
  -> CodecConfig (DegenFork b) -> NodeToNodeVersion (DegenFork b)
  -> forall s. Decoder s x
defaultDecodeNodeToNode _ (DCCfg ccfg) version =
    coerce . inject <$> decodeNodeToNode @b @(f b) (project ccfg) version

instance (SerialiseNodeToNodeConstraints b, NoHardForks b)
       => SerialiseNodeToNode (DegenFork b) (DegenForkHeaderHash b) where
  encodeNodeToNode = defaultEncodeNodeToNode (Proxy @(WrapHeaderHash b))
  decodeNodeToNode = defaultDecodeNodeToNode (Proxy @(WrapHeaderHash b))

instance (SerialiseNodeToNodeConstraints b, NoHardForks b)
       => SerialiseNodeToNode (DegenFork b) (DegenFork b) where
  encodeNodeToNode = defaultEncodeNodeToNode (Proxy @(I b))
  decodeNodeToNode = defaultDecodeNodeToNode (Proxy @(I b))

instance (SerialiseNodeToNodeConstraints b, NoHardForks b)
       => SerialiseNodeToNode (DegenFork b) (Header (DegenFork b)) where
  encodeNodeToNode = defaultEncodeNodeToNode (Proxy @(Header b))
  decodeNodeToNode = defaultDecodeNodeToNode (Proxy @(Header b))

instance (SerialiseNodeToNodeConstraints b, NoHardForks b)
       => SerialiseNodeToNode (DegenFork b) (Serialised (DegenFork b)) where
  encodeNodeToNode (DCCfg ccfg) version (Serialised bytes) =
      encodeNodeToNode
        (project ccfg)
        version
        (Serialised bytes :: Serialised b)
  decodeNodeToNode (DCCfg ccfg) version =
      (\(Serialised bytes) -> Serialised bytes) <$>
        decodeNodeToNode @b @(Serialised b) (project ccfg) version

instance (SerialiseNodeToNodeConstraints b, NoHardForks b)
       => SerialiseNodeToNode (DegenFork b) (Serialised (Header (DegenFork b))) where
  encodeNodeToNode (DCCfg ccfg) version (Serialised bytes) =
      encodeNodeToNode
        (project ccfg)
        version
        (Serialised bytes :: Serialised (Header b))
  decodeNodeToNode (DCCfg ccfg) version =
      (\(Serialised bytes) -> Serialised bytes) <$>
        decodeNodeToNode @b @(Serialised (Header b)) (project ccfg) version

instance (SerialiseNodeToNodeConstraints b, NoHardForks b)
       => SerialiseNodeToNode (DegenFork b) (GenTx (DegenFork b)) where
  encodeNodeToNode = defaultEncodeNodeToNode (Proxy @(GenTx b))
  decodeNodeToNode = defaultDecodeNodeToNode (Proxy @(GenTx b))

instance (SerialiseNodeToNodeConstraints b, NoHardForks b)
       => SerialiseNodeToNode (DegenFork b) (GenTxId (DegenFork b)) where
  encodeNodeToNode = defaultEncodeNodeToNode (Proxy @(WrapGenTxId b))
  decodeNodeToNode = defaultDecodeNodeToNode (Proxy @(WrapGenTxId b))

-- NodeToClient

instance (SerialiseNodeToClientConstraints b, NoHardForks b)
       => SerialiseNodeToClientConstraints (DegenFork b)

defaultEncodeNodeToClient
  :: forall x f b.
     ( Coercible x (f (HardForkBlock '[b]))
     , Isomorphic f
     , NoHardForks b
     , SerialiseNodeToClient b (f b)
     )
  => Proxy (f b)
  -> CodecConfig (DegenFork b) -> NodeToClientVersion (DegenFork b)
  -> x -> Encoding
defaultEncodeNodeToClient p (DCCfg ccfg) version x =
    encodeNodeToClient (project ccfg) version (project' p x :: f b)

defaultDecodeNodeToClient
  :: forall x f b.
     ( Coercible x (f (HardForkBlock '[b]))
     , Isomorphic f
     , NoHardForks b
     , SerialiseNodeToClient b (f b)
     )
  => Proxy (f b)
  -> CodecConfig (DegenFork b) -> NodeToClientVersion (DegenFork b)
  -> forall s. Decoder s x
defaultDecodeNodeToClient _ (DCCfg ccfg) version =
    coerce . inject <$> decodeNodeToClient @b @(f b) (project ccfg) version

instance (SerialiseNodeToClientConstraints b, NoHardForks b)
       => SerialiseNodeToClient (DegenFork b) (DegenForkHeaderHash b) where
  encodeNodeToClient = defaultEncodeNodeToClient (Proxy @(WrapHeaderHash b))
  decodeNodeToClient = defaultDecodeNodeToClient (Proxy @(WrapHeaderHash b))

instance (SerialiseNodeToClientConstraints b, NoHardForks b)
       => SerialiseNodeToClient (DegenFork b) (DegenFork b) where
  encodeNodeToClient = defaultEncodeNodeToClient (Proxy @(I b))
  decodeNodeToClient = defaultDecodeNodeToClient (Proxy @(I b))

instance (SerialiseNodeToClientConstraints b, NoHardForks b)
       => SerialiseNodeToClient (DegenFork b) (Serialised (DegenFork b)) where
  encodeNodeToClient (DCCfg ccfg) version (Serialised bytes) =
      encodeNodeToClient
        (project ccfg)
        version
        (Serialised bytes :: Serialised b)
  decodeNodeToClient (DCCfg ccfg) version =
      (\(Serialised bytes) -> Serialised bytes) <$>
        decodeNodeToClient @b @(Serialised b) (project ccfg) version

instance (SerialiseNodeToClientConstraints b, NoHardForks b)
       => SerialiseNodeToClient (DegenFork b) (GenTx (DegenFork b)) where
  encodeNodeToClient = defaultEncodeNodeToClient (Proxy @(GenTx b))
  decodeNodeToClient = defaultDecodeNodeToClient (Proxy @(GenTx b))

instance (SerialiseNodeToClientConstraints b, NoHardForks b)
       => SerialiseNodeToClient (DegenFork b) (DegenForkApplyTxErr b) where
  encodeNodeToClient = defaultEncodeNodeToClient (Proxy @(WrapApplyTxErr b))
  decodeNodeToClient = defaultDecodeNodeToClient (Proxy @(WrapApplyTxErr b))

instance (SerialiseNodeToClientConstraints b, NoHardForks b)
       => SerialiseNodeToClient (DegenFork b) (Some (Query (DegenFork b))) where
  encodeNodeToClient (DCCfg ccfg) version (Some (DQry qry)) =
      projQuery qry $ \_pf qry' ->
        encodeNodeToClient
          (project ccfg)
          version
          (Some qry')
  decodeNodeToClient (DCCfg ccfg) version =
      (\(Some qry) -> Some (DQry $ injQuery qry)) <$>
        decodeNodeToClient @b @(Some (Query b)) (project ccfg) version

instance (SerialiseNodeToClientConstraints b, NoHardForks b)
       => SerialiseResult (DegenFork b) (Query (DegenFork b)) where
  encodeResult (DCCfg ccfg) version (DQry qry) mResult =
      projQuery qry $ \Refl qry' ->
        case mResult of
          Right result -> encodeResult (project ccfg) version qry' result
          Left  err    -> absurd $ mismatchOneEra err
  decodeResult (DCCfg ccfg) version (DQry qry) =
      projQuery qry $ \Refl qry' ->
        Right <$> decodeResult (project ccfg) version qry'

{-------------------------------------------------------------------------------
  RunNode instance

  As discussed in the module header, for this we delegate to @b@, rather than
  to @HardForkBlock '[b]@
-------------------------------------------------------------------------------}

projCfg :: NoHardForks b => TopLevelConfig (DegenFork b) -> TopLevelConfig b
projCfg = project . castTopLevelConfig

instance HasNetworkProtocolVersion b => HasNetworkProtocolVersion (DegenFork b) where
  type NodeToNodeVersion   (DegenFork b) = NodeToNodeVersion   b
  type NodeToClientVersion (DegenFork b) = NodeToClientVersion b

  -- | Enumerate all supported node-to-node versions
  supportedNodeToNodeVersions   _ = supportedNodeToNodeVersions   (Proxy @b)
  supportedNodeToClientVersions _ = supportedNodeToClientVersions (Proxy @b)
  mostRecentNodeToNodeVersion   _ = mostRecentNodeToNodeVersion   (Proxy @b)
  mostRecentNodeToClientVersion _ = mostRecentNodeToClientVersion (Proxy @b)
  nodeToNodeProtocolVersion     _ = nodeToNodeProtocolVersion     (Proxy @b)
  nodeToClientProtocolVersion   _ = nodeToClientProtocolVersion   (Proxy @b)

instance (NoHardForks b, RunNode b) => RunNode (DegenFork b) where
  nodeBlockFetchSize (DHdr hdr) = nodeBlockFetchSize (project hdr)

  nodeImmDbChunkInfo cfg = nodeImmDbChunkInfo (projCfg cfg)

  nodeGetBinaryBlockInfo (DBlk blk) =
      nodeGetBinaryBlockInfo (project' (Proxy @(I b)) blk :: b)

  nodeAddHeaderEnvelope (DCCfg cfg) =
      nodeAddHeaderEnvelope (project cfg)

  nodeExceptionIsFatal _ = nodeExceptionIsFatal (Proxy @b)

  nodeInitChainDB cfg initDB =
      nodeInitChainDB
        (projCfg cfg)
        (project (InitChainDB.cast initDB))

  nodeCheckIntegrity cfg (DBlk blk) =
      nodeCheckIntegrity (projCfg cfg) (project' (Proxy @(I b)) blk)
