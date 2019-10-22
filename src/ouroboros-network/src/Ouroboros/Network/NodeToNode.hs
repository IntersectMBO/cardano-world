{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeFamilies      #-}

-- | This is the starting point for a module that will bring together the
-- overall node to node protocol, as a collection of mini-protocols.
--
module Ouroboros.Network.NodeToNode (
    NodeToNodeProtocols(..)
  , NodeToNodeVersion (..)
  , NodeToNodeVersionData (..)
  , DictVersion (..)
  , nodeToNodeCodecCBORTerm

  , connectTo
  , connectTo_V1

  , withServer
  , withServer_V1

  -- * Subscription Workers
  -- ** IP subscriptin worker
  , IPSubscriptionTarget (..)
  , ipSubscriptionWorker
  , ipSubscriptionWorker_V1
  , SubscriptionTrace (..)
  , WithIPList (..)
  -- ** DNS subscription worker
  , DnsSubscriptionTarget (..)
  , dnsSubscriptionWorker
  , dnsSubscriptionWorker_V1
  , DnsTrace (..)
  , WithDomainName (..)

  -- * Re-exports
  , ConnectionTable
  , newConnectionTable
  , TraceSendRecv (..)
  , DecoderFailureOrTooMuchInput
  , Handshake
  , LocalAddresses (..)
  ) where

import           Control.Concurrent.Async (Async)
import qualified Data.ByteString.Lazy as BL
import           Data.Time.Clock (DiffTime)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Typeable (Typeable)
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Term as CBOR
import           Codec.Serialise (Serialise (..), DeserialiseFailure)
import           Codec.SerialiseTerm

import qualified Network.Socket as Socket

import           Control.Monad.Class.MonadSTM

import           Network.Mux.Types (ProtocolEnum(..), MiniProtocolLimits (..), WithMuxBearer, MuxTrace)
import           Network.Mux.Interface

import           Ouroboros.Network.Magic
import           Ouroboros.Network.Mux
import           Ouroboros.Network.Protocol.Handshake.Type
import           Ouroboros.Network.Protocol.Handshake.Version
import           Ouroboros.Network.Socket
import           Ouroboros.Network.Server.ConnectionTable ( ConnectionTable
                                                          , newConnectionTable
                                                          )
import qualified Ouroboros.Network.Subscription.Ip as Subscription
import           Ouroboros.Network.Subscription.Ip ( IPSubscriptionTarget (..)
                                                   , WithIPList (..)
                                                   , SubscriptionTrace (..)
                                                   )
import qualified Ouroboros.Network.Subscription.Dns as Subscription
import           Ouroboros.Network.Subscription.Dns ( DnsSubscriptionTarget (..)
                                                    , DnsTrace (..)
                                                    , WithDomainName (..)
                                                    )
import           Ouroboros.Network.Subscription.Worker (LocalAddresses (..))
import           Network.TypedProtocol.Driver.ByteLimit (DecoderFailureOrTooMuchInput)
import           Network.TypedProtocol.Driver (TraceSendRecv (..))
import           Control.Tracer (Tracer)


-- | An index type used with the mux to enumerate all the mini-protocols that
-- make up the overall node-to-node protocol.
--
data NodeToNodeProtocols = ChainSyncWithHeadersPtcl
                         | BlockFetchPtcl
                         | TxSubmissionPtcl
  deriving (Eq, Ord, Enum, Bounded, Show)

-- These protocol numbers end up in the wire format so it is vital that they
-- are stable, even as they are upgraded. So we use custom Enum instances here.
-- This allows us to retire old versions and add new, which may leave some
-- holes in the numbering space.

-- | These are the actual wire format protocol numbers.
--
-- The application specific protocol numbers start from 2 because of the two
-- mux built-in protocols.
--
-- These are chosen to not overlap with the node to client protocol numbers.
-- This is not essential for correctness, but is helpful to allow a single
-- shared implementation of tools that can analyse both protocols, e.g.
-- wireshark plugins.
--
instance ProtocolEnum NodeToNodeProtocols where

  fromProtocolEnum ChainSyncWithHeadersPtcl = 2
  fromProtocolEnum BlockFetchPtcl           = 3
  fromProtocolEnum TxSubmissionPtcl         = 4

  toProtocolEnum 2 = Just ChainSyncWithHeadersPtcl
  toProtocolEnum 3 = Just BlockFetchPtcl
  toProtocolEnum 4 = Just TxSubmissionPtcl
  toProtocolEnum _ = Nothing

instance MiniProtocolLimits NodeToNodeProtocols where
  -- TODO: provide sensible limits
  -- https://github.com/input-output-hk/ouroboros-network/issues/575
  maximumMessageSize  _ = 0xffffffff
  maximumIngressQueue _ = 0xffffffff

-- | Enumeration of node to node protocol versions.
--
data NodeToNodeVersion = NodeToNodeV_1
  deriving (Eq, Ord, Enum, Show, Typeable)

instance Serialise NodeToNodeVersion where
    encode NodeToNodeV_1 = CBOR.encodeWord 1
    decode = do
      tag <- CBOR.decodeWord
      case tag of
        1 -> return NodeToNodeV_1
        _ -> fail "decode NodeToNodeVersion: unknown tag"

-- | Version data for NodeToNode protocol v1
--
newtype NodeToNodeVersionData = NodeToNodeVersionData
  { networkMagic :: NetworkMagic }
  deriving (Eq, Show, Typeable)

nodeToNodeCodecCBORTerm :: CodecCBORTerm Text NodeToNodeVersionData
nodeToNodeCodecCBORTerm = CodecCBORTerm {encodeTerm, decodeTerm}
    where
      encodeTerm :: NodeToNodeVersionData -> CBOR.Term
      encodeTerm NodeToNodeVersionData { networkMagic } =
        CBOR.TInt (fromIntegral $ unNetworkMagic networkMagic)

      decodeTerm :: CBOR.Term -> Either Text NodeToNodeVersionData
      decodeTerm (CBOR.TInt x) | x >= 0 && x <= 0xffffffff = Right (NodeToNodeVersionData $ NetworkMagic $ fromIntegral x)
                               | otherwise                 = Left $ T.pack $ "networkMagic out of bound: " <> show x
      decodeTerm t             = Left $ T.pack $ "unknown encoding: " ++ show t


-- | A specialised version of @'Ouroboros.Network.Socket.connectToNode'@.
--
connectTo
  :: Show peerid
  => Tracer IO (WithMuxBearer (MuxTrace NodeToNodeProtocols))
  -> Tracer IO (TraceSendRecv (Handshake NodeToNodeVersion CBOR.Term) peerid (DecoderFailureOrTooMuchInput DeserialiseFailure))
  -> (Socket.SockAddr -> Socket.SockAddr -> peerid)
  -- ^ create peerid from local address and remote address
  -> Versions NodeToNodeVersion
              DictVersion
              (OuroborosApplication InitiatorApp peerid NodeToNodeProtocols IO BL.ByteString a b)
  -> Maybe Socket.AddrInfo
  -> Socket.AddrInfo
  -> IO ()
connectTo =
  connectToNode
    (\(DictVersion codec) -> encodeTerm codec)
    (\(DictVersion codec) -> decodeTerm codec)


-- | Like 'connectTo' but specific to 'NodeToNodeV_1'.
--
connectTo_V1
  :: Show peerid
  => Tracer IO (WithMuxBearer (MuxTrace NodeToNodeProtocols))
  -> Tracer IO (TraceSendRecv (Handshake NodeToNodeVersion CBOR.Term) peerid (DecoderFailureOrTooMuchInput DeserialiseFailure))
  -> (Socket.SockAddr -> Socket.SockAddr -> peerid)
  -- ^ create peerid from local address and remote address
  -> NodeToNodeVersionData
  -> (OuroborosApplication InitiatorApp peerid NodeToNodeProtocols IO BL.ByteString a b)
  -> Maybe Socket.AddrInfo
  -> Socket.AddrInfo
  -> IO ()
connectTo_V1 muxTracer handshakeTracer peeridFn versionData application localAddr remoteAddr =
    connectTo
      muxTracer handshakeTracer
      peeridFn
      (simpleSingletonVersions
          NodeToNodeV_1
          versionData
          (DictVersion nodeToNodeCodecCBORTerm)
          application)
      localAddr
      remoteAddr

-- | A specialised version of @'Ouroboros.Network.Socket.withServerNode'@
--
withServer
  :: ( HasResponder appType ~ True, Show peerid)
  => Tracer IO (WithMuxBearer (MuxTrace NodeToNodeProtocols))
  -> Tracer IO (TraceSendRecv (Handshake NodeToNodeVersion CBOR.Term) peerid (DecoderFailureOrTooMuchInput DeserialiseFailure))
  -> ConnectionTable IO Socket.SockAddr
  -> Socket.AddrInfo
  -> (Socket.SockAddr -> Socket.SockAddr -> peerid)
  -- ^ create peerid from local address and remote address
  -> (forall vData. DictVersion vData -> vData -> vData -> Accept)
  -> Versions NodeToNodeVersion DictVersion (OuroborosApplication appType peerid NodeToNodeProtocols IO BL.ByteString a b)
  -> (Async () -> IO t)
  -> IO t
withServer muxTracer handshakeTracer tbl addr peeridFn acceptVersion versions k =
  withServerNode
    muxTracer
    handshakeTracer
    tbl
    addr
    (\(DictVersion codec) -> encodeTerm codec)
    (\(DictVersion codec) -> decodeTerm codec)
    peeridFn
    acceptVersion
    versions
    (\_ -> k)


-- | Like 'withServer' but specific to 'NodeToNodeV_1'.
--
withServer_V1
  :: ( HasResponder appType ~ True, Show peerid )
  => Tracer IO (WithMuxBearer (MuxTrace NodeToNodeProtocols))
  -> Tracer IO (TraceSendRecv (Handshake NodeToNodeVersion CBOR.Term) peerid (DecoderFailureOrTooMuchInput DeserialiseFailure))
  -> ConnectionTable IO Socket.SockAddr
  -> Socket.AddrInfo
  -> (Socket.SockAddr -> Socket.SockAddr -> peerid)
  -- ^ create peerid from local address and remote address
  -> NodeToNodeVersionData
  -> (OuroborosApplication appType peerid NodeToNodeProtocols IO BL.ByteString x y)
  -> (Async () -> IO t)
  -> IO t
withServer_V1 muxTracer handshakeTracer tbl addr peeridFn versionData application k =
    withServer
      muxTracer handshakeTracer tbl addr peeridFn
      (\(DictVersion _) -> acceptEq)
      (simpleSingletonVersions
          NodeToNodeV_1
          versionData
          (DictVersion nodeToNodeCodecCBORTerm)
          application)
      k


-- | 'ipSubscriptionWorker' which starts given application versions on each
-- established connection.
--
ipSubscriptionWorker
    :: forall appType peerid void x y.
       ( HasInitiator appType ~ True
       , Show peerid )
    => Tracer IO (WithIPList (SubscriptionTrace Socket.SockAddr))
    -> Tracer IO (WithMuxBearer (MuxTrace NodeToNodeProtocols))
    -> Tracer IO
        (TraceSendRecv
          (Handshake NodeToNodeVersion CBOR.Term)
          peerid
          (DecoderFailureOrTooMuchInput DeserialiseFailure))
    -> (Socket.SockAddr -> Socket.SockAddr -> peerid)
    -> ConnectionTable IO Socket.SockAddr
    -> LocalAddresses Socket.SockAddr
    -> (Socket.SockAddr -> Maybe DiffTime)
    -- ^ Lookup function, should return expected delay for the given address
    -> IPSubscriptionTarget
    -> Versions
        NodeToNodeVersion
        DictVersion
        (OuroborosApplication
          appType
          peerid
          NodeToNodeProtocols
          IO BL.ByteString x y)
    -> IO void
ipSubscriptionWorker
  subscriptionTracer
  muxTracer
  handshakeTracer
  peeridFn
  tbl
  localAddr
  connectionAttemptDelay
  ips
  versions
    = Subscription.ipSubscriptionWorker
        subscriptionTracer
        tbl
        localAddr
        connectionAttemptDelay
        ips
        (\_ -> retry)
        (connectToNode'
          (\(DictVersion codec) -> encodeTerm codec)
          (\(DictVersion codec) -> decodeTerm codec)
          muxTracer
          handshakeTracer
          peeridFn
          versions)


-- | Like 'ipSubscriptionWorker' but specific to 'NodeToNodeV_1'.
--
ipSubscriptionWorker_V1
    :: forall appType peerid void x y.
       ( HasInitiator appType ~ True
       , Show peerid )
    => Tracer IO (WithIPList (SubscriptionTrace Socket.SockAddr))
    -> Tracer IO (WithMuxBearer (MuxTrace NodeToNodeProtocols))
    -> Tracer IO (TraceSendRecv (Handshake NodeToNodeVersion CBOR.Term) peerid (DecoderFailureOrTooMuchInput DeserialiseFailure))
    -> (Socket.SockAddr -> Socket.SockAddr -> peerid)
    -> ConnectionTable IO Socket.SockAddr
    -> LocalAddresses Socket.SockAddr
    -> (Socket.SockAddr -> Maybe DiffTime)
    -- ^ Lookup function, should return expected delay for the given address
    -> IPSubscriptionTarget
    -> NodeToNodeVersionData
    -> (OuroborosApplication
          appType
          peerid
          NodeToNodeProtocols
          IO BL.ByteString x y)
    -> IO void
ipSubscriptionWorker_V1
  subscriptionTracer
  muxTracer
  handshakeTracer
  peeridFn
  tbl
  localAddresses
  connectionAttemptDelay
  ips
  versionData
  application
    = ipSubscriptionWorker
        subscriptionTracer
        muxTracer
        handshakeTracer
        peeridFn
        tbl
        localAddresses
        connectionAttemptDelay
        ips
        (simpleSingletonVersions
          NodeToNodeV_1
          versionData
          (DictVersion nodeToNodeCodecCBORTerm)
          application)


-- | 'dnsSubscriptionWorker' which starts given application versions on each
-- established connection.
--
dnsSubscriptionWorker
    :: forall appType peerid x y void.
       ( HasInitiator appType ~ True
       , Show peerid
       )
    => Tracer IO (WithDomainName (SubscriptionTrace Socket.SockAddr))
    -> Tracer IO (WithDomainName DnsTrace)
    -> Tracer IO (WithMuxBearer (MuxTrace NodeToNodeProtocols))
    -> Tracer IO
        (TraceSendRecv
          (Handshake NodeToNodeVersion CBOR.Term)
          peerid
          (DecoderFailureOrTooMuchInput DeserialiseFailure))
    -> (Socket.SockAddr -> Socket.SockAddr -> peerid)
    -> ConnectionTable IO Socket.SockAddr
    -> LocalAddresses Socket.SockAddr
    -> (Socket.SockAddr -> Maybe DiffTime)
    -> DnsSubscriptionTarget
    -> Versions
        NodeToNodeVersion
        DictVersion
        (OuroborosApplication
          appType
          peerid
          NodeToNodeProtocols
          IO BL.ByteString x y)
    -> IO void
dnsSubscriptionWorker
  subscriptionTracer
  dnsTracer
  muxTracer
  handshakeTracer
  peeridFn
  tbl
  localAddresses
  connectionAttemptDelay
  dst
  versions =
    Subscription.dnsSubscriptionWorker
      subscriptionTracer
      dnsTracer
      tbl
      localAddresses
      connectionAttemptDelay
      dst
      (\_ -> retry)
      (connectToNode'
        (\(DictVersion codec) -> encodeTerm codec)
        (\(DictVersion codec) -> decodeTerm codec)
        muxTracer
        handshakeTracer
        peeridFn
        versions)


-- | Like 'dnsSubscriptionWorker' but specific to 'NodeToNodeV_1'.
--
dnsSubscriptionWorker_V1
    :: forall appType peerid x y void.
       ( HasInitiator appType ~ True
       , Show peerid
       )
    => Tracer IO (WithDomainName (SubscriptionTrace Socket.SockAddr))
    -> Tracer IO (WithDomainName DnsTrace)
    -> Tracer IO (WithMuxBearer (MuxTrace NodeToNodeProtocols))
    -> Tracer IO (TraceSendRecv (Handshake NodeToNodeVersion CBOR.Term) peerid (DecoderFailureOrTooMuchInput DeserialiseFailure))
    -> (Socket.SockAddr -> Socket.SockAddr -> peerid)
    -> ConnectionTable IO Socket.SockAddr
    -> LocalAddresses Socket.SockAddr
    -> (Socket.SockAddr -> Maybe DiffTime)
    -> DnsSubscriptionTarget
    -> NodeToNodeVersionData
    -> (OuroborosApplication
          appType
          peerid
          NodeToNodeProtocols
          IO BL.ByteString x y)
    -> IO void
dnsSubscriptionWorker_V1
  subscriptionTracer
  dnsTracer
  muxTracer
  handshakeTracer
  peeridFn
  tbl
  localAddresses
  connectionAttemptDelay
  dst
  versionData
  application =
     dnsSubscriptionWorker
      subscriptionTracer
      dnsTracer
      muxTracer
      handshakeTracer
      peeridFn
      tbl
      localAddresses
      connectionAttemptDelay
      dst
      (simpleSingletonVersions
          NodeToNodeV_1
          versionData
          (DictVersion nodeToNodeCodecCBORTerm)
          application)
