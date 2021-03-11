{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

module System.Metrics.Forwarder.Network
  ( connectToAcceptor
  ) where

import qualified Codec.Serialise as CBOR
import           Control.Tracer (contramap, stdoutTracer)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import           Data.Void (Void)
import qualified Network.Socket as Socket
import qualified System.Metrics as EKG

import           Ouroboros.Network.IOManager (withIOManager)
import           Ouroboros.Network.Mux (MiniProtocol (..), MiniProtocolLimits (..),
                                        MiniProtocolNum (..), MuxMode (..),
                                        OuroborosApplication (..), MuxPeer (..),
                                        RunMiniProtocol (..),
                                        miniProtocolLimits, miniProtocolNum, miniProtocolRun)
import           Ouroboros.Network.Protocol.Handshake.Codec (cborTermVersionDataCodec)
import           Ouroboros.Network.Protocol.Handshake.Unversioned (UnversionedProtocol (..),
                                                                   UnversionedProtocolData (..),
                                                                   unversionedHandshakeCodec,
                                                                   unversionedProtocolDataCodec)
import           Ouroboros.Network.Protocol.Handshake.Version (acceptableVersion, simpleSingletonVersions)
import           Ouroboros.Network.Snocket (Snocket, localAddressFromPath, localSnocket, socketSnocket)
import           Ouroboros.Network.Socket (connectToNode, nullNetworkConnectTracers)

import           System.Metrics.Configuration (ForwarderConfiguration (..), HowToConnect (..))
import           System.Metrics.Forwarder.Store (mkResponse)
import qualified System.Metrics.Internal.Protocol.Forwarder as Forwarder
import qualified System.Metrics.Internal.Protocol.Codec as Forwarder

connectToAcceptor
  :: ForwarderConfiguration
  -> EKG.Store
  -> IO ()
connectToAcceptor ForwarderConfiguration{..} ekgStore = withIOManager $ \iocp ->
  case acceptorEndpoint of
    LocalPipe localPipe -> do
      let snocket = localSnocket iocp localPipe
          address = localAddressFromPath localPipe
      doConnectToAcceptor snocket address ekgStore
    RemoteSocket host port -> do
      acceptorAddr:_ <- Socket.getAddrInfo Nothing (Just $ T.unpack host) (Just $ show port)
      let snocket = socketSnocket iocp
          address = Socket.addrAddress acceptorAddr
      doConnectToAcceptor snocket address ekgStore

doConnectToAcceptor
  :: Snocket IO fd addr
  -> addr
  -> EKG.Store
  -> IO ()
doConnectToAcceptor snocket address ekgStore =
  connectToNode
    snocket
    unversionedHandshakeCodec
    (cborTermVersionDataCodec unversionedProtocolDataCodec)
    nullNetworkConnectTracers
    acceptableVersion
    (simpleSingletonVersions
       UnversionedProtocol
       UnversionedProtocolData
       $ forwarderApp ekgStore)
    Nothing
    address

forwarderApp
  :: EKG.Store
  -> OuroborosApplication 'InitiatorMode addr LBS.ByteString IO () Void
forwarderApp ekgStore =
  OuroborosApplication $ \_connectionId _shouldStopSTM ->
    [ MiniProtocol
        { miniProtocolNum    = MiniProtocolNum 2
        , miniProtocolLimits = MiniProtocolLimits { maximumIngressQueue = maxBound }
        , miniProtocolRun    = forwardEKGMetrics ekgStore
        }
    ]

forwardEKGMetrics
  :: EKG.Store
  -> RunMiniProtocol 'InitiatorMode LBS.ByteString IO () Void
forwardEKGMetrics ekgStore =
  InitiatorProtocolOnly $
    MuxPeer
      (contramap show stdoutTracer)
      (Forwarder.codecEKGForward CBOR.encode CBOR.decode
                                 CBOR.encode CBOR.decode)
      (Forwarder.ekgForwarderPeer $ mkResponse ekgStore)
