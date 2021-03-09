{- |
Copyright: (c) 2021 Input Output (Hong Kong) Ltd.
Maintainer: Denis Shevchenko <denis.shevchenko@iohk.io>

See README for more info
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- This top-level module will be used by the forwarder app
-- (the app that collects EKG metrics and sends them to the acceptor).
--
module System.Metrics.Forwarder (
  runEKGForwarder
  ) where

import qualified Codec.Serialise as CBOR
import           Control.Tracer (contramap, stdoutTracer)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import           Data.Void (Void)
import qualified Network.Socket as Socket

import           Ouroboros.Network.Mux (MiniProtocol (..), MiniProtocolLimits (..),
                                        MiniProtocolNum (..), MuxMode (..),
                                        OuroborosApplication (..), MuxPeer (..),
                                        RunMiniProtocol (..),
                                        miniProtocolLimits, miniProtocolNum, miniProtocolRun)
import           Ouroboros.Network.NodeToNode (simpleSingletonVersions)
import           Ouroboros.Network.IOManager (withIOManager)
import           Ouroboros.Network.Snocket (localAddressFromPath, localSnocket, socketSnocket)
import           Ouroboros.Network.Socket (connectToNode, nullNetworkConnectTracers)
import           Ouroboros.Network.Codec (Codec)
import           Ouroboros.Network.Protocol.Handshake.Codec (cborTermVersionDataCodec)
import           Ouroboros.Network.Protocol.Handshake.Unversioned (UnversionedProtocol (..),
                                                                   UnversionedProtocolData (..),
                                                                   unversionedHandshakeCodec,
                                                                   unversionedProtocolDataCodec)
import           Ouroboros.Network.Protocol.Handshake.Version (acceptableVersion)

import qualified System.Metrics.Internal.Protocol.Forwarder as Forwarder
import qualified System.Metrics.Internal.Protocol.Codec as Forwarder
import qualified System.Metrics.Internal.Protocol.Type as Forwarder
import           System.Metrics.Internal.Request (Request (..))
import           System.Metrics.Internal.Response (Response (..), MetricValue (..))
import           System.Metrics.Configuration (ForwarderConfiguration (..), HowToConnect (..))

runEKGForwarder :: ForwarderConfiguration -> IO ()
runEKGForwarder ForwarderConfiguration{..} = withIOManager $ \iocp ->
  case connectToAcceptor of
    LocalPipe localPipe ->
      connectToNode
        (localSnocket iocp localPipe)
        unversionedHandshakeCodec
        (cborTermVersionDataCodec unversionedProtocolDataCodec)
        nullNetworkConnectTracers
        acceptableVersion
        (simpleSingletonVersions
           UnversionedProtocol
           UnversionedProtocolData
           app)
        Nothing
        (localAddressFromPath localPipe)
    RemoteSocket host port -> do
      acceptorAddress:_ <- Socket.getAddrInfo Nothing (Just $ T.unpack host) (Just $ show port)
      connectToNode
        (socketSnocket iocp)
        unversionedHandshakeCodec
        (cborTermVersionDataCodec unversionedProtocolDataCodec)
        nullNetworkConnectTracers
        acceptableVersion
        (simpleSingletonVersions
           UnversionedProtocol
           UnversionedProtocolData
           app)
        Nothing
        (Socket.addrAddress acceptorAddress)
 where
  app :: OuroborosApplication 'InitiatorMode addr LBS.ByteString IO () Void
  app =
    OuroborosApplication $ \_connectionId _shouldStopSTM -> [
        MiniProtocol {
          miniProtocolNum    = MiniProtocolNum 2,
          miniProtocolLimits = maximumMiniProtocolLimits,
          miniProtocolRun    = forwardEKGMetrics
        }
      ]

  forwardEKGMetrics :: RunMiniProtocol 'InitiatorMode LBS.ByteString IO () Void
  forwardEKGMetrics =
    InitiatorProtocolOnly $
      MuxPeer
        (contramap show stdoutTracer)
        codecEKGForward
        (Forwarder.ekgForwarderPeer ekgForwarderActions)

codecEKGForward
  :: ( CBOR.Serialise req
     , CBOR.Serialise resp
     )
  => Codec (Forwarder.EKGForward req resp)
           CBOR.DeserialiseFailure
           IO LBS.ByteString
codecEKGForward =
  Forwarder.codecEKGForward
    CBOR.encode CBOR.decode
    CBOR.encode CBOR.decode

ekgForwarderActions
  :: Forwarder.EKGForwarder Request Response IO ()
ekgForwarderActions =
  Forwarder.EKGForwarder {
    Forwarder.recvMsgReq = \_req ->
      return ( Metrics (NE.fromList [ ("metric1", GaugeValue 1)
                                    , ("metric2", GaugeValue 2)
                                    , ("metric3", GaugeValue 3)
                                    ]
                       )
             , ekgForwarderActions
             )
  , Forwarder.recvMsgDone = return ()
  }

maximumMiniProtocolLimits :: MiniProtocolLimits
maximumMiniProtocolLimits =
    MiniProtocolLimits {
      maximumIngressQueue = maxBound
    }
