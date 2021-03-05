{- |
Copyright: (c) 2021 Input Output (Hong Kong) Ltd.
Maintainer: Denis Shevchenko <denis.shevchenko@iohk.io>

See README for more info
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE PolyKinds #-}


-- This top-level module will be used by the forwarder app
-- (the app that collects EKG metrics and sends them to the acceptor).
--
module System.Metrics.Forwarder (
  ekgForwarder
  ) where

import qualified Data.ByteString.Lazy as LBS
import           GHC.Generics (Generic)
import           Data.Void (Void)

import           Control.Concurrent.Async
import           Control.Tracer

import qualified Codec.Serialise as CBOR

import           Ouroboros.Network.Mux
import           Ouroboros.Network.NodeToNode
import           Ouroboros.Network.IOManager
import           Ouroboros.Network.Snocket
import           Ouroboros.Network.Socket
import           Ouroboros.Network.Util.ShowProxy (ShowProxy(..))

import           Ouroboros.Network.Codec
import           Ouroboros.Network.Protocol.Handshake.Codec
import           Ouroboros.Network.Protocol.Handshake.Unversioned
import           Ouroboros.Network.Protocol.Handshake.Version

import qualified System.Metrics.Internal.Protocol.Forwarder as Forwarder
import qualified System.Metrics.Internal.Protocol.Codec as Forwarder
import qualified System.Metrics.Internal.Protocol.Type as Forwarder
import           System.Metrics.Type

ekgForwarder :: FilePath -> IO ()
ekgForwarder sockPath = withIOManager $ \iocp -> do
  -- threadDelay (50000 * index)
  connectToNode
    (localSnocket iocp sockPath)
    unversionedHandshakeCodec
    (cborTermVersionDataCodec unversionedProtocolDataCodec)
    nullNetworkConnectTracers
    acceptableVersion
    (simpleSingletonVersions
       UnversionedProtocol
       UnversionedProtocolData
       app)
    Nothing
    (localAddressFromPath sockPath)

 where
  app :: OuroborosApplication InitiatorMode addr LBS.ByteString IO () Void
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
  :: Forwarder.EKGForwarder Req Resp IO ()
ekgForwarderActions =
  Forwarder.EKGForwarder {
    Forwarder.recvMsgReq = \_req -> return (SimpleResp [1, 2, 3], ekgForwarderActions)
  , Forwarder.recvMsgDone = return ()
  }

maximumMiniProtocolLimits :: MiniProtocolLimits
maximumMiniProtocolLimits =
    MiniProtocolLimits {
      maximumIngressQueue = maxBound
    }
