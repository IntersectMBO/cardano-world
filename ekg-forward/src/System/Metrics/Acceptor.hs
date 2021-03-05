{- |
Copyright: (c) 2021 Input Output (Hong Kong) Ltd.
Maintainer: Denis Shevchenko <denis.shevchenko@iohk.io>

See README for more info
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}

-- | This top-level module will be used by the acceptor app
-- (the app that asks EKG metrics from the forwarder app).
--
module System.Metrics.Acceptor (
  ekgAcceptor
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

import qualified System.Metrics.Internal.Protocol.Acceptor as Acceptor
import qualified System.Metrics.Internal.Protocol.Codec as Acceptor
import qualified System.Metrics.Internal.Protocol.Type as Acceptor
import           System.Metrics.Type

-- | Please note that acceptor is a server from the __networking__ point of view:
-- the forwarder establishes network connection with the acceptor.
--
ekgAcceptor :: FilePath -> IO Void
ekgAcceptor sockAddr = withIOManager $ \iocp -> do
    networkState <- newNetworkMutableState
    _ <- async $ cleanNetworkMutableState networkState
    withServerNode
      (localSnocket iocp defaultLocalSocketAddrPath)
      nullNetworkServerTracers
      networkState
      (AcceptedConnectionsLimit maxBound maxBound 0)
      (localAddressFromPath sockAddr)
      unversionedHandshakeCodec
      (cborTermVersionDataCodec unversionedProtocolDataCodec)
      acceptableVersion
      (simpleSingletonVersions
        UnversionedProtocol
        UnversionedProtocolData
        (SomeResponderApplication app))
      nullErrorPolicies
      $ \_ serverAsync -> wait serverAsync -- Block until async exception.
  where
    app :: OuroborosApplication 'ResponderMode addr LBS.ByteString IO Void ()
    app =
      OuroborosApplication $ \_connectionId _shouldStopSTM -> [
        MiniProtocol {
          miniProtocolNum    = MiniProtocolNum 2,
          miniProtocolLimits = maximumMiniProtocolLimits,
          miniProtocolRun    = acceptEKGMetrics
        }
      ]

    acceptEKGMetrics :: RunMiniProtocol 'ResponderMode LBS.ByteString IO Void ()
    acceptEKGMetrics =
      ResponderProtocolOnly $
        MuxPeer
          (contramap show stdoutTracer)
          codecEKGForward
          (Acceptor.ekgAcceptorPeer (ekgAcceptorActions 0))

codecEKGForward
  :: ( CBOR.Serialise req
     , CBOR.Serialise resp
     )
  => Codec (Acceptor.EKGForward req resp)
           CBOR.DeserialiseFailure
           IO LBS.ByteString
codecEKGForward =
  Acceptor.codecEKGForward
    CBOR.encode CBOR.decode
    CBOR.encode CBOR.decode

ekgAcceptorActions :: Int -> Acceptor.EKGAcceptor Req Resp IO ()
ekgAcceptorActions 0 = Acceptor.SendMsgReq SimpleReq (\resp -> do
                                                        print resp
                                                        return (ekgAcceptorActions 1))
ekgAcceptorActions _ = Acceptor.SendMsgDone (return ())

defaultLocalSocketAddrPath :: FilePath
#if defined(mingw32_HOST_OS)
defaultLocalSocketAddrPath =  "\\\\.\\pipe\\demo-ekg-forward"
#else
defaultLocalSocketAddrPath =  "./demo-ekg-forward.sock"
#endif

maximumMiniProtocolLimits :: MiniProtocolLimits
maximumMiniProtocolLimits =
    MiniProtocolLimits {
      maximumIngressQueue = maxBound
    }
