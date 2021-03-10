{- |
Copyright: (c) 2021 Input Output (Hong Kong) Ltd.
Maintainer: Denis Shevchenko <denis.shevchenko@iohk.io>

See README for more info
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

-- | This top-level module will be used by the acceptor app
-- (the app that asks EKG metrics from the forwarder app).
--
module System.Metrics.Acceptor (
  runEKGAcceptor
  ) where

import qualified Codec.Serialise as CBOR
import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async (async, wait)
import           Control.Tracer (contramap, stdoutTracer)
import qualified Data.ByteString.Lazy as LBS
import           Data.Functor (void)
import qualified Data.Text as T
import           Data.Void (Void)
import qualified Network.Socket as Socket

import           Ouroboros.Network.Mux (MiniProtocol (..), MiniProtocolLimits (..),
                                        MiniProtocolNum (..), MuxMode (..),
                                        OuroborosApplication (..), MuxPeer (..),
                                        RunMiniProtocol (..),
                                        miniProtocolLimits, miniProtocolNum, miniProtocolRun)
import           Ouroboros.Network.ErrorPolicy (nullErrorPolicies)
import           Ouroboros.Network.IOManager (withIOManager)
import           Ouroboros.Network.Snocket (localAddressFromPath, localSnocket, socketSnocket)
import           Ouroboros.Network.Socket (AcceptedConnectionsLimit (..),
                                           SomeResponderApplication (..),
                                           cleanNetworkMutableState, newNetworkMutableState,
                                           nullNetworkServerTracers, withServerNode)
import           Ouroboros.Network.Codec (Codec)
import           Ouroboros.Network.Protocol.Handshake.Codec (cborTermVersionDataCodec)
import           Ouroboros.Network.Protocol.Handshake.Unversioned (UnversionedProtocol (..),
                                                                   UnversionedProtocolData (..),
                                                                   unversionedHandshakeCodec,
                                                                   unversionedProtocolDataCodec)
import           Ouroboros.Network.Protocol.Handshake.Version (acceptableVersion, simpleSingletonVersions)

import qualified System.Metrics as EKG

import qualified System.Metrics.Internal.Protocol.Acceptor as Acceptor
import qualified System.Metrics.Internal.Protocol.Codec as Acceptor
import qualified System.Metrics.Internal.Protocol.Type as Acceptor
import           System.Metrics.Request (Request (..))
import           System.Metrics.Response (Response (..))
import           System.Metrics.Configuration (AcceptorConfiguration (..), HowToConnect (..),
                                               RequestFrequency (..), TimePeriod (..),
                                               WhatToRequest (..))

-- | Please note that acceptor is a server from the __networking__ point of view:
-- the forwarder establishes network connection with the acceptor.
--
runEKGAcceptor
  :: AcceptorConfiguration
  -> (Response -> IO ())
  -> EKG.Store
  -> IO ()
runEKGAcceptor config actionOnResponse ekgStore =
  void $ runEKGAcceptor' config actionOnResponse ekgStore

runEKGAcceptor'
  :: AcceptorConfiguration
  -> (Response -> IO ())
  -> EKG.Store
  -> IO Void
runEKGAcceptor' config actionOnResponse ekgStore = withIOManager $ \iocp ->
  case listenToForwarder config of
    LocalPipe localPipe -> do
      networkState <- newNetworkMutableState
      _ <- async $ cleanNetworkMutableState networkState
      withServerNode
        (localSnocket iocp localPipe)
        nullNetworkServerTracers
        networkState
        (AcceptedConnectionsLimit maxBound maxBound 0)
        (localAddressFromPath localPipe)
        unversionedHandshakeCodec
        (cborTermVersionDataCodec unversionedProtocolDataCodec)
        acceptableVersion
        (simpleSingletonVersions
          UnversionedProtocol
          UnversionedProtocolData
          (SomeResponderApplication app))
        nullErrorPolicies
        $ \_ serverAsync -> wait serverAsync -- Block until async exception.
    RemoteSocket host port -> do
      listenAddress:_ <- Socket.getAddrInfo Nothing (Just $ T.unpack host) (Just $ show port)
      networkState <- newNetworkMutableState
      _ <- async $ cleanNetworkMutableState networkState
      withServerNode
        (socketSnocket iocp)
        nullNetworkServerTracers
        networkState
        (AcceptedConnectionsLimit maxBound maxBound 0)
        (Socket.addrAddress listenAddress)
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
      MiniProtocol
        { miniProtocolNum    = MiniProtocolNum 2
        , miniProtocolLimits = MiniProtocolLimits {
                                 maximumIngressQueue = maxBound
                               }
        , miniProtocolRun    = acceptEKGMetrics
        }
    ]

  acceptEKGMetrics :: RunMiniProtocol 'ResponderMode LBS.ByteString IO Void ()
  acceptEKGMetrics =
    ResponderProtocolOnly $
      MuxPeer
        (contramap show stdoutTracer)
        codecEKGForward
        (Acceptor.ekgAcceptorPeer $
          ekgAcceptorActions config actionOnResponse ekgStore)

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

ekgAcceptorActions
  :: AcceptorConfiguration
  -> (Response -> IO ())
  -> EKG.Store
  -> Acceptor.EKGAcceptor Request Response IO ()
ekgAcceptorActions config@AcceptorConfiguration {..} actionOnResponse ekgStore =
  Acceptor.SendMsgReq request $ \response -> do
    threadDelay $ mkDelay requestFrequency -- Temporary function, see below.
    actionOnResponse response
    return $ ekgAcceptorActions config actionOnResponse ekgStore
 where
  request =
    case whatToRequest of
      AllMetrics -> GetAllMetrics
      TheseMetrics mNames -> GetMetrics mNames

  -- TODO: temporary function, should be rewritten
  -- (we have to take into account actual time of 'actionOnResponse'
  -- as well as actual time of getting the response from the forwarder).
  mkDelay (AskMetricsEvery delay Seconds)      = fromIntegral delay * 1000000
  mkDelay (AskMetricsEvery delay MilliSeconds) = fromIntegral delay * 1000
