{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}

{-# OPTIONS_GHC -Wno-orphans            #-}

module Test.Mux
    ( tests
    ) where

import           Codec.CBOR.Decoding as CBOR
import           Codec.CBOR.Encoding as CBOR
import           Codec.Serialise (Serialise (..))
import           Control.Monad
import           Control.Tracer
import qualified Data.Binary.Put as Bin
import           Data.Bits
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8 (pack)
import           Data.List (dropWhileEnd)
import           Data.Int
import           Data.Tuple (swap)
import           Data.Word
import           Test.QuickCheck
import           Test.QuickCheck.Gen
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)
import           Text.Printf
import qualified System.Random.SplitMix as SM

import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadSay
import           Control.Monad.Class.MonadST
import           Control.Monad.Class.MonadSTM.Strict
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadTimer
import           Control.Monad.IOSim (runSimStrictShutdown)
import           Control.Tracer (nullTracer)

import           Test.Mux.ReqResp

import qualified Network.Mux as Mx
import qualified Network.Mux.Codec as Mx
import qualified Network.Mux.Channel as Mx
import qualified Network.Mux.Interface as Mx
import qualified Network.Mux.Bearer.Queues as Mx

tests :: TestTree
tests =
  testGroup "Mux"
  [ testProperty "mux send receive"     prop_mux_snd_recv
  , testProperty "2 miniprotocols"      prop_mux_2_minis
  , testProperty "starvation"           prop_mux_starvation
  , testProperty "demuxing (Sim)"       prop_demux_sdu_sim
  , testProperty "demuxing (IO)"        prop_demux_sdu_io
  , testGroup "Generators"
    [ testProperty "genByteString"      prop_arbitrary_genByteString
    , testProperty "genLargeByteString" prop_arbitrary_genLargeByteString
    ]
  ]

defaultMiniProtocolLimit :: Int64
defaultMiniProtocolLimit = 3000000

smallMiniProtocolLimit :: Int64
smallMiniProtocolLimit = 16*1024

activeTracer :: forall m a. (MonadSay m, Show a) => Tracer m a
activeTracer = nullTracer
--activeTracer = showTracing sayTracer

_sayTracer :: MonadSay m => Tracer m String
_sayTracer = Tracer say

--
-- Various ProtocolEnum instances used in tests
--

data TestProtocols1 = ReqResp1
  deriving (Eq, Ord, Enum, Bounded, Show)

instance Mx.ProtocolEnum TestProtocols1 where
  fromProtocolEnum ReqResp1 = 2

  toProtocolEnum 2 = Just ReqResp1
  toProtocolEnum _ = Nothing

instance Mx.MiniProtocolLimits TestProtocols1 where
  maximumMessageSize ReqResp1  = defaultMiniProtocolLimit
  maximumIngressQueue ReqResp1 = defaultMiniProtocolLimit

-- |
-- Allows to run two copies of ReqResp protocol.t
--
data TestProtocols2 = ReqResp2 | ReqResp3
  deriving (Eq, Ord, Enum, Bounded, Show)

instance Mx.ProtocolEnum TestProtocols2 where
  fromProtocolEnum ReqResp2 = 2
  fromProtocolEnum ReqResp3 = 3

  toProtocolEnum 2 = Just ReqResp2
  toProtocolEnum 3 = Just ReqResp3
  toProtocolEnum _ = Nothing

instance Mx.MiniProtocolLimits TestProtocols2 where
  maximumMessageSize _  = defaultMiniProtocolLimit
  maximumIngressQueue _ = defaultMiniProtocolLimit

data TestProtocolsSmall = ReqRespSmall
  deriving (Eq, Ord, Enum, Bounded, Show)

instance Mx.ProtocolEnum TestProtocolsSmall where
  fromProtocolEnum ReqRespSmall = 2

  toProtocolEnum 2 = Just ReqRespSmall
  toProtocolEnum _ = Nothing

instance Mx.MiniProtocolLimits TestProtocolsSmall where
  maximumMessageSize ReqRespSmall  = smallMiniProtocolLimit
  maximumIngressQueue ReqRespSmall = smallMiniProtocolLimit

--
-- Generators
--

newtype DummyPayload = DummyPayload {
      unDummyPayload :: BL.ByteString
    } deriving Eq

instance Show DummyPayload where
    show d = printf "DummyPayload %d\n" (BL.length $ unDummyPayload d)

-- |
-- Generate a byte string of a given size.
--
genByteString :: Int64 -> Gen BL.ByteString
genByteString size = do
    g0 <- return . SM.mkSMGen =<< chooseAny
    return $ BL.unfoldr gen (size, g0)
  where
    gen :: (Int64, SM.SMGen) -> Maybe (Word8, (Int64, SM.SMGen))
    gen (!i, !g)
        | i <= 0    = Nothing
        | otherwise = Just (fromIntegral w64, (i - 1, g'))
      where
        !(w64, g') = SM.nextWord64 g

prop_arbitrary_genByteString :: (NonNegative (Small Int64)) -> Property
prop_arbitrary_genByteString (NonNegative (Small size)) = ioProperty $ do
  bs <- generate $ genByteString size
  return $ size == BL.length bs

genLargeByteString :: Int64 -> Int64 -> Gen BL.ByteString
genLargeByteString chunkSize  size | chunkSize < size = do
  chunk <- genByteString chunkSize
  return $ BL.concat $
        replicate (fromIntegral $ size `div` chunkSize) chunk
      ++
        [BL.take (size `mod` chunkSize) chunk]
genLargeByteString _chunkSize size = genByteString size

-- |
-- Large Int64 values, but not too large, up to @1024*1024@.
--
newtype LargeInt64 = LargeInt64 Int64
  deriving (Eq, Ord, Num, Show)

instance Arbitrary LargeInt64 where
    arbitrary = LargeInt64 <$> choose (1, 1024*1024)
    shrink (LargeInt64 n) = map LargeInt64 $ shrink n

prop_arbitrary_genLargeByteString :: NonNegative LargeInt64 -> Property
prop_arbitrary_genLargeByteString (NonNegative (LargeInt64 size)) = ioProperty $ do
  bs <- generate $ genLargeByteString 1024 size
  return $ size == BL.length bs

instance Arbitrary DummyPayload where
    arbitrary = do
        n <- choose (1, 128)
        len <- oneof [ return n
                     , return $ defaultMiniProtocolLimit - n - cborOverhead
                     , choose (1, defaultMiniProtocolLimit - cborOverhead)
                     ]
        -- Generating a completly arbitrary bytestring is too costly so it is only
        -- done for short bytestrings.
        DummyPayload <$> genLargeByteString 1024 len
      where
        cborOverhead = 7 -- XXX Bytes needed by CBOR to encode the dummy payload

instance Serialise DummyPayload where
    encode a = CBOR.encodeBytes (BL.toStrict $ unDummyPayload a)
    decode = DummyPayload . BL.fromStrict <$> CBOR.decodeBytes

-- | A sequences of dummy requests and responses for test with the ReqResp protocol.
newtype DummyTrace = DummyTrace {unDummyTrace :: [(DummyPayload, DummyPayload)]}
    deriving (Show)

instance Arbitrary DummyTrace where
    arbitrary = do
        len <- choose (1, 20)
        DummyTrace <$> vector len

data InvalidSDU = InvalidSDU {
      isTimestamp  :: !Mx.RemoteClockModel
    , isIdAndMode  :: !Word16
    , isLength     :: !Word16
    , isRealLength :: !Int64
    , isPattern    :: !Word8
    }

instance Show InvalidSDU where
    show a = printf "InvalidSDU 0x%08x 0x%04x 0x%04x 0x%04x 0x%02x\n"
                    (Mx.unRemoteClockModel $ isTimestamp a)
                    (isIdAndMode a)
                    (isLength a)
                    (isRealLength a)
                    (isPattern a)

data ArbitrarySDU = ArbitraryInvalidSDU InvalidSDU Mx.MuxBearerState Mx.MuxErrorType
                  | ArbitraryValidSDU DummyPayload Mx.MuxBearerState (Maybe Mx.MuxErrorType)
                  deriving Show

instance Arbitrary ArbitrarySDU where
    arbitrary = oneof [ unknownMiniProtocol
                      , invalidLenght
                      , validSdu
                      , tooLargeSdu
                      ]
      where
        validSdu = do
            b <- arbitrary

            -- Valid SDUs before version negotiation does only make sense for single SDUs.
            state <- if BL.length (unDummyPayload b) < 0xffff
                         then arbitrary
                         else return Mx.Mature
            let err_m = if state == Mx.Larval || state == Mx.Connected
                            then Just Mx.MuxUnknownMiniProtocol
                            else Nothing

            return $ ArbitraryValidSDU b state err_m

        tooLargeSdu = do
            l <- choose (1 + smallMiniProtocolLimit , 2 * smallMiniProtocolLimit)
            pl <- BL8.pack <$> replicateM (fromIntegral l) arbitrary

            -- This SDU is still considered valid, since the header itself will
            -- not cause a trouble, the error will be triggered by the fact that
            -- it is sent as a single message.
            return $ ArbitraryValidSDU (DummyPayload pl) Mx.Mature (Just Mx.MuxIngressQueueOverRun)

        unknownMiniProtocol = do
            ts  <- arbitrary
            mid <- choose (6, 0x7fff) -- ClientChainSynWithBlocks with 5 is the highest valid mid
            mode <- oneof [return 0x0, return 0x8000]
            len <- arbitrary
            state <- arbitrary
            p <- arbitrary

            return $ ArbitraryInvalidSDU (InvalidSDU (Mx.RemoteClockModel ts) (mid .|. mode) len
                                          (8 + fromIntegral len) p)
                                         state
                                         Mx.MuxUnknownMiniProtocol
        invalidLenght = do
            ts  <- arbitrary
            mid <- arbitrary
            len <- arbitrary
            realLen <- choose (0, 7) -- Size of mux header is 8
            p <- arbitrary
            state <- arbitrary

            return $ ArbitraryInvalidSDU (InvalidSDU (Mx.RemoteClockModel ts) mid len realLen p)
                                         state
                                         Mx.MuxDecodeError

instance Arbitrary Mx.MuxBearerState where
     -- XXX Larval and Connected test behaviour is dependant on version
     -- negotation so they are disabled for now.
     arbitrary = elements [ -- Mx.Larval
                            -- , Mx.Connected
                            Mx.Mature
                          , Mx.Dying
                          , Mx.Dead
                          ]



-- | A pair of two bytestrings which lengths are unevenly distributed
--
data Uneven = Uneven DummyPayload DummyPayload
  deriving (Eq, Show)

instance Arbitrary Uneven where
    arbitrary = do
      n <- choose (1, 128)
      b <- arbitrary
      (l, k) <- (if b then swap else id) <$>
                oneof [ (n,) <$> choose (2 * n, defaultMiniProtocolLimit - cborOverhead)
                      , let k = defaultMiniProtocolLimit - n - cborOverhead
                        in (k,) <$> choose (1, k `div` 2)
                      , do
                          k <- choose (1, defaultMiniProtocolLimit - cborOverhead)
                          return (k, k `div` 2)
                      ]
      Uneven <$> (DummyPayload <$> genLargeByteString 1024 l)
             <*> (DummyPayload <$> genLargeByteString 1024 k)
      where
        cborOverhead = 7 -- XXX Bytes needed by CBOR to encode the dummy payload


--
-- QuickChekc Properties
--


-- | Verify that an initiator and a responder can send and receive messages
-- from each other.  Large DummyPayloads will be split into sduLen sized
-- messages and the testcases will verify that they are correctly reassembled
-- into the original message.
--
prop_mux_snd_recv :: DummyTrace
                  -> Property
prop_mux_snd_recv messages = ioProperty $ do
    let sduLen = 1260

    client_w <- atomically $ newTBQueue 10
    client_r <- atomically $ newTBQueue 10
    endMpsVar <- atomically $ newTVar 2

    let server_w = client_r
        server_r = client_w

    (verify, client_mp, server_mp) <- setupMiniReqRsp
                                        (return ()) endMpsVar messages

    let clientApp = Mx.MuxApplication
                      [ Mx.MuxMiniProtocol {
                          Mx.miniProtocolId  = ReqResp1,
                          Mx.miniProtocolCode= 2,
                          Mx.miniProtocolRun = Mx.InitiatorProtocolOnly (const client_mp)
                        }
                      ]

        serverApp = Mx.MuxApplication
                      [ Mx.MuxMiniProtocol {
                          Mx.miniProtocolId  = ReqResp1,
                          Mx.miniProtocolCode= 2,
                          Mx.miniProtocolRun = Mx.ResponderProtocolOnly (const server_mp)
                        }
                      ]

    clientAsync <-
      async $ Mx.runMuxWithQueues activeTracer "client"
                                  clientApp client_w client_r sduLen Nothing
    serverAsync <-
      async $ Mx.runMuxWithQueues activeTracer "server"
                                  serverApp server_w server_r sduLen Nothing

    r <- waitBoth clientAsync serverAsync
    case r of
         (Just _, _) -> return $ property False
         (_, Just _) -> return $ property False
         _           -> property <$> verify

-- | Create a verification function, a MiniProtocolDescription for the client
-- side and a MiniProtocolDescription for the server side for a RequestResponce
-- protocol.
--
setupMiniReqRsp :: IO ()              -- | Action performed by responder before processing the response
                -> StrictTVar IO Int  -- | Total number of miniprotocols.
                -> DummyTrace         -- | Trace of messages
                -> IO ( IO Bool
                      , Mx.Channel IO -> IO ()
                      , Mx.Channel IO -> IO ()
                      )
setupMiniReqRsp serverAction mpsEndVar (DummyTrace msgs) = do
    serverResultVar <- newEmptyTMVarM
    clientResultVar <- newEmptyTMVarM

    return ( verifyCallback serverResultVar clientResultVar
           , clientApp clientResultVar
           , serverApp serverResultVar
           )
  where
    requests  = map fst msgs
    responses = map snd msgs

    verifyCallback serverResultVar clientResultVar =
        atomically $ (&&) <$> takeTMVar serverResultVar <*> takeTMVar clientResultVar

    reqRespServer :: [DummyPayload]
                  -> ReqRespServer DummyPayload DummyPayload IO Bool
    reqRespServer = go []
      where
        go reqs (resp:resps) = ReqRespServer {
            recvMsgReq  = \req -> serverAction >> return (resp, go (req:reqs) resps),
            recvMsgDone = pure $ reverse reqs == requests
          }
        go reqs [] = ReqRespServer {
            recvMsgReq  = error "server out of replies",
            recvMsgDone = pure $ reverse reqs == requests
          }


    reqRespClient :: [DummyPayload]
                  -> ReqRespClient DummyPayload DummyPayload IO Bool
    reqRespClient = go []
      where
        go resps []         = SendMsgDone (pure $ reverse resps == responses)
        go resps (req:reqs) = SendMsgReq req $ \resp -> return (go (resp:resps) reqs)

    clientApp :: StrictTMVar IO Bool
              -> Mx.Channel IO
              -> IO ()
    clientApp clientResultVar clientChan = do
        result <- runClient nullTracer clientChan (reqRespClient requests)
        atomically (putTMVar clientResultVar result)
        end

    serverApp :: StrictTMVar IO Bool
              -> Mx.Channel IO
              -> IO ()
    serverApp serverResultVar serverChan = do
        result <- runServer nullTracer serverChan (reqRespServer responses)
        atomically (putTMVar serverResultVar result)
        end

    -- Wait on all miniprotocol jobs before letting a miniprotocol thread exit.
    end = do
        atomically $ modifyTVar mpsEndVar (\a -> a - 1)
        atomically $ do
            c <- readTVar mpsEndVar
            unless (c == 0) retry

waitOnAllClients :: StrictTVar IO Int
                 -> Int
                 -> IO ()
waitOnAllClients clientVar clientTot = do
        atomically $ modifyTVar clientVar (+ 1)
        atomically $ do
            c <- readTVar clientVar
            unless (c == clientTot) retry

-- | Verify that it is possible to run two miniprotocols over the same bearer.
-- Makes sure that messages are delivered to the correct miniprotocol in order.
--
prop_mux_2_minis :: DummyTrace
                 -> DummyTrace
                 -> Property
prop_mux_2_minis msgTrace0 msgTrace1 = ioProperty $ do
    let sduLen = 14000

    client_w <- atomically $ newTBQueue 10
    client_r <- atomically $ newTBQueue 10
    endMpsVar <- atomically $ newTVar 4 -- Two initiators and two responders.

    let server_w = client_r
        server_r = client_w

    (verify_0, client_mp0, server_mp0) <-
        setupMiniReqRsp (return ()) endMpsVar msgTrace0
    (verify_1, client_mp1, server_mp1) <-
        setupMiniReqRsp (return ()) endMpsVar msgTrace1

    let clientApp = Mx.MuxApplication
                      [ Mx.MuxMiniProtocol {
                          Mx.miniProtocolId  = ReqResp2,
                          Mx.miniProtocolCode= 2,
                          Mx.miniProtocolRun = Mx.InitiatorProtocolOnly (const client_mp0)
                        }
                      , Mx.MuxMiniProtocol {
                          Mx.miniProtocolId  = ReqResp3,
                          Mx.miniProtocolCode= 3,
                          Mx.miniProtocolRun = Mx.InitiatorProtocolOnly (const client_mp1)
                        }
                      ]

        serverApp = Mx.MuxApplication
                      [ Mx.MuxMiniProtocol {
                          Mx.miniProtocolId  = ReqResp2,
                          Mx.miniProtocolCode= 2,
                          Mx.miniProtocolRun = Mx.ResponderProtocolOnly (const server_mp0)
                        }
                      , Mx.MuxMiniProtocol {
                          Mx.miniProtocolId  = ReqResp3,
                          Mx.miniProtocolCode= 3,
                          Mx.miniProtocolRun = Mx.ResponderProtocolOnly (const server_mp1)
                        }
                      ]

    clientAsync <- async $ Mx.runMuxWithQueues activeTracer "client" clientApp client_w client_r sduLen Nothing
    serverAsync <- async $ Mx.runMuxWithQueues activeTracer "server" serverApp server_w server_r sduLen Nothing


    r <- waitBoth clientAsync serverAsync
    case r of
         (Just _, _) -> return $ property False
         (_, Just _) -> return $ property False
         _           -> do
             res0 <- verify_0
             res1 <- verify_1

             return $ property $ res0 .&&. res1

-- | Attempt to verify that capacity is diveded fairly between two active
-- miniprotocols.  Two initiators send a request over two different
-- miniprotocols and the corresponding responders each send a large reply back.
-- The Mux bearer should alternate between sending data for the two responders.
--
prop_mux_starvation :: Uneven
                    -> Property
prop_mux_starvation (Uneven response0 response1) =
    let sduLen = 1260 in
    (BL.length (unDummyPayload response0) > 2 * fromIntegral sduLen) &&
    (BL.length (unDummyPayload response1) > 2 * fromIntegral sduLen) ==>
    ioProperty $ do
    let request       = DummyPayload $ BL.replicate 4 0xa

    client_w <- atomically $ newTBQueue 10
    client_r <- atomically $ newTBQueue 10
    activeMpsVar <- atomically $ newTVar 0
    -- 2 active initiators and 2 active responders
    endMpsVar <- atomically $ newTVar 4
    -- At most track 100 packets per test run
    traceQueueVar <- atomically $ newTBQueue 100

    let server_w = client_r
        server_r = client_w

    (verify_short, client_short, server_short) <-
        setupMiniReqRsp (waitOnAllClients activeMpsVar 2)
                        endMpsVar  $ DummyTrace [(request, response0)]
    (verify_long, client_long, server_long) <-
        setupMiniReqRsp (waitOnAllClients activeMpsVar 2)
                        endMpsVar $ DummyTrace [(request, response1)]

    let clientApp = Mx.MuxApplication
                      [ Mx.MuxMiniProtocol {
                          Mx.miniProtocolId  = ReqResp2,
                          Mx.miniProtocolCode= 2,
                          Mx.miniProtocolRun = Mx.InitiatorProtocolOnly (const client_short)
                        }
                      , Mx.MuxMiniProtocol  {
                          Mx.miniProtocolId  = ReqResp3,
                          Mx.miniProtocolCode= 3,
                          Mx.miniProtocolRun = Mx.InitiatorProtocolOnly (const client_long)
                        }
                      ]

        serverApp = Mx.MuxApplication
                      [ Mx.MuxMiniProtocol {
                          Mx.miniProtocolId  = ReqResp2,
                          Mx.miniProtocolCode= 2,
                          Mx.miniProtocolRun = Mx.ResponderProtocolOnly (const server_short)
                        }
                      , Mx.MuxMiniProtocol {
                          Mx.miniProtocolId  = ReqResp3,
                          Mx.miniProtocolCode= 3,
                          Mx.miniProtocolRun = Mx.ResponderProtocolOnly (const server_long)
                        }
                      ]

    clientAsync <- async $ Mx.runMuxWithQueues activeTracer "client" clientApp client_w client_r sduLen (Just traceQueueVar)
    serverAsync <- async $ Mx.runMuxWithQueues activeTracer "server" serverApp server_w server_r sduLen Nothing

    -- First verify that all messages where received correctly
    r <- waitBoth clientAsync serverAsync
    case r of
         (Just _, _) -> return $ property False
         (_, Just _) -> return $ property False
         _           -> do
             -- First verify that all messages where received correctly
             res_short <- verify_short
             res_long <- verify_long

             -- Then look at the message trace to check for starvation.
             trace <- atomically $ flushTBQueue traceQueueVar []
             let es = map (\(e, _, _) -> e) trace
                 ls = dropWhile (\e -> e == head es) es
                 fair = verifyStarvation ls
             return $ res_short .&&. res_long .&&. fair
  where
   -- We can't make 100% sure that both servers start responding at the same
   -- time but once they are both up and running messages should alternate
   -- between Mx.ReqResp2 and Mx.ReqResp3
    verifyStarvation :: Eq a => [a] -> Property
    verifyStarvation [] = property True
    verifyStarvation ms =
      let ms' = dropWhileEnd (\e -> e == last ms)
                  (head ms : dropWhile (\e -> e == head ms) ms)
                ++ [last ms]
      in
        label ("length " ++ labelPr_ ((length ms' * 100) `div` length ms) ++ "%")
        $ label ("length " ++ label_ (length ms')) $ alternates ms'

      where
        alternates []     = True
        alternates (_:[]) = True
        alternates (a : b : as) = a /= b && alternates (b : as)

    label_ :: Int -> String
    label_ n = mconcat
      [ show $ n `div` 10 * 10
      , "-"
      , show $ n `div` 10 * 10 + 10 - 1
      ]

    labelPr_ :: Int -> String
    labelPr_ n | n >= 100  = "100"
               | otherwise = label_ n


    flushTBQueue q acc = do
        e <- isEmptyTBQueue q
        if e then return $ reverse acc
             else do
                 a <- readTBQueue q
                 flushTBQueue q (a : acc)

encodeInvalidMuxSDU :: InvalidSDU -> BL.ByteString
encodeInvalidMuxSDU sdu =
    let header = Bin.runPut enc in
    BL.append header $ BL.replicate (fromIntegral $ isLength sdu) (isPattern sdu)
  where
    enc = do
        Bin.putWord32be $ Mx.unRemoteClockModel $ isTimestamp sdu
        Bin.putWord16be $ isIdAndMode sdu
        Bin.putWord16be $ isLength sdu

-- | Verify ingress processing of valid and invalid SDUs.
--
prop_demux_sdu :: forall m.
                    ( MonadAsync m
                    , MonadCatch m
                    , MonadMask m
                    , MonadSay m
                    , MonadST m
                    , MonadSTM m
                    , MonadThrow (STM m)
                    , MonadTime m
                    , MonadTimer m
                    , Eq (Async m ())
                    )
                 => ArbitrarySDU
                 -> m Property
prop_demux_sdu a = do
    r <- run a
    return $ tabulate "SDU type" [stateLabel a] $
             tabulate "SDU Violation " [violationLabel a] r
  where
    run (ArbitraryValidSDU sdu state (Just Mx.MuxIngressQueueOverRun)) = do
        stopVar <- newEmptyTMVarM

        -- To trigger MuxIngressQueueOverRun we use a special test protocol
        -- with an ingress queue which is less than 0xffff so that it can be
        -- triggered by a single segment.
        let server_mps = Mx.MuxApplication
                           [ Mx.MuxMiniProtocol {
                               Mx.miniProtocolId  = ReqRespSmall,
                               Mx.miniProtocolCode= 2,
                               Mx.miniProtocolRun = Mx.ResponderProtocolOnly (const (serverRsp stopVar))
                             }
                           ]

        (client_w, said) <- plainServer server_mps
        setup state client_w

        writeSdu client_w $! unDummyPayload sdu

        atomically $! putTMVar stopVar $ unDummyPayload sdu

        res <- wait said
        case res of
            Just e  ->
                case fromException e of
                    Just me -> return $ Mx.errorType me === Mx.MuxIngressQueueOverRun
                    Nothing -> return $ property False
            Nothing -> return $ property False

    run (ArbitraryValidSDU sdu state err_m) = do
        stopVar <- newEmptyTMVarM

        let server_mps = Mx.MuxApplication
                           [ Mx.MuxMiniProtocol {
                               Mx.miniProtocolId  = ReqResp1,
                               Mx.miniProtocolCode= 2,
                               Mx.miniProtocolRun = Mx.ResponderProtocolOnly (const (serverRsp stopVar))
                             }
                           ]

        (client_w, said) <- plainServer server_mps

        setup state client_w

        atomically $! putTMVar stopVar $! unDummyPayload sdu
        writeSdu client_w $ unDummyPayload sdu

        res <- wait said
        case res of
            Just e  ->
                case fromException e of
                    Just me -> case err_m of
                                    Just err -> return $ Mx.errorType me === err
                                    Nothing  -> return $ property False
                    Nothing -> return $ property False
            Nothing -> return $ err_m === Nothing

    run (ArbitraryInvalidSDU badSdu state err) = do
        stopVar <- newEmptyTMVarM

        let server_mps = Mx.MuxApplication
                           [ Mx.MuxMiniProtocol {
                               Mx.miniProtocolId  = ReqResp1,
                               Mx.miniProtocolCode= 2,
                               Mx.miniProtocolRun = Mx.ResponderProtocolOnly (const (serverRsp stopVar))
                             }
                           ]

        (client_w, said) <- plainServer server_mps

        setup state client_w
        atomically $ writeTBQueue client_w $ BL.take (isRealLength badSdu) $ encodeInvalidMuxSDU badSdu
        atomically $ putTMVar stopVar $ BL.replicate (fromIntegral $ isLength badSdu) 0xa

        res <- wait said
        case res of
            Just e  ->
                case fromException e of
                    Just me -> return $ Mx.errorType me === err
                    Nothing -> return $ property False
            Nothing -> return $ property False

    plainServer server_mps = do
        server_w <- atomically $ newTBQueue 10
        server_r <- atomically $ newTBQueue 10

        said <- async $ Mx.runMuxWithQueues activeTracer "server" server_mps server_w server_r 1280 Nothing

        return (server_r, said)

    -- Server that expects to receive a specific ByteString.
    -- Doesn't send a reply.
    serverRsp stopVar chan =
        atomically (takeTMVar stopVar) >>= loop
      where
        loop e | e == BL.empty = return ()
        loop e = do
            msg_m <- Mx.recv chan
            case msg_m of
                 Just msg ->
                     case BL.stripPrefix msg e of
                          Just e' -> loop e'
                          Nothing -> error "recv corruption"
                 Nothing -> error "eof corruption"

    writeSdu _ payload | payload == BL.empty = return ()
    writeSdu queue payload = do
        let (!frag, !rest) = BL.splitAt 0xffff payload
            sdu' = Mx.MuxSDU (Mx.RemoteClockModel 0)
                             (Mx.fromProtocolEnum (Mx.AppProtocolId ReqRespSmall))
                              Mx.ModeInitiator
                             (fromIntegral $ BL.length frag) frag
            !pkt = Mx.encodeMuxSDU (sdu' :: Mx.MuxSDU)

        atomically $ writeTBQueue queue pkt
        writeSdu queue rest

    -- Unless we are in Larval or Connected we fake version negotiation before
    -- we run the test.
    {- Not yet! setup state q | state /= Mx.Larval && state /= Mx.Connected = do
        let msg = Mx.MsgInitReq [version0]
            blob = toLazyByteString $ Mx.encodeControlMsg msg
            pkt = Mx.MuxSDU (Mx.RemoteClockModel 0) Mx.Muxcontrol Mx.ModeInitiator
                            (fromIntegral $ BL.length blob) blob
        atomically $ writeTBQueue q $ Mx.encodeMuxSDU (pkt :: Mx.MuxSDU TestProtocols1)
        return () -}
    setup _ _ = return ()

    stateLabel (ArbitraryInvalidSDU _ state _) = "Invalid " ++ versionLabel state
    stateLabel (ArbitraryValidSDU _ state _)   = "Valid " ++ versionLabel state

    versionLabel Mx.Larval    = "before version negotiation"
    versionLabel Mx.Connected = "before version negotiation"
    versionLabel _            = "after version negotiation"

    violationLabel (ArbitraryValidSDU _ _ err_m) = sduViolation err_m
    violationLabel (ArbitraryInvalidSDU _ _ err) = sduViolation $ Just err

    sduViolation (Just Mx.MuxUnknownMiniProtocol) = "unknown miniprotocol"
    sduViolation (Just Mx.MuxDecodeError        ) = "decode error"
    sduViolation (Just Mx.MuxIngressQueueOverRun) = "ingress queue overrun"
    sduViolation (Just _                        ) = "unknown violation"
    sduViolation Nothing                          = "none"

prop_demux_sdu_sim :: ArbitrarySDU
                     -> Property
prop_demux_sdu_sim badSdu =
    let r_e =  runSimStrictShutdown $ prop_demux_sdu badSdu in
    case r_e of
         Left  _ -> property False
         Right r -> r

prop_demux_sdu_io :: ArbitrarySDU
                    -> Property
prop_demux_sdu_io badSdu = ioProperty $ prop_demux_sdu badSdu
