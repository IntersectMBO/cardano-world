{- |
Copyright: (c) 2021 Input Output (Hong Kong) Ltd.
Maintainer: Denis Shevchenko <denis.shevchenko@iohk.io>

See README for more info
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module System.Metrics.Internal.Protocol.Codec where

import           Text.Read (readMaybe)

import           Network.TypedProtocol.Core
import           Ouroboros.Network.Codec

import           System.Metrics.Internal.Protocol.Type

codecEKGForward ::
    forall req resp m
  . (Monad m, Show req, Show resp, Read req, Read resp)
  => Codec (EKGForward req resp) CodecFailure m String
codecEKGForward =
  Codec{encode, decode}
 where
  encode :: forall req' resp'
                   (pr  :: PeerRole)
                   (st  :: EKGForward req' resp')
                   (st' :: EKGForward req' resp')
         . (Show (Message (EKGForward req' resp') st st'))
         => PeerHasAgency pr st
         -> Message (EKGForward req' resp') st st'
         -> String
  encode (ClientAgency TokIdle) msg = show msg ++ "\n"
  encode (ServerAgency TokBusy) msg = show msg ++ "\n"

  decode :: forall req' resp' m'
                   (pr :: PeerRole)
                   (st :: EKGForward req' resp')
         .  (Monad m', Read req', Read resp')
         => PeerHasAgency pr st
         -> m' (DecodeStep String CodecFailure m' (SomeMessage st))
  decode stok =
    decodeTerminatedFrame '\n' $ \str trailing ->
      case (stok, break (==' ') str) of
        -- Please note that, from __interaction__ point of view:
        -- 1. ClientAgency corresponds to acceptor's agency.
        -- 3. ServerAgency corresponds to forwarder's agency.
        (ClientAgency TokIdle, ("MsgReq", str'))
           | Just resp <- readMaybe str'          -> DecodeDone (SomeMessage (MsgReq resp)) trailing
        (ServerAgency TokBusy, ("MsgResp", str'))
           | Just resp <- readMaybe str'          -> DecodeDone (SomeMessage (MsgResp resp)) trailing
        (ClientAgency TokIdle, ("MsgDone", ""))   -> DecodeDone (SomeMessage MsgDone) trailing
        -- Unexpected messages!
        (ServerAgency _      , _     ) -> DecodeFail failure
          where failure = CodecFailure ("Unexpected message from forwarder: " ++ str)
        (ClientAgency _      , _     ) -> DecodeFail failure
          where failure = CodecFailure ("Unexpected message from acceptor: " ++ str)

decodeTerminatedFrame
  :: forall m a. Monad m
  => Char
  -> (String -> Maybe String -> DecodeStep String CodecFailure m a)
  -> m (DecodeStep String CodecFailure m a)
decodeTerminatedFrame terminator k = go []
  where
    go :: [String] -> m (DecodeStep String CodecFailure m a)
    go chunks =
      return $ DecodePartial $ \mchunk ->
        case mchunk of
          Nothing    -> return $ DecodeFail CodecFailureOutOfInput
          Just chunk ->
            case break (==terminator) chunk of
              (c, _:c') -> return $ k (concat (reverse (c:chunks)))
                                      (if null c' then Nothing else Just c)
              _         -> go (chunk : chunks)
