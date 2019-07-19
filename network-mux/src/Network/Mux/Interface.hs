{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

-- TODO: this module should be exposed as 'Ouorboros.Network'
module Network.Mux.Interface
  (
  -- * High level interface for the multiplex layer
  -- $interface
    AppType (..)
  , HasInitiator
  , HasResponder
  , MuxApplication (..)
  , initiatorApplication
  , responderApplication
  , ProtocolEnum (..)
  , MiniProtocolLimits (..)
  , TraceLabelPeer (..)
  ) where

import           Data.Void (Void)

import           Network.TypedProtocol.Channel

import           Network.Mux.Types ( MiniProtocolLimits (..)
                                   , ProtocolEnum (..)
                                   )


-- | A peer label for use in 'Tracer's. This annotates tracer output as being
-- associated with a given peer identifier.
--
data TraceLabelPeer peerid a = TraceLabelPeer peerid a
  deriving (Eq, Functor, Show)


-- $interface
--
-- To run a node you will also need a bearer and a way to run a server, see
--
-- * @'Ouroboros.Network.Socket'@ module provides a socket based bearer and
--   a server that accepts connections and allows to connect to remote peers.
--
-- * @'Ouroboros.Network.Pipe'@ module provides a pipe based bearer with
--   a function that runs the mux layer on it.
--

data AppType where
    InitiatorApp             :: AppType
    ResponderApp             :: AppType
    InitiatorAndResponderApp :: AppType

type family HasInitiator (appType :: AppType) :: Bool where
    HasInitiator InitiatorApp             = True
    HasInitiator ResponderApp             = False
    HasInitiator InitiatorAndResponderApp = True

type family HasResponder (appType :: AppType) :: Bool where
    HasResponder InitiatorApp             = False
    HasResponder ResponderApp             = True
    HasResponder InitiatorAndResponderApp = True

-- |
-- Application run by mux layer.
--
-- * enumeration of client application, e.g. a wallet application communicating
--   with a node using ChainSync and TxSubmission protocols; this only requires
--   to run client side of each protocol.
--
-- * enumeration of server applications: this application type is mostly useful
--   tests.
--
-- * enumeration of both client and server applications, e.g. a full node
--   serving downstream peers using server side of each protocol and getting
--   updates from upstream peers using client side of each of the protocols.
--
data MuxApplication (appType :: AppType) peerid ptcl m bytes a b where
  MuxInitiatorApplication
    -- Initiator application; most simple application will be @'runPeer'@ or
    -- @'runPipelinedPeer'@ supplied with a codec and a @'Peer'@ for each
    -- @ptcl@.  But it allows to handle resources if just application of
    -- @'runPeer'@ is not enough.  It will be run as @'ModeInitiator'@.
    :: (peerid -> ptcl -> Channel m bytes ->  m a)
    -> MuxApplication InitiatorApp peerid ptcl m bytes a Void

  MuxResponderApplication
    -- Responder application; similarly to the @'MuxInitiatorApplication'@ but it
    -- will be run using @'ModeResponder'@.
    :: (peerid -> ptcl -> Channel m bytes ->  m a)
    -> MuxApplication ResponderApp peerid ptcl m bytes Void a

  MuxInitiatorAndResponderApplication
    -- Initiator and server applications.
    :: (peerid -> ptcl -> Channel m bytes ->  m a)
    -> (peerid -> ptcl -> Channel m bytes ->  m b)
    -> MuxApplication InitiatorAndResponderApp peerid ptcl m bytes a b

-- |
-- Accessor for the client side of a @'MuxApplication'@.
--
initiatorApplication
  :: HasInitiator appType ~ True
  => MuxApplication appType peerid ptcl m bytes a b
  -> (peerid -> ptcl -> Channel m bytes ->  m a)
initiatorApplication (MuxInitiatorApplication app) = \peerid ptcl channel -> app peerid ptcl channel
initiatorApplication (MuxInitiatorAndResponderApplication app _) = \peerid ptcl channel -> app peerid ptcl channel

-- |
-- Accessor for the client side of a @'MuxApplication'@.
--
responderApplication
  :: HasResponder appType ~ True
  => MuxApplication appType peerid ptcl m bytes a b
  -> (peerid -> ptcl -> Channel m bytes ->  m b)
responderApplication (MuxResponderApplication app) = app
responderApplication (MuxInitiatorAndResponderApplication _ app) = app
