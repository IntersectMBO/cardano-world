{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TypeFamilies      #-}

module Ouroboros.Consensus.Node.NetworkProtocolVersion
  ( HasNetworkProtocolVersion(..)
  , SupportedNetworkProtocolVersion(..)
    -- * Re-exports
  , NodeToNodeVersion(..)
  , NodeToClientVersion(..)
  ) where

import           Data.Kind (Type)
import           Data.Map.Strict (Map)
import           Data.Proxy

import           Ouroboros.Network.NodeToClient
import           Ouroboros.Network.NodeToNode

{-------------------------------------------------------------------------------
  Protocol versioning
-------------------------------------------------------------------------------}

-- | Protocol versioning
class ( Show (BlockNodeToNodeVersion   blk)
      , Show (BlockNodeToClientVersion blk)
      , Eq   (BlockNodeToNodeVersion   blk)
      , Eq   (BlockNodeToClientVersion blk)
      ) => HasNetworkProtocolVersion blk where
  type BlockNodeToNodeVersion   blk :: Type
  type BlockNodeToClientVersion blk :: Type

  -- Defaults

  type BlockNodeToNodeVersion   blk = ()
  type BlockNodeToClientVersion blk = ()

class HasNetworkProtocolVersion blk => SupportedNetworkProtocolVersion blk where
  -- | Enumerate all supported node-to-node versions
  supportedNodeToNodeVersions
    :: Proxy blk -> Map NodeToNodeVersion (BlockNodeToNodeVersion blk)

  -- | Enumerate all supported node-to-client versions
  supportedNodeToClientVersions
    :: Proxy blk -> Map NodeToClientVersion (BlockNodeToClientVersion blk)
