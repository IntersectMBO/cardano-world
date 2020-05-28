{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}

module Ouroboros.Consensus.Protocol.LeaderSchedule (
    LeaderSchedule (..)
  , WithLeaderSchedule
  , ConsensusConfig (..)
  ) where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Proxy
import           GHC.Generics (Generic)

import           Cardano.Prelude (NoUnexpectedThunks)

import           Ouroboros.Network.Block (SlotNo (..))

import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.NodeId (CoreNodeId (..), fromCoreNodeId)
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Util.Condense (Condense (..))

{-------------------------------------------------------------------------------
  Leader schedule

  The leader schedule allows us to define, in tests, precisely when each node
  is meant to lead. Unlike in, say, Praos, where this is determined by a single
  random seed, this gives us the ability to construct test cases in an
  inspectable and shrinkable manner.
-------------------------------------------------------------------------------}

newtype LeaderSchedule = LeaderSchedule {
        getLeaderSchedule :: Map SlotNo [CoreNodeId]
      }
    deriving stock    (Show, Eq, Ord, Generic)
    deriving anyclass (NoUnexpectedThunks)

instance Semigroup LeaderSchedule where
    LeaderSchedule l <> LeaderSchedule r =
        LeaderSchedule $
        Map.unionWith comb l r
      where
        comb ls rs = ls ++ filter (`notElem` ls) rs

instance Condense LeaderSchedule where
    condense (LeaderSchedule m) = condense
                                $ map (\(s, ls) -> (s, map fromCoreNodeId ls))
                                $ Map.toList m

{-------------------------------------------------------------------------------
  ConsensusProtocol instance
-------------------------------------------------------------------------------}

-- | Extension of protocol @p@ by a static leader schedule.
data WithLeaderSchedule p

-- | Chain selection is unchanged
instance ChainSelection p => ChainSelection (WithLeaderSchedule p) where
  type ChainSelConfig (WithLeaderSchedule p) = ChainSelConfig p
  type SelectView     (WithLeaderSchedule p) = SelectView     p

  preferCandidate   _ = preferCandidate   (Proxy @p)
  compareCandidates _ = compareCandidates (Proxy @p)

data instance ConsensusConfig (WithLeaderSchedule p) = WLSConfig
  { wlsConfigSchedule :: !LeaderSchedule
  , wlsConfigP        :: !(ConsensusConfig p)
  , wlsConfigNodeId   :: !CoreNodeId
  }
  deriving (Generic)

instance ConsensusProtocol p => ConsensusProtocol (WithLeaderSchedule p) where
  type ConsensusState (WithLeaderSchedule p) = ()
  type LedgerView     (WithLeaderSchedule p) = ()
  type ValidationErr  (WithLeaderSchedule p) = ()
  type IsLeader       (WithLeaderSchedule p) = ()
  type ValidateView   (WithLeaderSchedule p) = ()
  type CanBeLeader    (WithLeaderSchedule p) = ()

  protocolSecurityParam = protocolSecurityParam . wlsConfigP
  chainSelConfig        = chainSelConfig        . wlsConfigP

  checkIsLeader WLSConfig{..} () (Ticked slot _) _ = return $
    case Map.lookup slot $ getLeaderSchedule wlsConfigSchedule of
        Nothing                           -> Nothing
        Just nids
            | wlsConfigNodeId `elem` nids -> Just ()
            | otherwise                   -> Nothing

  updateConsensusState _ _ _ _ = return ()
  rewindConsensusState _ _ _ _ = Just ()

instance ConsensusProtocol p
      => NoUnexpectedThunks (ConsensusConfig (WithLeaderSchedule p))
