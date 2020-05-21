-- | The hard fork combinator
--
-- Intended for unqualified import
module Ouroboros.Consensus.HardFork.Combinator (
    module X
  ) where

-- Defines 'SingleEraBlock' and 'CanHardFork'
import           Ouroboros.Consensus.HardFork.Combinator.Abstract as X

-- Defines 'HardForkProtocol', 'HardForkBlock', and 'LedgerState',
-- as well as various config types.
import           Ouroboros.Consensus.HardFork.Combinator.Basics as X

-- Defines 'HasPartialConsensusConfig' and 'HasPartialLedgerConfig'
import           Ouroboros.Consensus.HardFork.Combinator.PartialConfig as X

-- Instances for 'BlockHasCodecConfig', 'GetHeader', 'HasHeader',
-- 'BasicEnvelopeValidation'
import           Ouroboros.Consensus.HardFork.Combinator.Block as X

-- Instances for 'IsLedger', 'ApplyBlock', 'UpdateLedger',
-- 'LedgerSupportsProtocol', 'HasHardForkHistory', 'ValidateEnvelope'
import           Ouroboros.Consensus.HardFork.Combinator.Ledger as X

-- Instances for 'ApplyTx', 'HasTxId', 'HasTxs'
import           Ouroboros.Consensus.HardFork.Combinator.Mempool as X

-- Instance for 'ConsensusProtocol'
import           Ouroboros.Consensus.HardFork.Combinator.Protocol as X

-- Instances for 'ShowQuery' and 'QueryLedger'
import           Ouroboros.Consensus.HardFork.Combinator.Ledger.Query as X

-- Instance for 'ChainSelection'
import           Ouroboros.Consensus.HardFork.Combinator.Protocol.ChainSel as X

-- Defines 'SingleEraInfo' and 'LedgerEraInfo'
import           Ouroboros.Consensus.HardFork.Combinator.SingleEra.Info as X

-- Defines the various translation types required for concrete HFC instances
import           Ouroboros.Consensus.HardFork.Combinator.Translation as X

-- Instance for 'CanForge'
import           Ouroboros.Consensus.HardFork.Combinator.Forge as X ()

-- Omitted from this export:
--
-- * "Ouroboros.Consensus.HardFork.Combinator.State"
--   This defines 'HardForkState', a wrapper around a 'Telescope'. We use this
--   to define 'HardForkLedgerState', 'HardForkLedgerView' as well as
--   'HardForkConsensusState', but the type itself should mostly be internal
--   to the hard fork combinator.
--
-- * "module Ouroboros.Consensus.HardFork.Combinator.State.Infra"
--   This module is only separate from @.State@ to avoid some cyclic module
--   dependencies. Most modules internally to the HFC should import from
--   @.State@ directly, and outside of the HFC not even @.State@ should be
--   needed (see above).
--
-- * "Ouroboros.Consensus.HardFork.Combinator.Protocol.LedgerView"
--   This is internal to "Ouroboros.Consensus.HardFork.Combinator.Protocol"
--
-- * "Ouroboros.Consensus.HardFork.Combinator.Protocol.State"
--   This is internal to "Ouroboros.Consensus.HardFork.Combinator.Protocol"
--
-- * "Ouroboros.Consensus.HardFork.Combinator.Degenerate"
--   This defines 'DegenFork', which is useful as a test case that the hard
--   fork combinator when applied to a single block results in a system
--   that is equivalent to just using that single block directly.
--
-- * "Ouroboros.Consensus.HardFork.Combinator.Unary"
--   Mostly used in combination with 'DegenFork'.
--
-- * Most of @Ouroboros.Consensus.HardFork.Combinator.SingleEra.*@
--   These types are primarily used internally to define the HFC types.
--   In a few cases some of the HFC types /are/ types from the SingleEra
--   module hierarchy directly; in such cases, we should export them from
--   this module.
--   TODO: Currently we only do this for @SingleEra.Info@, but we might also
--   need to do it for other types.
--
-- * Ouroboros.Consensus.HardFork.Combinator.Util.*
--   Utility functions and types
