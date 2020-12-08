{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE UndecidableInstances       #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Ouroboros.Storage.LedgerDB.InMemory (
    tests
  ) where

import           Codec.CBOR.FlatTerm (FlatTerm, TermToken (..), fromFlatTerm,
                     toFlatTerm)
import           Codec.Serialise (decode, encode)
import           Data.Maybe (fromJust)
import           Data.Word
import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

import           Ouroboros.Network.Testing.Serialise (prop_serialise)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.Config
import qualified Ouroboros.Consensus.HardFork.History as HardFork
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Util

import           Ouroboros.Consensus.Storage.LedgerDB.InMemory

import           Test.Util.QuickCheck
import           Test.Util.TestBlock

tests :: TestTree
tests = testGroup "InMemory" [
      testGroup "Serialisation" [
          testCase     "encode ChainSummary"    test_encode_ChainSummary
        , testCase     "decode ChainSummary"    test_decode_ChainSummary
        , testProperty "serialise ChainSummary" prop_serialise_ChainSummary
        ]
    , testGroup "Genesis" [
          testProperty "length"  prop_genesisLength
        , testProperty "current" prop_genesisCurrent
        ]
    , testGroup "Push" [
          testProperty "incrementsLength"       prop_pushIncrementsLength
        , testProperty "lengthMatchesNumBlocks" prop_lengthMatchesNumBlocks
        , testProperty "expectedLedger"         prop_pushExpectedLedger
        , testProperty "pastLedger"             prop_pastLedger
        ]
    , testGroup "Rollback" [
          testProperty "maxRollbackGenesisZero" prop_maxRollbackGenesisZero
        , testProperty "ledgerDbMaxRollback"    prop_snapshotsMaxRollback
        , testProperty "switchSameChain"        prop_switchSameChain
        , testProperty "switchExpectedLedger"   prop_switchExpectedLedger
        , testProperty "pastAfterSwitch"        prop_pastAfterSwitch
        ]
    , testGroup "Lookup" [
          testProperty "ledgerDbPast"           prop_pastVsSpec
        ]
    ]

{-------------------------------------------------------------------------------
  Serialisation
-------------------------------------------------------------------------------}

example_ChainSummary :: ChainSummary Int TestBlock
example_ChainSummary =
    ChainSummary
      (BlockPoint (SlotNo 3) (testHashFromList [0, 0]))
      10
      100

golden_ChainSummary :: FlatTerm
golden_ChainSummary =
    [ TkListLen 3
      -- tip: WithOrigin (RealPoint TestBlock)
    , TkListLen 1
    , TkListLen 2
    , TkInt 3
    , TkListBegin, TkInt 0, TkInt 0, TkBreak
      -- chain length: Word64
    , TkInt 10
      -- ledger: Int for simplicity
    , TkInt 100
    ]

test_encode_ChainSummary :: Assertion
test_encode_ChainSummary =
    toFlatTerm (enc example_ChainSummary) @?= golden_ChainSummary
  where
    enc = encodeChainSummary encode encode

test_decode_ChainSummary :: Assertion
test_decode_ChainSummary =
    fromFlatTerm dec golden_ChainSummary @?= Right example_ChainSummary
  where
    dec = decodeChainSummary decode decode

prop_serialise_ChainSummary :: Trivial (ChainSummary Int TestBlock) -> Property
prop_serialise_ChainSummary (Trivial summary) = prop_serialise summary

{-------------------------------------------------------------------------------
  Genesis
-------------------------------------------------------------------------------}

prop_genesisCurrent :: LedgerDbParams -> Property
prop_genesisCurrent params =
    ledgerDbCurrent genSnaps === testInitLedger
  where
    genSnaps = ledgerDbFromGenesis params testInitLedger

prop_genesisLength :: LedgerDbParams -> Property
prop_genesisLength params =
   ledgerDbChainLength genSnaps === 0
  where
    genSnaps = ledgerDbFromGenesis params testInitLedger

{-------------------------------------------------------------------------------
  Constructing snapshots
-------------------------------------------------------------------------------}

prop_pushIncrementsLength :: ChainSetup -> Property
prop_pushIncrementsLength setup@ChainSetup{..} =
    classify (chainSetupSaturated setup) "saturated" $
          ledgerDbChainLength (ledgerDbPush' (csBlockConfig setup) nextBlock csPushed)
      === ledgerDbChainLength csPushed + 1
  where
    nextBlock :: TestBlock
    nextBlock = case lastMaybe csChain of
                  Nothing -> firstBlock 0
                  Just b  -> successorBlock b

prop_lengthMatchesNumBlocks :: ChainSetup -> Property
prop_lengthMatchesNumBlocks setup@ChainSetup{..} =
    classify (chainSetupSaturated setup) "saturated" $
          ledgerDbChainLength csPushed
      === csNumBlocks

prop_pushExpectedLedger :: ChainSetup -> Property
prop_pushExpectedLedger setup@ChainSetup{..} =
    classify (chainSetupSaturated setup) "saturated" $
      conjoin [
          l === refoldLedger (csBlockConfig setup) (expectedChain o) testInitLedger
        | (o, l) <- ledgerDbSnapshots csPushed
        ]
  where
    expectedChain :: Word64 -> [TestBlock]
    expectedChain o = take (fromIntegral (csNumBlocks - o)) csChain

prop_pastLedger :: ChainSetup -> Property
prop_pastLedger setup@ChainSetup{..} =
    classify (chainSetupSaturated setup) "saturated"    $
    classify withinReach                 "within reach" $
          ledgerDbPast tip csPushed
      === if withinReach
            then Just (ledgerDbCurrent afterPrefix)
            else Nothing
  where
    prefix :: [TestBlock]
    prefix = take (fromIntegral csPrefixLen) csChain

    tip :: Point TestBlock
    tip = maybe GenesisPoint blockPoint (lastMaybe prefix)

    afterPrefix :: LedgerDB (LedgerState TestBlock) TestBlock
    afterPrefix = ledgerDbPushMany' (csBlockConfig setup) prefix csGenSnaps

    -- See 'prop_snapshotsMaxRollback'
    withinReach :: Bool
    withinReach = (csNumBlocks - csPrefixLen) <= ledgerDbMaxRollback csPushed

{-------------------------------------------------------------------------------
  Rollback
-------------------------------------------------------------------------------}

prop_maxRollbackGenesisZero :: LedgerDbParams -> Property
prop_maxRollbackGenesisZero params =
        ledgerDbMaxRollback (ledgerDbFromGenesis params testInitLedger)
    === 0

prop_snapshotsMaxRollback :: ChainSetup -> Property
prop_snapshotsMaxRollback setup@ChainSetup{..} =
    classify (chainSetupSaturated setup) "saturated" $
      conjoin [
          if chainSetupSaturated setup
            then (ledgerDbMaxRollback csPushed) `ge` k
            else (ledgerDbMaxRollback csPushed) `ge` (min k csNumBlocks)
        , (ledgerDbMaxRollback csPushed) `le` k
        ]
  where
    SecurityParam k = ledgerDbSecurityParam csParams

prop_switchSameChain :: SwitchSetup -> Property
prop_switchSameChain setup@SwitchSetup{..} =
    classify (switchSetupSaturated setup) "saturated" $
          ledgerDbSwitch' (csBlockConfig ssChainSetup) ssNumRollback blockInfo csPushed
      === Just csPushed
  where
    ChainSetup{csPushed} = ssChainSetup
    blockInfo            = ssRemoved

prop_switchExpectedLedger :: SwitchSetup -> Property
prop_switchExpectedLedger setup@SwitchSetup{..} =
    classify (switchSetupSaturated setup) "saturated" $
      conjoin [
          l === refoldLedger (csBlockConfig ssChainSetup) (expectedChain o) testInitLedger
        | (o, l) <- ledgerDbSnapshots ssSwitched
        ]
  where
    expectedChain :: Word64 -> [TestBlock]
    expectedChain o = take (fromIntegral (ssNumBlocks - o)) ssChain

-- | Check 'prop_pastLedger' still holds after switching to a fork
prop_pastAfterSwitch :: SwitchSetup -> Property
prop_pastAfterSwitch setup@SwitchSetup{..} =
    classify (switchSetupSaturated setup) "saturated"    $
    classify withinReach                  "within reach" $
          ledgerDbPast tip ssSwitched
      === if withinReach
            then Just (ledgerDbCurrent afterPrefix)
            else Nothing
  where
    prefix :: [TestBlock]
    prefix = take (fromIntegral ssPrefixLen) ssChain

    tip :: Point TestBlock
    tip = maybe GenesisPoint blockPoint (lastMaybe prefix)

    afterPrefix :: LedgerDB (LedgerState TestBlock) TestBlock
    afterPrefix = ledgerDbPushMany' (csBlockConfig ssChainSetup) prefix (csGenSnaps ssChainSetup)

    -- See 'prop_snapshotsMaxRollback'
    withinReach :: Bool
    withinReach = (ssNumBlocks - ssPrefixLen) <= ledgerDbMaxRollback ssSwitched

{-------------------------------------------------------------------------------
  Lookup
-------------------------------------------------------------------------------}

-- | Check the implementation of 'ledgerDbPast' against the 'ledgerDbPastSpec'
-- reference implementation.
prop_pastVsSpec :: SwitchSetup -> Property
prop_pastVsSpec SwitchSetup{..} = conjoin [
          ledgerDbPast     r ssSwitched
      === ledgerDbPastSpec r ssSwitched
      -- Include all blocks in the LedgerDB, but also blocks /not/ in the
      -- LedgerDB
    | r <- GenesisPoint : map blockPoint (ssChain <> ssRemoved)
    ]

{-------------------------------------------------------------------------------
  Test setup
-------------------------------------------------------------------------------}

data ChainSetup = ChainSetup {
      -- | Ledger DB parameters
      csParams    :: LedgerDbParams

      -- | Number of blocks applied
    , csNumBlocks :: Word64

      -- | Some prefix of the chain
      --
      -- Although we choose this to be less than or equal to 'csNumBlocks',
      -- we don't guarantee this during shrinking. If 'csPrefixLen' is larger
      -- than 'csNumBlocks', the prefix should simply be considered to be the
      -- entire chain.
    , csPrefixLen :: Word64

      -- | Derived: genesis snapshots
    , csGenSnaps  :: LedgerDB (LedgerState TestBlock) TestBlock

      -- | Derived: the actual blocks that got applied (old to new)
    , csChain     :: [TestBlock]

      -- | Derived: the snapshots after all blocks were applied
    , csPushed    :: LedgerDB (LedgerState TestBlock) TestBlock
    }
  deriving (Show)

csBlockConfig :: ChainSetup -> LedgerConfig TestBlock
csBlockConfig = csBlockConfig' . csParams

csBlockConfig' :: LedgerDbParams -> LedgerConfig TestBlock
csBlockConfig' dbParams = HardFork.defaultEraParams k slotLength
  where
    k          = ledgerDbSecurityParam dbParams
    slotLength = slotLengthFromSec 20

chainSetupSaturated :: ChainSetup -> Bool
chainSetupSaturated = ledgerDbIsSaturated . csPushed

data SwitchSetup = SwitchSetup {
      -- | Chain setup
      ssChainSetup  :: ChainSetup

      -- | Number of blocks to roll back
    , ssNumRollback :: Word64

      -- | Number of new blocks (to be applied after the rollback)
    , ssNumNew      :: Word64

      -- | Prefix of the new chain
      --
      -- See also 'csPrefixLen'
    , ssPrefixLen   :: Word64

      -- | Derived: number of blocks in the new chain
    , ssNumBlocks   :: Word64

      -- | Derived: the blocks that were removed
    , ssRemoved     :: [TestBlock]

      -- | Derived: the new blocks themselves
    , ssNewBlocks   :: [TestBlock]

      -- | Derived: the full chain after switching to this fork
    , ssChain       :: [TestBlock]

      -- | Derived; the snapshots after the switch was performed
    , ssSwitched    :: LedgerDB (LedgerState TestBlock) TestBlock
    }
  deriving (Show)

switchSetupSaturated :: SwitchSetup -> Bool
switchSetupSaturated = chainSetupSaturated . ssChainSetup

mkTestSetup :: LedgerDbParams -> Word64 -> Word64 -> ChainSetup
mkTestSetup csParams csNumBlocks csPrefixLen =
    ChainSetup {..}
  where
    csGenSnaps = ledgerDbFromGenesis csParams testInitLedger
    csChain    = take (fromIntegral csNumBlocks) $
                   iterate successorBlock (firstBlock 0)
    csPushed   = ledgerDbPushMany' (csBlockConfig' csParams) csChain csGenSnaps

mkRollbackSetup :: ChainSetup -> Word64 -> Word64 -> Word64 -> SwitchSetup
mkRollbackSetup ssChainSetup ssNumRollback ssNumNew ssPrefixLen =
    SwitchSetup {..}
  where
    ChainSetup{..} = ssChainSetup

    ssNumBlocks = csNumBlocks - ssNumRollback + ssNumNew
    ssRemoved   = takeLast ssNumRollback csChain
    ssNewBlocks = let afterRollback      = dropLast ssNumRollback csChain
                      firstAfterRollback =
                        case lastMaybe afterRollback of
                          Nothing -> firstBlock 1
                          Just b  -> modifyFork (+ 1) $ successorBlock b
                  in take (fromIntegral ssNumNew) $
                        iterate successorBlock firstAfterRollback
    ssChain     = concat [
                         take (fromIntegral (csNumBlocks - ssNumRollback)) csChain
                       , ssNewBlocks
                       ]
    ssSwitched  = fromJust $ ledgerDbSwitch' (csBlockConfig ssChainSetup) ssNumRollback ssNewBlocks csPushed

instance Arbitrary ChainSetup where
  arbitrary = do
      params <- arbitrary
      let k = maxRollbacks (ledgerDbSecurityParam params)
      numBlocks <- choose (0, k * 2)
      prefixLen <- choose (0, numBlocks)
      return $ mkTestSetup params numBlocks prefixLen

  shrink ChainSetup{..} = concat [
        -- Shrink the policy
        [ mkTestSetup csParams' csNumBlocks csPrefixLen
        | csParams' <- shrink csParams
        ]

        -- Reduce number of blocks
      , [ mkTestSetup csParams csNumBlocks' csPrefixLen
        | csNumBlocks' <- shrink csNumBlocks
        ]
      ]

instance Arbitrary SwitchSetup where
  arbitrary = do
      chainSetup  <- arbitrary
      numRollback <- choose (0, ledgerDbMaxRollback (csPushed chainSetup))
      numNew      <- choose (numRollback, 2 * numRollback)
      prefixLen   <- choose (0, csNumBlocks chainSetup - numRollback + numNew)
      return $ mkRollbackSetup chainSetup numRollback numNew prefixLen

  shrink SwitchSetup{..} = concat [
        -- If we shrink the chain setup, we might restrict max rollback
        [ mkRollbackSetup ssChainSetup' ssNumRollback ssNumNew ssPrefixLen
        | ssChainSetup' <- shrink ssChainSetup
        , ssNumRollback <= ledgerDbMaxRollback (csPushed ssChainSetup')
        ]
        -- Number of new blocks must be at least the rollback
      , [ mkRollbackSetup ssChainSetup ssNumRollback ssNumNew' ssPrefixLen
        | ssNumNew' <- shrink ssNumNew
        , ssNumNew' >= ssNumRollback
        ]
        -- But rolling back less is always possible
      , [ mkRollbackSetup ssChainSetup ssNumRollback' ssNumNew ssPrefixLen
        | ssNumRollback' <- shrink ssNumRollback
        ]
      ]

{-------------------------------------------------------------------------------
  Orphan Arbitrary instances
-------------------------------------------------------------------------------}

instance Arbitrary LedgerDbParams where
  arbitrary = do
      k <- choose (0, 6)
      return $ LedgerDbParams {
            ledgerDbSecurityParam = SecurityParam k
          }
  shrink params@LedgerDbParams{..} =
    [ params {ledgerDbSecurityParam = SecurityParam k'}
    | k' <- shrink (maxRollbacks ledgerDbSecurityParam)
    ]

{-------------------------------------------------------------------------------
  Serialisation roundtrip
-------------------------------------------------------------------------------}

-- | Marker that we're not recording anything interesting, but merely testing
-- roundtrip properties
newtype Trivial a = Trivial { trivial :: a }
  deriving (Show)

instance Arbitrary (Trivial (ChainSummary Int TestBlock)) where
  arbitrary = fmap Trivial $
                ChainSummary <$> (trivial <$> arbitrary)
                             <*> arbitrary
                             <*> arbitrary

instance Arbitrary (Trivial (Point TestBlock)) where
  arbitrary = fmap Trivial $ do
      gen <- arbitrary
      if gen then
        return GenesisPoint
      else
        BlockPoint
          <$> (SlotNo <$> arbitrary)
          <*> (testHashFromList . getNonEmpty <$> arbitrary)
