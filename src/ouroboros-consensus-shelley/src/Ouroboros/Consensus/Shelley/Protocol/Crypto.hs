{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}
module Ouroboros.Consensus.Shelley.Protocol.Crypto (
    module Cardano.Ledger.Shelley.Crypto
  , TPraosCrypto
  , TPraosStandardCrypto
  ) where

import           Data.Typeable (Typeable)
import           Numeric.Natural

import           Cardano.Binary (ToCBOR)
import qualified Cardano.Crypto.DSIGN.Class as DSIGN
import           Cardano.Crypto.DSIGN.Ed448 (Ed448DSIGN)
import           Cardano.Crypto.Hash.SHA256 (SHA256)
import           Cardano.Crypto.KES.Class
import qualified Cardano.Crypto.KES.Class as KES
import           Cardano.Crypto.KES.Simple
import qualified Cardano.Crypto.VRF.Class as VRF
import           Cardano.Crypto.VRF.Simple (SimpleVRF)

import           Ouroboros.Consensus.Util.Condense (Condense)

import           Cardano.Ledger.Shelley.Crypto (Crypto (..))
import           Shelley.Spec.Ledger.BaseTypes (Seed)
import           Shelley.Spec.Ledger.BlockChain (BHBody)
import           Shelley.Spec.Ledger.Keys (VKeyES (..))
import           Shelley.Spec.Ledger.OCert (KESPeriod)
import           Shelley.Spec.Ledger.TxData (TxBody)

class ( Crypto c
      , Typeable (VRF c)
      , Condense (SigKES (KES c))
      , DSIGN.Signable
          (DSIGN c)
          (VKeyES c, Natural, KESPeriod)
      , DSIGN.Signable
          (DSIGN c)
          (TxBody c)
      , KES.Signable
          (KES c)
          (BHBody c)
      , VRF.Signable (VRF c) Seed
      , ToCBOR (DSIGN.VerKeyDSIGN (DSIGN c))
      ) => TPraosCrypto c

data TPraosStandardCrypto

instance Crypto TPraosStandardCrypto where
  type DSIGN TPraosStandardCrypto = Ed448DSIGN
  type KES   TPraosStandardCrypto = SimpleKES Ed448DSIGN
  type VRF   TPraosStandardCrypto = SimpleVRF
  type HASH  TPraosStandardCrypto = SHA256

instance TPraosCrypto TPraosStandardCrypto
