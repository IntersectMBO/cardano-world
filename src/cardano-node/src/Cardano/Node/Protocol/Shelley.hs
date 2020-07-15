{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Node.Protocol.Shelley
  (
    -- * Protocol exposing the specific type
    -- | Use this when you need the specific instance
    mkConsensusProtocolShelley

    -- * Protocols hiding the specific type
    -- | Use this when you want to handle protocols generically
  , mkSomeConsensusProtocolShelley

    -- * Errors
  , ShelleyProtocolInstantiationError(..)
  , renderShelleyProtocolInstantiationError

    -- * Reusable parts
  , readGenesis
  , readLeaderCredentials
  ) where

import           Cardano.Prelude
import           Prelude (String, id)

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Aeson as Aeson

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra
                   (firstExceptT, newExceptT, handleIOExceptT, hoistEither)

import qualified Cardano.Crypto.Hash.Class as Crypto

import qualified Ouroboros.Consensus.Cardano as Consensus

import           Ouroboros.Consensus.Shelley.Ledger
import           Ouroboros.Consensus.Shelley.Protocol
                   (TPraosStandardCrypto, TPraosIsCoreNode(..))
import           Ouroboros.Consensus.Shelley.Node
                   (TPraosLeaderCredentials(..), ShelleyGenesis, Nonce (..))

import           Shelley.Spec.Ledger.PParams (ProtVer(..))
import           Shelley.Spec.Ledger.Keys (coerceKeyRole)

import           Cardano.Api.Typed hiding (FileError)
import qualified Cardano.Api.Typed as Api (FileError)

import           Cardano.Node.Types (NodeShelleyProtocolConfiguration(..))
import           Cardano.Config.Types
                   (ProtocolFilepaths(..), GenesisFile (..))
import           Cardano.TracingOrphanInstances.Shelley ()

import           Cardano.Node.Protocol.Types


------------------------------------------------------------------------------
-- Shelley protocol
--

-- | Make 'SomeConsensusProtocol' using the Shelley instance.
--
-- This lets us handle multiple protocols in a generic way.
--
-- This also serves a purpose as a sanity check that we have all the necessary
-- type class instances available.
--
mkSomeConsensusProtocolShelley
  :: NodeShelleyProtocolConfiguration
  -> Maybe ProtocolFilepaths
  -> ExceptT ShelleyProtocolInstantiationError IO SomeConsensusProtocol
mkSomeConsensusProtocolShelley nc files =

    -- Applying the SomeConsensusProtocol here is a check that
    -- the type of mkConsensusProtocolShelley fits all the class
    -- constraints we need to run the protocol.
    SomeConsensusProtocol <$> mkConsensusProtocolShelley nc files


-- | Instantiate 'Consensus.Protocol' for Shelley specifically.
--
-- Use this when you need to run the consensus with this specific protocol.
--
mkConsensusProtocolShelley
  :: NodeShelleyProtocolConfiguration
  -> Maybe ProtocolFilepaths
  -> ExceptT ShelleyProtocolInstantiationError IO
             (Consensus.Protocol IO (ShelleyBlock TPraosStandardCrypto)
                                 Consensus.ProtocolRealTPraos)
mkConsensusProtocolShelley NodeShelleyProtocolConfiguration {
                            npcShelleyGenesisFile,
                            npcShelleySupportedProtocolVersionMajor,
                            npcShelleySupportedProtocolVersionMinor,
                            npcShelleyMaxSupportedProtocolVersion
                          }
                          files = do
    (genesis, genesisHash) <- readGenesis npcShelleyGenesisFile
    optionalLeaderCredentials <- readLeaderCredentials files

    return $
      Consensus.ProtocolRealTPraos
        genesis
        (Nonce (Crypto.castHash genesisHash))
        (ProtVer npcShelleySupportedProtocolVersionMajor
                 npcShelleySupportedProtocolVersionMinor)
        npcShelleyMaxSupportedProtocolVersion
        optionalLeaderCredentials

readGenesis :: Crypto.HashAlgorithm h
            => GenesisFile
            -> ExceptT ShelleyProtocolInstantiationError IO
                       (ShelleyGenesis TPraosStandardCrypto,
                        Crypto.Hash h ByteString)
readGenesis (GenesisFile file) = do
    content <- handleIOExceptT (GenesisReadError file) $
                 BS.readFile file
    genesis <- firstExceptT (GenesisDecodeError file) $ hoistEither $
                 Aeson.eitherDecodeStrict' content
    let genesisHash = Crypto.hashRaw id content
    return (genesis, genesisHash)


readLeaderCredentials :: Maybe ProtocolFilepaths
                      -> ExceptT ShelleyProtocolInstantiationError IO
                                 (Maybe (TPraosLeaderCredentials TPraosStandardCrypto))

-- It's ok to supply none of the files
readLeaderCredentials Nothing = return Nothing
readLeaderCredentials (Just ProtocolFilepaths {
                              shelleyCertFile = Nothing,
                              shelleyVRFFile  = Nothing,
                              shelleyKESFile  = Nothing
                            }) = return Nothing

-- Or to supply all of the files
readLeaderCredentials (Just ProtocolFilepaths {
                              shelleyCertFile = Just certFile,
                              shelleyVRFFile  = Just vrfFile,
                              shelleyKESFile  = Just kesFile
                            }) = do

    OperationalCertificate opcert (StakePoolVerificationKey vkey) <-
      firstExceptT FileError . newExceptT $ readFileTextEnvelope AsOperationalCertificate certFile
    VrfSigningKey vrfKey <-
      firstExceptT FileError . newExceptT $ readFileTextEnvelope (AsSigningKey AsVrfKey) vrfFile
    KesSigningKey kesKey <-
      firstExceptT FileError . newExceptT $ readFileTextEnvelope (AsSigningKey AsKesKey) kesFile

    return $ Just TPraosLeaderCredentials {
               tpraosLeaderCredentialsIsCoreNode =
                 TPraosIsCoreNode {
                   tpraosIsCoreNodeOpCert     = opcert,
                   tpraosIsCoreNodeColdVerKey = coerceKeyRole vkey,
                   tpraosIsCoreNodeSignKeyVRF = vrfKey
                 },
               tpraosLeaderCredentialsSignKey = kesKey
             }

-- But not ok to supply some of the files without the others.
readLeaderCredentials (Just ProtocolFilepaths {shelleyCertFile = Nothing}) =
    throwError OCertNotSpecified
readLeaderCredentials (Just ProtocolFilepaths {shelleyVRFFile = Nothing}) =
    throwError VRFKeyNotSpecified
readLeaderCredentials (Just ProtocolFilepaths {shelleyKESFile = Nothing}) =
    throwError KESKeyNotSpecified


------------------------------------------------------------------------------
-- Errors
--

data ShelleyProtocolInstantiationError =
       GenesisReadError !FilePath !IOException
     | GenesisDecodeError !FilePath !String
     | FileError (Api.FileError TextEnvelopeError)
--TODO: pick a less generic constructor than FileError

     | OCertNotSpecified
     | VRFKeyNotSpecified
     | KESKeyNotSpecified
     deriving Show


renderShelleyProtocolInstantiationError :: ShelleyProtocolInstantiationError
                                        -> Text
renderShelleyProtocolInstantiationError pie =
  case pie of
    GenesisReadError fp err ->
        "There was an error reading the genesis file: "
     <> toS fp <> " Error: " <> (T.pack $ show err)

    GenesisDecodeError fp err ->
        "There was an error parsing the genesis file: "
     <> toS fp <> " Error: " <> (T.pack $ show err)

    FileError fileErr -> T.pack $ displayError fileErr

    OCertNotSpecified  -> missingFlagMessage "shelley-operational-certificate"
    VRFKeyNotSpecified -> missingFlagMessage "shelley-vrf-key"
    KESKeyNotSpecified -> missingFlagMessage "shelley-kes-key"
  where
    missingFlagMessage flag =
      "To create blocks, the --" <> flag <> " must also be specified"
