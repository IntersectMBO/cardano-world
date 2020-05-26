module Cardano.CLI.Shelley.Run.Address
  ( ShelleyAddressCmdError
  , renderShelleyAddressCmdError
  , runAddressCmd
  ) where

import           Cardano.Prelude hiding (putStrLn)
import           Prelude (putStrLn)

import qualified Data.ByteString.Char8 as BS
import qualified Data.Text.IO as Text

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT, newExceptT)

import           Cardano.Api

import           Cardano.CLI.Shelley.Parsers
                   (SigningKeyFile (..), VerificationKeyFile (..),
                    AddressCmd(..))
import           Cardano.CLI.Shelley.Run.Address.Info (ShelleyAddressInfoError, renderShelleyAddressInfoError,
                   runAddressInfo)

import qualified Cardano.Crypto.Hash.Class as Crypto

import qualified Shelley.Spec.Ledger.Keys as Ledger

data ShelleyAddressCmdError
  = ShelleyAddressCmdAddressInfoError !ShelleyAddressInfoError
  | ShelleyAddressCmdPaySigningKeyWriteErr !ApiError
  | ShelleyAddressCmdPayVerificationKeyReadErrr !ApiError
  | ShelleyAddressCmdPayVerificationKeyWriteErr !ApiError
  deriving Show

renderShelleyAddressCmdError :: ShelleyAddressCmdError -> Text
renderShelleyAddressCmdError err =
  case err of
    ShelleyAddressCmdAddressInfoError addrInfoErr ->
      "Error occurred while printing address info: " <> renderShelleyAddressInfoError addrInfoErr
    ShelleyAddressCmdPaySigningKeyWriteErr apiErr ->
      "Error occured while writing the payment signing key: " <> renderApiError apiErr
    ShelleyAddressCmdPayVerificationKeyReadErrr apiErr ->
      "Error occured while reading the payment verification key: " <> renderApiError apiErr
    ShelleyAddressCmdPayVerificationKeyWriteErr apiErr ->
      "Error occured while writing the payment verification key: " <> renderApiError apiErr

runAddressCmd :: AddressCmd -> ExceptT ShelleyAddressCmdError IO ()
runAddressCmd cmd =
  case cmd of
    AddressKeyGen vkf skf -> runAddressKeyGen  vkf skf
    AddressKeyHash vkf -> runAddressKeyHash vkf
    AddressBuild payVk stkVk -> runAddressBuild payVk stkVk
    AddressBuildMultiSig {} -> runAddressBuildMultiSig
    AddressInfo txt -> firstExceptT ShelleyAddressCmdAddressInfoError $ runAddressInfo txt

runAddressKeyGen :: VerificationKeyFile -> SigningKeyFile -> ExceptT ShelleyAddressCmdError IO ()
runAddressKeyGen (VerificationKeyFile vkeyPath) (SigningKeyFile skeyPath) = do
    sk <- liftIO shelleyGenSigningKey
    let vk = getPaymentVerificationKey sk
    firstExceptT ShelleyAddressCmdPayVerificationKeyWriteErr
      . ExceptT $ writePaymentVerificationKey vkeyPath vk
    firstExceptT ShelleyAddressCmdPaySigningKeyWriteErr
      . ExceptT $ writeSigningKey skeyPath sk

runAddressKeyHash :: VerificationKeyFile -> ExceptT ShelleyAddressCmdError IO ()
runAddressKeyHash (VerificationKeyFile vkeyPath) =
    firstExceptT ShelleyAddressCmdPayVerificationKeyReadErrr $ do
      PaymentVerificationKeyShelley vkey <- ExceptT $ readPaymentVerificationKey vkeyPath
      let Ledger.KeyHash khash = Ledger.hashKey vkey
      liftIO $ BS.putStrLn $ Crypto.getHashBytesAsHex khash

runAddressBuild :: VerificationKeyFile
                -> Maybe VerificationKeyFile
                -> ExceptT ShelleyAddressCmdError IO ()
runAddressBuild (VerificationKeyFile payVkeyFp) mstkVkeyFp =
  firstExceptT ShelleyAddressCmdPayVerificationKeyReadErrr $ do
    payVKey <- newExceptT $ readPaymentVerificationKey payVkeyFp
    mstkVKey <- case mstkVkeyFp of
                  Just (VerificationKeyFile stkVkeyFp) ->
                    Just <$> newExceptT (readStakingVerificationKey stkVkeyFp)
                  Nothing ->
                    return Nothing
    let addr = shelleyVerificationKeyAddress payVKey mstkVKey
    liftIO $ Text.putStrLn $ addressToHex addr

runAddressBuildMultiSig :: ExceptT ShelleyAddressCmdError IO ()
runAddressBuildMultiSig =
    liftIO $ putStrLn ("runAddressBuildMultiSig: TODO")
