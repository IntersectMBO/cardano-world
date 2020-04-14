module Cardano.Api.View
  ( parseAddressView
  , parseKeyPairView
  , parsePublicKeyView
  , parseTxSignedView
  , parseTxUnsignedView

  , readAddress
  , readKeyPair
  , readPublicKey
  , readTxSigned
  , readTxUnsigned

  , renderAddressView
  , renderKeyPairView
  , renderPublicKeyView
  , renderTxSignedView
  , renderTxUnsignedView

  , writeAddress
  , writeKeyPair
  , writePublicKey
  , writeTxSigned
  , writeTxUnsigned
  ) where

import           Cardano.Api.CBOR
import           Cardano.Api.Types
import           Cardano.Api.Error
import           Cardano.Api.TextView

import           Cardano.Prelude

import           Control.Monad.Trans.Except.Extra (handleIOExceptT, hoistEither, runExceptT)

import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS


parseAddressView :: ByteString -> Either ApiError Address
parseAddressView bs =
  join (addressFromCBOR . tvRawCBOR <$> parseTextView bs)

parseKeyPairView :: ByteString -> Either ApiError KeyPair
parseKeyPairView bs =
  join (keyPairFromCBOR . tvRawCBOR <$> parseTextView bs)

parsePublicKeyView :: ByteString -> Either ApiError PublicKey
parsePublicKeyView bs =
  join (publicKeyFromCBOR . tvRawCBOR <$> parseTextView bs)

parseTxSignedView :: ByteString -> Either ApiError TxSigned
parseTxSignedView bs =
  join (txSignedFromCBOR . tvRawCBOR <$> parseTextView bs)

parseTxUnsignedView :: ByteString -> Either ApiError TxUnsigned
parseTxUnsignedView bs =
  join (txUnsignedFromCBOR . tvRawCBOR <$> parseTextView bs)

renderAddressView :: Address -> ByteString
renderAddressView addr =
  case addr of
    AddressByron {} -> renderTextView $ TextView "PublicKeyByron" "Free form text" cbor
    AddressShelley {} -> renderTextView $ TextView "KeyPairShelley" "Free form text" cbor
  where
    cbor :: ByteString
    cbor = addressToCBOR addr

renderKeyPairView :: KeyPair -> ByteString
renderKeyPairView kp =
  case kp of
    KeyPairByron {} -> renderTextView $ TextView "PublicKeyByron" "Free form text" cbor
    KeyPairShelley {} -> renderTextView $ TextView "KeyPairShelley" "Free form text" cbor
  where
    cbor :: ByteString
    cbor = keyPairToCBOR kp

renderPublicKeyView :: PublicKey -> ByteString
renderPublicKeyView pk =
  case pk of
    PubKeyByron {} -> renderTextView $ TextView "PublicKeyByron" "Free form text" cbor
    PubKeyShelley {} -> renderTextView $ TextView "PubKeyShelley" "Free form text" cbor
  where
    cbor :: ByteString
    cbor = publicKeyToCBOR pk

renderTxSignedView :: TxSigned -> ByteString
renderTxSignedView ts =
  case ts of
    TxSignedByron {} -> renderTextView $ TextView "TxSignedByron" "Free form text" cbor
    TxSignedShelley {} -> renderTextView $ TextView "TxSignedShelley" "Free form text" cbor
  where
    cbor :: ByteString
    cbor = txSignedToCBOR ts

renderTxUnsignedView :: TxUnsigned -> ByteString
renderTxUnsignedView tu =
  case tu of
    TxUnsignedByron {} -> renderTextView $ TextView "TxUnsignedByron" "Free form text" cbor
    TxUnsignedShelley {} -> renderTextView $ TextView "TxUnsignedShelley" "Free form text" cbor
  where
    cbor :: ByteString
    cbor = txUnsignedToCBOR tu

-- -------------------------------------------------------------------------------------------------

readAddress :: FilePath -> IO (Either ApiError Address)
readAddress path =
  runExceptT $ do
    bs <- handleIOExceptT (ApiErrorIO path) $ BS.readFile path
    hoistEither $ parseAddressView bs

readKeyPair :: FilePath -> IO (Either ApiError KeyPair)
readKeyPair path =
  runExceptT $ do
    bs <- handleIOExceptT (ApiErrorIO path) $ BS.readFile path
    hoistEither $ parseKeyPairView bs

readPublicKey :: FilePath -> IO (Either ApiError PublicKey)
readPublicKey path =
  runExceptT $ do
    bs <- handleIOExceptT (ApiErrorIO path) $ BS.readFile path
    hoistEither $ parsePublicKeyView bs

readTxSigned :: FilePath -> IO (Either ApiError TxSigned)
readTxSigned path =
  runExceptT $ do
    bs <- handleIOExceptT (ApiErrorIO path) $ BS.readFile path
    hoistEither $ parseTxSignedView bs

readTxUnsigned :: FilePath -> IO (Either ApiError TxUnsigned)
readTxUnsigned path =
  runExceptT $ do
    bs <- handleIOExceptT (ApiErrorIO path) $ BS.readFile path
    hoistEither $ parseTxUnsignedView bs

writeAddress :: FilePath -> Address -> IO (Either ApiError ())
writeAddress path kp =
  runExceptT .
    handleIOExceptT (ApiErrorIO path) $ BS.writeFile path (renderAddressView kp)

writeKeyPair :: FilePath -> KeyPair -> IO (Either ApiError ())
writeKeyPair path kp =
  runExceptT .
    handleIOExceptT (ApiErrorIO path) $ BS.writeFile path (renderKeyPairView kp)

writePublicKey :: FilePath -> PublicKey -> IO (Either ApiError ())
writePublicKey path kp =
  runExceptT .
    handleIOExceptT (ApiErrorIO path) $ BS.writeFile path (renderPublicKeyView kp)

writeTxSigned :: FilePath -> TxSigned -> IO (Either ApiError ())
writeTxSigned path kp =
  runExceptT .
    handleIOExceptT (ApiErrorIO path) $ BS.writeFile path (renderTxSignedView kp)

writeTxUnsigned :: FilePath -> TxUnsigned -> IO (Either ApiError ())
writeTxUnsigned path kp =
  runExceptT .
    handleIOExceptT (ApiErrorIO path) $ BS.writeFile path (renderTxUnsignedView kp)
