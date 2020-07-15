{-# LANGUAGE OverloadedStrings #-}

module Test.CLI.Shelley.Golden.Address.KeyGen
  ( golden_shelleyAddressKeyGen
  ) where

import Cardano.Prelude hiding (to)

import Hedgehog (Property)

import qualified Test.OptParse as OP
import qualified System.IO as IO

{- HLINT ignore "Use camelCase" -}

golden_shelleyAddressKeyGen :: Property
golden_shelleyAddressKeyGen = OP.propertyOnce $ OP.workspace "tmp/address-key-gen" $ \tempDir -> do
  let addressVKeyFile = tempDir <> "/address.vkey"
  let addressSKeyFile = tempDir <> "/address.skey"
  let outputFiles = [addressVKeyFile, addressSKeyFile]

  void $ liftIO $ OP.execCardanoCLI
    [ "shelley","address","key-gen"
    , "--verification-key-file", addressVKeyFile
    , "--signing-key-file", addressSKeyFile
    ]

  void $ OP.noteEvalM $ liftIO $ IO.readFile addressVKeyFile
  void $ OP.noteEvalM $ liftIO $ IO.readFile addressSKeyFile

  OP.assertFilesExist outputFiles

  OP.assertFileOccurences 1 "PaymentVerificationKeyShelley" addressVKeyFile
  OP.assertFileOccurences 1 "SigningKeyShelley" addressSKeyFile
