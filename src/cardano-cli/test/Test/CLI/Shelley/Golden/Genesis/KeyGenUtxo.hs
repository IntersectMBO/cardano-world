{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.CLI.Shelley.Golden.Genesis.KeyGenUtxo
  ( golden_shelleyGenesisKeyGenUtxo
  ) where

import Cardano.Prelude hiding (to)

import Hedgehog (Property)

import qualified Test.OptParse as OP

{- HLINT ignore "Use camelCase" -}

golden_shelleyGenesisKeyGenUtxo :: Property
golden_shelleyGenesisKeyGenUtxo = OP.propertyOnce $ do
  OP.workspace "tmp/genesis-key-gen-utxo" $ \tempDir -> do
    let utxoVerificationKeyFile = tempDir <> "/utxo.vkey"
        utxoSigningKeyFile = tempDir <> "/utxo.skey"

    void . liftIO $ OP.execCardanoCLI
        [ "shelley","genesis","key-gen-utxo"
        , "--verification-key-file", utxoVerificationKeyFile
        , "--signing-key-file", utxoSigningKeyFile
        ]

    OP.assertFileOccurences 1 "GenesisUTxOVerificationKey_ed25519" $ utxoVerificationKeyFile
    OP.assertFileOccurences 1 "GenesisUTxOSigningKey_ed25519" $ utxoSigningKeyFile
