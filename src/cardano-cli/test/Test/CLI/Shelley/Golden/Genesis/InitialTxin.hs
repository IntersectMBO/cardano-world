{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.CLI.Shelley.Golden.Genesis.InitialTxin
  ( golden_shelleyGenesisInitialTxin
  ) where

import Cardano.Prelude hiding (to)

import Hedgehog (Property)

import qualified System.IO as IO
import qualified Test.OptParse as OP

{- HLINT ignore "Use camelCase" -}

golden_shelleyGenesisInitialTxin :: Property
golden_shelleyGenesisInitialTxin = OP.propertyOnce $ do
  OP.workspace "tmp/genesis-initial-txin" $ \tempDir -> do
    let verificationKeyFile = "test/Test/golden/shelley/keys/genesis_verification_keys/genesis-utxo.vkey"
        utxoHashFile = tempDir <> "/utxo_hash"
        goldenUtxoHashFile = "test/Test/golden/shelley/keys/genesis_utxo_hashes/utxo_hash"

    utxoHash <-liftIO $ OP.execCardanoCLI
        [ "shelley","genesis","initial-txin"
        , "--testnet-magic", "16"
        , "--verification-key-file", verificationKeyFile
        ]

    liftIO $ IO.writeFile utxoHashFile utxoHash

    goldenUtxoHash <- OP.noteEvalM . liftIO $ IO.readFile goldenUtxoHashFile

    OP.equivalence [] utxoHash goldenUtxoHash
