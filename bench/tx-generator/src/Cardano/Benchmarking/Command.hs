{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-all-missed-specialisations -Wno-orphans #-}

module Cardano.Benchmarking.Command
(
  runCommand
, commandParser -- for tests
)
where

import           Prelude
import           System.Exit

import           Options.Applicative as Opt

import           Ouroboros.Network.NodeToClient (withIOManager)

import           Cardano.Benchmarking.CliArgsScript
                   (GeneratorCmd, parseGeneratorCmd, runPlainOldCliScript, runEraTransitionTest)
import           Cardano.Benchmarking.Script (runScript, parseScriptFileAeson, parseScriptFileLegacy)

data Command
  = CliArguments !GeneratorCmd
  | EraTransition !GeneratorCmd
  | Json !FilePath
  | LegacyJson !FilePath  

runCommand :: IO ()
runCommand = withIOManager $ \iocp -> do
  cmd <- customExecParser
           (prefs showHelpOnEmpty)
           (info commandParser mempty)
  case cmd of
    CliArguments   args -> runPlainOldCliScript iocp args >>= handleError
    EraTransition args -> runEraTransitionTest iocp args >>= handleError
    Json file     -> do
      script <- parseScriptFileAeson file
      runScript script iocp >>= handleError
    LegacyJson file     -> do
      script <- parseScriptFileLegacy file
      runScript script iocp >>= handleError
  where
  handleError :: Show a => Either a b -> IO ()
  handleError = \case
    Right _  -> exitSuccess
    Left err -> die $ show err

commandParser :: Parser Command
commandParser
  = subparser
    (  cliArgumentsCmd
    <> eraTransitionCmd
    <> jsonCmd
    <> legacyJsonCmd    
    )
 where
  cliArgumentsCmd = command "cliArguments"
    (CliArguments <$> info parseGeneratorCmd
      (  progDesc "tx-generator with CLI arguments"
      <> fullDesc
      <> header "tx-generator - load Cardano clusters with parametrised transaction flow (CLI version)"
      )
    )

  eraTransitionCmd = command "eraTransition"
    (EraTransition <$> info parseGeneratorCmd
      (  progDesc "tx-generator demo era transition"
      <> fullDesc
      <> header "tx-generator - load Cardano clusters with parametrised transaction flow (era transition)"
      )
    )

  jsonCmd = command "json"
    (Json <$> info (strArgument (metavar "FILEPATH"))
      (  progDesc "tx-generator run JsonScript"
      <> fullDesc
      <> header "tx-generator - run a generic benchmarking script"
      )
    )

  legacyJsonCmd = command "legacy-json"
    (LegacyJson <$> info (strArgument (metavar "FILEPATH"))
      (  progDesc "tx-generator run JsonScript (legacy format)"
      <> fullDesc
      <> header "tx-generator - run a generic benchmarking script (legacy JSON format)"
      )
    )
