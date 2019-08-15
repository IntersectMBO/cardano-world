{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}


import           Data.Semigroup ((<>))

import           Options.Applicative

import           Cardano.Node.Configuration.Lib (finaliseCardanoConfiguration)
import           Cardano.Node.Configuration.PartialTypes (PartialCardanoConfiguration (..))
import           Cardano.Node.Configuration.Presets (mainnetConfiguration)
import           Cardano.Node.Configuration.Types (CardanoConfiguration (..),
                                                   CardanoEnvironment (..))
import           Cardano.Node.Features.Logging (LoggingCLIArguments (..),
                                                LoggingLayer (..),
                                                createLoggingFeature
                                                )
import           Cardano.Prelude hiding (option)
import           Cardano.Shell.Lib (GeneralException (..),
                                    runCardanoApplicationWithFeatures)
import           Cardano.Shell.Types (CardanoApplication (..),
                                      CardanoFeature (..),
                                      CardanoFeatureInit (..))

import           Cardano.Node.CLI

import           Cardano.Node.ConfigCLI
import           Cardano.Node.Parsers (loggingParser)
import           Cardano.Node.Run


-- | Main function.
main :: IO ()
main = do

    let cardanoConfiguration = mainnetConfiguration
    let cardanoEnvironment   = NoEnvironment

    logConfig           <- execParser opts

    (cardanoFeatures, nodeLayer) <- initializeAllFeatures logConfig cardanoConfiguration cardanoEnvironment

    let cardanoApplication :: NodeLayer -> CardanoApplication
        cardanoApplication = CardanoApplication . nlRunNode

    runCardanoApplicationWithFeatures cardanoFeatures (cardanoApplication nodeLayer)

initializeAllFeatures :: CLIArguments -> PartialCardanoConfiguration -> CardanoEnvironment -> IO ([CardanoFeature], NodeLayer)
initializeAllFeatures (CLIArguments logCli nodeCli) partialConfig cardanoEnvironment = do
    finalConfig <- case finaliseCardanoConfiguration $
                          mergeConfiguration partialConfig (commonCLI nodeCli)
                   of
      Left err -> throwIO $ ConfigurationError err
      Right x  -> pure x

    (loggingLayer, loggingFeature) <- createLoggingFeature cardanoEnvironment finalConfig logCli
    (nodeLayer   , nodeFeature)    <- createNodeFeature loggingLayer nodeCli cardanoEnvironment finalConfig

    -- Here we return all the features.
    let allCardanoFeatures :: [CardanoFeature]
        allCardanoFeatures =
            [ loggingFeature
            , nodeFeature
            ]

    pure (allCardanoFeatures, nodeLayer)

--------------------------------
-- Layer
--------------------------------

data NodeLayer = NodeLayer
    { nlRunNode   :: forall m. MonadIO m => m ()
    }

--------------------------------
-- Node Feature
--------------------------------

type NodeCardanoFeature = CardanoFeatureInit CardanoEnvironment LoggingLayer CardanoConfiguration NodeCLIArguments NodeLayer


createNodeFeature :: LoggingLayer -> NodeCLIArguments -> CardanoEnvironment -> CardanoConfiguration -> IO (NodeLayer, CardanoFeature)
createNodeFeature loggingLayer nodeCLI cardanoEnvironment cardanoConfiguration = do
    -- we parse any additional configuration if there is any
    -- We don't know where the user wants to fetch the additional configuration from, it could be from
    -- the filesystem, so we give him the most flexible/powerful context, @IO@.

    -- we construct the layer
    nodeLayer <- (featureInit nodeCardanoFeatureInit) cardanoEnvironment loggingLayer cardanoConfiguration nodeCLI

    -- we construct the cardano feature
    let cardanoFeature = nodeCardanoFeature nodeCardanoFeatureInit nodeLayer

    -- we return both
    pure (nodeLayer, cardanoFeature)

nodeCardanoFeatureInit :: NodeCardanoFeature
nodeCardanoFeatureInit = CardanoFeatureInit
    { featureType    = "NodeFeature"
    , featureInit    = featureStart'
    , featureCleanup = featureCleanup'
    }
  where
    featureStart' :: CardanoEnvironment -> LoggingLayer -> CardanoConfiguration -> NodeCLIArguments -> IO NodeLayer
    featureStart' _ loggingLayer cc nodeCli = do
        pure $ NodeLayer {nlRunNode = liftIO $ runNode nodeCli loggingLayer cc}

    featureCleanup' :: NodeLayer -> IO ()
    featureCleanup' _ = pure ()


nodeCardanoFeature :: NodeCardanoFeature -> NodeLayer -> CardanoFeature
nodeCardanoFeature nodeCardanoFeature' nodeLayer = CardanoFeature
    { featureName       = featureType nodeCardanoFeature'
    , featureStart      = pure ()
    , featureShutdown   = liftIO $ (featureCleanup nodeCardanoFeature') nodeLayer
    }


-- | The product type of all command line arguments.
-- All here being - from all the features.
data CLIArguments = CLIArguments !LoggingCLIArguments !NodeCLIArguments

-- | The product parser for all the CLI arguments.
commandLineParser :: Parser CLIArguments
commandLineParser = CLIArguments
    <$> loggingParser
    <*> nodeParser

-- | Top level parser with info.
opts :: ParserInfo CLIArguments
opts = info (commandLineParser <**> helper)
    (  fullDesc
    <> progDesc "Cardano demo node."
    <> header "Demo node to run."
    )
