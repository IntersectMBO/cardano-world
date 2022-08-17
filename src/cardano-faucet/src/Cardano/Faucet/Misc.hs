{-# LANGUAGE ImportQualifiedPost #-}

module Cardano.Faucet.Misc where

import Cardano.Api (ConsensusModeParams(CardanoModeParams), CardanoMode, EpochSlots(EpochSlots), AddressAny, parseAddressAny, TxOutValue(TxOutAdaOnly, TxOutValue), CardanoEra, EraInMode, toEraInMode, ConsensusMode(CardanoMode),AssetId(AdaAssetId), Quantity, valueToList)
import Cardano.Api.Shelley (Lovelace, selectLovelace, AssetId(AssetId))
import Cardano.Faucet.Types
import Cardano.Prelude
import Control.Monad.Trans.Except.Extra (left)
import Data.Text qualified as T
import Text.Parsec

getValue :: TxOutValue era -> FaucetValue
getValue (TxOutAdaOnly _ ll) = Ada ll
getValue (TxOutValue _ val) = convertRemaining remaining
  where
    ll :: Lovelace
    ll = selectLovelace val
    isntAda :: (AssetId, Quantity) -> Bool
    isntAda (AdaAssetId, _) = False
    isntAda (AssetId _ _, _) = True
    remaining :: [(AssetId, Quantity)]
    remaining = filter isntAda (valueToList val)
    convertRemaining :: [(AssetId, Quantity)] -> FaucetValue
    convertRemaining [t] = FaucetValueMultiAsset ll (FaucetToken t)
    convertRemaining [] = Ada ll
    convertRemaining _ = FaucetValueManyTokens ll

toFaucetValue :: ApiKeyValue -> FaucetValue
toFaucetValue (ApiKeyValue _ lovelace _ Nothing _) = Ada lovelace
toFaucetValue (ApiKeyValue _ ll _ (Just t) _) = FaucetValueMultiAsset ll t

-- returns just the lovelace component and ignores tokens
faucetValueToLovelace :: FaucetValue -> Lovelace
faucetValueToLovelace (Ada ll) = ll
faucetValueToLovelace (FaucetValueMultiAsset ll _token) = ll
faucetValueToLovelace (FaucetValueManyTokens ll) = ll

parseAddress :: Text -> ExceptT FaucetWebError IO AddressAny
parseAddress addr = case parse (parseAddressAny <* eof) "" (T.unpack addr) of
  Right a -> return $ a
  Left e -> left $ FaucetWebErrorInvalidAddress addr (show e)

defaultCModeParams :: ConsensusModeParams CardanoMode
defaultCModeParams = CardanoModeParams (EpochSlots defaultByronEpochSlots)

defaultByronEpochSlots :: Word64
defaultByronEpochSlots = 21600

convertEra :: Monad m => CardanoEra era -> ExceptT FaucetWebError m (EraInMode era CardanoMode)
convertEra era = case (toEraInMode era CardanoMode) of
  Just eraInMode -> pure eraInMode
  Nothing -> left $ FaucetWebErrorEraConversion
