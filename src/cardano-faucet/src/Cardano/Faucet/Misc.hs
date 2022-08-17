{-# LANGUAGE ImportQualifiedPost #-}

module Cardano.Faucet.Misc where

import Cardano.Api (ConsensusModeParams(CardanoModeParams), CardanoMode, EpochSlots(EpochSlots), AddressAny, parseAddressAny, TxOutValue(TxOutAdaOnly, TxOutValue), CardanoEra, EraInMode, toEraInMode, ConsensusMode(CardanoMode),AssetId(AdaAssetId, AssetId), quantityToLovelace, Quantity, valueToList, Lovelace, lovelaceToQuantity)
import Cardano.Faucet.Types
import Cardano.Prelude
import Control.Monad.Trans.Except.Extra (left)
import Data.Text qualified as T
import Text.Parsec

getValue :: TxOutValue era -> FaucetValue
getValue (TxOutAdaOnly _ ll) = Ada ll
getValue (TxOutValue _ val) = convertValue $ valueToList val
  where
    convertValue :: [(AssetId, Quantity)] -> FaucetValue
    convertValue [(AdaAssetId, q)] = Ada $ quantityToLovelace q
    convertValue other = FaucetValueMultiAsset (map FaucetToken other)

toFaucetValue :: ApiKeyValue -> FaucetValue
toFaucetValue (ApiKeyValue _ lovelace _ Nothing _) = Ada lovelace
toFaucetValue (ApiKeyValue _ lovelace _ t _) = FaucetValueMultiAsset (FaucetToken (AdaAssetId, lovelaceToQuantity lovelace):catMaybes [ t ])

faucetValueToLovelace :: FaucetValue -> Lovelace
faucetValueToLovelace (Ada l) = l
faucetValueToLovelace (FaucetValueMultiAsset tokenList) = quantityToLovelace $ foldl' fn 0 tokenList
  where
    fn s (FaucetToken (AdaAssetId, q)) = s + q
    fn s (FaucetToken (AssetId _ _, _)) = s

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
