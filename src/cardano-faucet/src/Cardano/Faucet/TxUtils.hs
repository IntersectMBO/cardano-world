{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Faucet.TxUtils where

import Cardano.Api (Lovelace, IsShelleyBasedEra, ShelleyBasedEra, TxIn, TxOut(TxOut), CtxUTxO, TxBody, TxBodyContent(TxBodyContent), Witness(KeyWitness), KeyWitnessInCtx(KeyWitnessForSpending), TxInsCollateral(TxInsCollateralNone), TxInsReference(TxInsReferenceNone), TxTotalCollateral(TxTotalCollateralNone), TxReturnCollateral(TxReturnCollateralNone), TxMetadataInEra(TxMetadataNone), TxAuxScripts(TxAuxScriptsNone), TxExtraKeyWitnesses(TxExtraKeyWitnessesNone), TxWithdrawals(TxWithdrawalsNone), TxCertificates, BuildTxWith(BuildTxWith), TxUpdateProposal(TxUpdateProposalNone), TxMintValue(..), TxScriptValidity(TxScriptValidityNone), shelleyBasedToCardanoEra, Tx, makeShelleyKeyWitness, makeSignedTransaction, AddressAny, TxId, getTxId, BuildTx, ShelleyWitnessSigningKey)
import Cardano.Api.Shelley (lovelaceToValue, Value, createAndValidateTransactionBody, TxGovernanceActions(TxGovernanceActionsNone), TxVotes(TxVotesNone))
import Cardano.CLI.Shelley.Run.Transaction
import Cardano.CLI.Types
import Cardano.Faucet.Misc (getValue, faucetValueToLovelace)
import Cardano.Faucet.Types (FaucetWebError(..), FaucetValue)
import Cardano.Faucet.Utils
import Cardano.Prelude hiding ((%))
import Control.Monad.Trans.Except.Extra (left)

getMintedValue :: TxMintValue BuildTx era -> Value
getMintedValue (TxMintValue _ val _) = val
getMintedValue (TxMintNone) = mempty

newtype Fee = Fee Lovelace

txBuild :: IsShelleyBasedEra era
  => ShelleyBasedEra era
  -> (TxIn, TxOut CtxUTxO era)
  -> Either AddressAny [TxOutAnyEra]
  -> TxCertificates BuildTx era
  -> TxMintValue BuildTx era
  -> Fee
  -> ExceptT FaucetWebError IO (TxBody era)
txBuild sbe (txin, txout) addressOrOutputs certs minting (Fee fixedFee) = do
  let
    --localNodeConnInfo = LocalNodeConnectInfo cModeParams networkId sockPath
    era = shelleyBasedToCardanoEra sbe
    unwrap :: TxOut ctx1 era1 -> FaucetValue
    unwrap (TxOut _ val _ _) = getValue val
    value :: Lovelace
    value = faucetValueToLovelace $ unwrap txout
    change :: Lovelace
    change = value - fixedFee
    mintedValue = getMintedValue minting
    -- TODO, add minted tokens
    changeValue :: Value
    changeValue = (lovelaceToValue change) <> mintedValue

    getTxOuts :: Either AddressAny [TxOutAnyEra] -> [TxOutAnyEra]
    getTxOuts (Left addr) = [ TxOutAnyEra addr changeValue TxOutDatumByNone ReferenceScriptAnyEraNone ]
    getTxOuts (Right outs) = outs

  txBodyContent <- TxBodyContent
    <$> pure [(txin, BuildTxWith $ KeyWitness KeyWitnessForSpending)]
    <*> pure TxInsCollateralNone
    <*> pure TxInsReferenceNone
    <*> mapM (\x -> withExceptT (FaucetWebErrorTodo . renderShelleyTxCmdError) $ toTxOutInAnyEra era x) (getTxOuts addressOrOutputs)
    <*> pure TxTotalCollateralNone
    <*> pure TxReturnCollateralNone
    <*> validateTxFee era (Just fixedFee)
    <*> noBoundsIfSupported era
    <*> pure TxMetadataNone
    <*> pure TxAuxScriptsNone
    <*> pure TxExtraKeyWitnessesNone
    <*> pure (BuildTxWith Nothing)
    <*> pure TxWithdrawalsNone
    <*> pure certs
    <*> pure TxUpdateProposalNone
    <*> pure minting
    <*> pure TxScriptValidityNone
    <*> pure TxGovernanceActionsNone
    <*> pure TxVotesNone

  case createAndValidateTransactionBody txBodyContent of
    Left err -> left $ FaucetWebErrorTodo $ show err
    Right txbody -> pure txbody
  {-
   -- keep this code for now, as an example of how to use the cardano api in a monad
  eInMode <- case toEraInMode era CardanoMode of
    Just result -> return result
    Nothing -> left (FaucetWebErrorConsensusModeMismatchTxBalance (show $ AnyConsensusMode CardanoMode) (AnyCardanoEra era))

  let
    utxo = UTxO $ Map.fromList [ (txin, txout) ]

  (pparams, eraHistory, systemStart, stakePools) <-
    newExceptT . fmap (join . first (FaucetWebErrorAcquireFailure . show)) $
      executeLocalStateQueryExpr localNodeConnInfo Nothing $ \_ntcVersion -> runExceptT $ do
        --UTxO utxo <- firstExceptT (_ . ShelleyTxCmdTxSubmitErrorEraMismatch) . newExceptT . queryExpr
        --  $ QueryInEra eInMode $ QueryInShelleyBasedEra sbe
        --  $ QueryUTxO (QueryUTxOByTxIn (Set.singleton txin))

        --when (null utxo || not (txin `L.elem` Map.keys utxo)) $ do
          -- txout for txin does not exist
        --  left $ ShelleyTxCmdTxInsDoNotExist [txin]

        pparams <- firstExceptT (FaucetWebErrorEraMismatch . show) . newExceptT . queryExpr
          $ QueryInEra eInMode $ QueryInShelleyBasedEra sbe QueryProtocolParameters

        eraHistory <- lift . queryExpr $ QueryEraHistory CardanoModeIsMultiEra

        systemStart <- lift $ queryExpr QuerySystemStart

        stakePools <- firstExceptT (FaucetWebErrorEraMismatch . show) . ExceptT $
          queryExpr . QueryInEra eInMode . QueryInShelleyBasedEra sbe $ QueryStakePools

        return (pparams, eraHistory, systemStart, stakePools)

  cAddr <- pure $ case anyAddressInEra era changeAddr of
    Just addr -> addr
    Nothing -> Prelude.error "txBuild: Byron address used: "

  (BalancedTxBody balancedTxBody _ _fee) <- firstExceptT (FaucetWebErrorAutoBalance . T.pack . displayError) . hoistEither $
    makeTransactionBodyAutoBalance eInMode systemStart eraHistory pparams stakePools utxo txBodyContent cAddr Nothing

  return balancedTxBody
  -}

txSign :: IsShelleyBasedEra era
  => TxBody era
  -> [ShelleyWitnessSigningKey]
  -> ExceptT ShelleyTxCmdError IO (Tx era)
txSign txBody sks = do
  --let (sksByron, sksShelley) = partitionSomeWitnesses $ map categoriseSomeWitness sks

  let shelleyKeyWitnesses = map (makeShelleyKeyWitness txBody) sks
  let tx = makeSignedTransaction shelleyKeyWitnesses txBody

  return tx

makeAndSignTx :: IsShelleyBasedEra era
  => ShelleyBasedEra era
  -> (TxIn, TxOut CtxUTxO era)
  -> Either AddressAny [TxOutAnyEra]
  -> [ShelleyWitnessSigningKey]
  -> TxCertificates BuildTx era
  -> TxMintValue BuildTx era
  -> Fee
  -> ExceptT FaucetWebError IO (Tx era, TxId)
makeAndSignTx sbe txinout addressOrOutputs skeys certs minting fee = do
  -- instead of having to specify an output that is exactly equal to input-fees
  -- i specify no outputs, and set the change addr to the end-user
  unsignedTx <- txBuild sbe txinout addressOrOutputs certs minting fee
  let
    txid :: TxId
    txid = getTxId unsignedTx
  signedTx <- withExceptT (FaucetWebErrorTodo . renderShelleyTxCmdError) $ txSign unsignedTx skeys
  pure (signedTx, txid)
