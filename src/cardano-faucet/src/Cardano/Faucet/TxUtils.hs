{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Faucet.TxUtils where

import Cardano.Api (Lovelace(Lovelace), IsShelleyBasedEra, ShelleyBasedEra, NetworkId, TxIn, TxOut(TxOut), CtxUTxO, TxBody, TxBodyContent(TxBodyContent), Witness(KeyWitness), KeyWitnessInCtx(KeyWitnessForSpending), TxInsCollateral(TxInsCollateralNone), TxInsReference(TxInsReferenceNone), TxTotalCollateral(TxTotalCollateralNone), TxReturnCollateral(TxReturnCollateralNone), TxMetadataInEra(TxMetadataNone), TxAuxScripts(TxAuxScriptsNone), TxExtraKeyWitnesses(TxExtraKeyWitnessesNone), TxWithdrawals(TxWithdrawalsNone), TxCertificates, BuildTxWith(BuildTxWith), TxUpdateProposal(TxUpdateProposalNone), TxMintValue(TxMintNone), TxScriptValidity(TxScriptValidityNone), shelleyBasedToCardanoEra, Tx, makeShelleyKeyWitness, makeSignedTransaction, AddressAny, TxId, getTxId, BuildTx)
import Cardano.CLI.Shelley.Run.Transaction
import Cardano.Api.Shelley (lovelaceToValue, makeTransactionBody)
import Cardano.CLI.Types
import Cardano.Faucet.Types (FaucetWebError(..), FaucetValue, faucetValueToLovelace)
import Cardano.Faucet.Utils
import Cardano.Faucet.Misc
import Cardano.Prelude hiding ((%))
import Control.Monad.Trans.Except.Extra (firstExceptT, left, hoistEither)

txBuild :: IsShelleyBasedEra era
  => ShelleyBasedEra era
  -> (TxIn, TxOut CtxUTxO era)
  -> TxOutChangeAddress
  -> TxCertificates BuildTx era
  -> ExceptT FaucetWebError IO (TxBody era)
txBuild sbe (txin, txout) (TxOutChangeAddress changeAddr) certs = do
  let
    --localNodeConnInfo = LocalNodeConnectInfo cModeParams networkId sockPath
    era = shelleyBasedToCardanoEra sbe
    fixedFee = Lovelace 200000
    unwrap :: TxOut ctx1 era1 -> FaucetValue
    unwrap (TxOut _ val _ _) = getValue val
    value :: Lovelace
    value = faucetValueToLovelace $ unwrap txout
    change = value - fixedFee

  txBodyContent <- TxBodyContent
    <$> pure [(txin, BuildTxWith $ KeyWitness KeyWitnessForSpending)]
    <*> pure TxInsCollateralNone
    <*> pure TxInsReferenceNone
    <*> mapM (\x -> withExceptT (FaucetWebErrorTodo . renderShelleyTxCmdError) $ toTxOutInAnyEra era x) [ (TxOutAnyEra changeAddr (lovelaceToValue change) TxOutDatumByNone ReferenceScriptAnyEraNone) ]
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
    <*> pure TxMintNone
    <*> pure TxScriptValidityNone

  case makeTransactionBody txBodyContent of
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
  => NetworkId
  -> TxBody era
  -> [SomeWitness]
  -> ExceptT ShelleyTxCmdError IO (Tx era)
txSign networkId txBody sks = do
  let (sksByron, sksShelley) = partitionSomeWitnesses $ map categoriseSomeWitness sks

  -- Byron witnesses require the network ID. This can either be provided
  -- directly or derived from a provided Byron address.
  byronWitnesses <- firstExceptT (ShelleyTxCmdBootstrapWitnessError) . hoistEither $
    mkShelleyBootstrapWitnesses (Just networkId) txBody sksByron

  let shelleyKeyWitnesses = map (makeShelleyKeyWitness txBody) sksShelley
  let tx = makeSignedTransaction (byronWitnesses ++ shelleyKeyWitnesses) txBody

  return tx

makeAndSignTx :: IsShelleyBasedEra era
  => ShelleyBasedEra era
  -> (TxIn, TxOut CtxUTxO era)
  -> AddressAny
  -> NetworkId
  -> [SomeWitness]
  -> TxCertificates BuildTx era
  -> ExceptT FaucetWebError IO (Tx era, TxId)
makeAndSignTx sbe txinout addressAny network skeys certs = do
  -- instead of having to specify an output that is exactly equal to input-fees
  -- i specify no outputs, and set the change addr to the end-user
  unsignedTx <- txBuild sbe txinout (TxOutChangeAddress addressAny) certs
  let
    txid :: TxId
    txid = getTxId unsignedTx
  signedTx <- withExceptT (FaucetWebErrorTodo . renderShelleyTxCmdError) $ txSign network unsignedTx skeys
  pure (signedTx, txid)
