{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module TxUtils
(
   calculateMinimumLovelace,
   makeTxBodyContent,
  --  balanceTxBody,
   signAndSubmitTransaction
)

where

import qualified Cardano.Ledger.Alonzo.Rules.Utxo as Alonzo
import           Cardano.Api.Byron hiding (SomeByronSigningKey (..))
import           Cardano.Api.Shelley
import Cardano.Api
import Error (SomeError(SomeError))
import Ouroboros.Network.Protocol.LocalTxSubmission.Type




data MinimumUTxOError =
    PParamsMinUTxOMissing
  | PParamsUTxOCostPerWordMissing
  deriving Show

instance Error MinimumUTxOError where
  displayError PParamsMinUTxOMissing =
    "\"minUtxoValue\" field not present in protocol parameters when \
    \trying to calculate minimum UTxO value."
  displayError PParamsUTxOCostPerWordMissing =
    "\"utxoCostPerWord\" field not present in protocol parameters when \
    \trying to calculate minimum UTxO value."

txOutValueToValue :: TxOutValue era -> Value
txOutValueToValue tv =
  case tv of
    TxOutAdaOnly _ l -> lovelaceToValue l
    TxOutValue _ v -> v

calculateMinimumUTxO
  :: ShelleyBasedEra era
  -> TxOut era
  -> ProtocolParameters
  -> Either MinimumUTxOError Value
calculateMinimumUTxO era txout@(TxOut _ v _) pparams' =
  case era of
    ShelleyBasedEraShelley -> lovelaceToValue <$> getMinUTxOPreAlonzo pparams'
    ShelleyBasedEraAllegra -> calcMinUTxOAllegraMary
    ShelleyBasedEraMary -> calcMinUTxOAllegraMary
    ShelleyBasedEraAlonzo ->
      case protocolParamUTxOCostPerWord pparams' of
        Just (Lovelace costPerWord) -> do
          Right . lovelaceToValue
            $ Lovelace (Alonzo.utxoEntrySize (toShelleyTxOut era txout) * costPerWord)
        Nothing -> Left PParamsUTxOCostPerWordMissing
 where
   calcMinUTxOAllegraMary :: Either MinimumUTxOError Value
   calcMinUTxOAllegraMary = do
     let val = txOutValueToValue v
     minUTxO <- getMinUTxOPreAlonzo pparams'
     Right . lovelaceToValue $ calcMinimumDeposit val minUTxO

   getMinUTxOPreAlonzo
     :: ProtocolParameters -> Either MinimumUTxOError Lovelace
   getMinUTxOPreAlonzo =
     maybe (Left PParamsMinUTxOMissing) Right . protocolParamMinUTxOValue


calculateMinimumLovelace :: TxOut AlonzoEra -> ProtocolParameters -> Either SomeError Value
calculateMinimumLovelace txOut pParam = case calculateMinimumUTxO ShelleyBasedEraAlonzo txOut pParam of
                                            Left err -> Left (SomeError "Error PParamsUTxOCostPerWordMissing.")
                                            Right value -> Right value



type TxIns build era = [(TxIn, BuildTxWith build (Witness WitCtxTxIn era))]

makeTxBodyContent :: TxIns BuildTx AlonzoEra-> [TxOut AlonzoEra] -> ProtocolParameters -> Integer -> TxMintValue BuildTx AlonzoEra -> TxBodyContent BuildTx AlonzoEra
makeTxBodyContent txins txouts pParam fee mintValue = (TxBodyContent {
                  txIns= txins,
                  txInsCollateral=TxInsCollateral CollateralInAlonzoEra  [  ],
                  txOuts= txouts,
                  txFee=TxFeeExplicit TxFeesExplicitInAlonzoEra $ Lovelace fee,
                  txValidityRange=(TxValidityNoLowerBound,TxValidityNoUpperBound ValidityNoUpperBoundInAlonzoEra),
                  txMetadata=TxMetadataNone ,
                  txAuxScripts=TxAuxScriptsNone,
                  txExtraScriptData=BuildTxWith TxExtraScriptDataNone ,
                  txExtraKeyWits=TxExtraKeyWitnessesNone,
                  txProtocolParams=BuildTxWith (Just  pParam),
                  txWithdrawals=TxWithdrawalsNone,
                  txCertificates=TxCertificatesNone,
                  txUpdateProposal=TxUpdateProposalNone,
                  txMintValue=mintValue,
                  txScriptValidity=TxScriptValidityNone
                })


-- balanceTxBody :: TxBodyContent BuildTx AlonzoEra -> (AddressInEra AlonzoEra -> Value -> Value -> (Hash ScriptData) -> [TxOut AlonzoEra])-> TxIns BuildTx AlonzoEra-> ProtocolParameters -> AddressInEra AlonzoEra -> Value -> Value-> Hash ScriptData -> Either SomeError (TxBody AlonzoEra)
-- balanceTxBody body composeTxOuts txIns pParam alonzoWalletAddr changeValue lockedValue scriptDataHash= 

--         case makeTransactionBody $ body of
--           Left tbe ->
--             Left (SomeError "Tx Body has error.")
--           -- Balance transaction body with fee
--           Right txBody -> do
--             let Lovelace transactionFee = evaluateTransactionFee pParam txBody 1 0
--                 changeValueWithFeeNegated = changeValue <> (negateValue (lovelaceToValue $ Lovelace transactionFee)) 
--                                             <> (negateValue (lovelaceToValue $ Lovelace 200))
--                 txoutsWithFeeNegated = composeTxOuts alonzoWalletAddr changeValueWithFeeNegated lockedValue scriptDataHash

--                 bodyWithFeeNegated = makeTxBodyContent txIns txoutsWithFeeNegated pParam (transactionFee+200)

--                         --End Balance transaction body with fee
--             case makeTransactionBody bodyWithFeeNegated of
--               Left tbe -> Left (SomeError "Tx Body has error.")
--               Right txBody -> Right txBody


signAndSubmitTransaction :: LocalNodeConnectInfo CardanoMode -> SigningKey PaymentKey -> TxBody AlonzoEra -> IO ()
signAndSubmitTransaction conn sKey balancedTxBody = do
  let tx = makeSignedTransaction [ makeShelleyKeyWitness balancedTxBody (WitnessPaymentKey sKey) ] (balancedTxBody) -- witness and txBody
  res <-submitTxToNodeLocal conn $  TxInMode tx AlonzoEraInCardanoMode
  case res of
    SubmitSuccess ->  do
      let v= getTxId balancedTxBody
      putStrLn "Transaction successfully submitted."
      putStrLn $ "TXId : "++ (show $ v)
    SubmitFail reason ->
      case reason of
        TxValidationErrorInMode err _eraInMode ->  print err
        TxValidationEraMismatch mismatchErr -> print mismatchErr
