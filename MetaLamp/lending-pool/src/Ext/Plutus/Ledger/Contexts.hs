{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# LANGUAGE NamedFieldPuns    #-}

module Ext.Plutus.Ledger.Contexts where

import           Ledger                      (Address (Address), Datum (..),
                                              DatumHash, PubKeyHash,
                                              ScriptContext,
                                              TxInInfo (txInInfoResolved),
                                              TxInfo (txInfoInputs),
                                              TxOut (TxOut, txOutAddress, txOutDatumHash, txOutValue),
                                              ValidatorHash, Value, findDatum,
                                              findDatumHash, ownHashes,
                                              scriptContextTxInfo,
                                              scriptOutputsAt)
import           Plutus.V1.Ledger.Contexts   (ScriptContext)
import           Plutus.V1.Ledger.Credential (Credential (PubKeyCredential, ScriptCredential))
import qualified PlutusTx
import           PlutusTx.Prelude            (Eq ((==)), Maybe (..), filter,
                                              find, fst, mapMaybe, mconcat,
                                              otherwise, snd, ($), (.), (<$>),
                                              (>>=))

{-# INLINABLE findOnlyOneDatumByValue #-}
findOnlyOneDatumByValue :: PlutusTx.IsData a => ScriptContext -> Value -> Maybe a
findOnlyOneDatumByValue ctx value = findOnlyOneDatumHashByValue value (getScriptOutputs ctx) >>= parseDatum (scriptContextTxInfo ctx)

{-# INLINABLE findOnlyOneDatumHashByValue #-}
-- | Find the hash of a datum, if it is part of the script's outputs.
--   Assume search failed if more than one correspondence is found.
findOnlyOneDatumHashByValue :: Value -> [(DatumHash, Value)] -> Maybe DatumHash
findOnlyOneDatumHashByValue val outs = fst <$> case filter f outs of
  [res] -> Just res
  _     -> Nothing
  where
    f (_, val') = val' == val

{-# INLINABLE getScriptOutputs #-}
getScriptOutputs :: ScriptContext -> [(DatumHash, Value)]
getScriptOutputs ctx = scriptOutputsAt scriptsHash (scriptContextTxInfo ctx)
  where
    (scriptsHash, _) = ownHashes ctx

{-# INLINABLE findValueByDatum #-}
findValueByDatum :: PlutusTx.IsData a => ScriptContext -> a -> Maybe Value
findValueByDatum ctx datum = (`findValueByDatumHash` scriptOutputs) <$> findDatumHash (Datum $ PlutusTx.toData datum) txInfo
  where
    txInfo = scriptContextTxInfo ctx
    scriptOutputs = getScriptOutputs ctx

{-# INLINABLE findInputValueByDatum #-}
findInputValueByDatum :: PlutusTx.IsData a => ScriptContext -> a -> Maybe Value
findInputValueByDatum ctx datum = (`findValueByDatumHash` scriptOutputs) <$> findDatumHash (Datum $ PlutusTx.toData datum) txInfo
  where
    txInfo = scriptContextTxInfo ctx
    (validatorHash, _) = ownHashes ctx
    scriptOutputs = scriptInputsAt validatorHash txInfo

{-# INLINABLE findValueByDatumHash #-}
-- | Concat value of the script's outputs that have the specified hash of a datum
findValueByDatumHash :: DatumHash -> [(DatumHash, Value)] -> Value
findValueByDatumHash dh outs = mconcat $ mapMaybe f outs
  where
    f (dh', val) | dh' == dh = Just val
                 | otherwise = Nothing

{-# INLINABLE parseDatum #-}
-- | Find datum inside pending transaction and parse it from data
parseDatum :: PlutusTx.IsData a => TxInfo -> DatumHash -> Maybe a
parseDatum txInfo dh = findDatum dh txInfo >>= (PlutusTx.fromData . getDatum)

{-# INLINABLE valueSpentFrom #-}
-- | Concat value of the inputs belonging to the provided public key inside the pending transaction's inputs
valueSpentFrom :: TxInfo -> PubKeyHash -> Value
valueSpentFrom txInfo pk =
  let flt TxOut {txOutAddress = Address (PubKeyCredential pk') _, txOutValue}
        | pk == pk' = Just txOutValue
      flt _ = Nothing
   in mconcat $ mapMaybe flt (txInInfoResolved <$> txInfoInputs txInfo)

{-# INLINABLE scriptInputsAt #-}
-- | Finds all inputs belonging to a specific script inside the pending transaction's inputs
scriptInputsAt :: ValidatorHash -> TxInfo -> [(DatumHash, Value)]
scriptInputsAt h p =
    let flt TxOut{txOutDatumHash=Just ds, txOutAddress=Address (ScriptCredential s) _, txOutValue} | s == h = Just (ds, txOutValue)
        flt _ = Nothing
    in mapMaybe flt (txInInfoResolved <$> txInfoInputs p)
