{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# LANGUAGE NamedFieldPuns    #-}

module Ext.Plutus.Ledger.Contexts where

import           Ledger                      (Address (Address),
                                              Datum (getDatum), DatumHash,
                                              PubKeyHash,
                                              TxInInfo (txInInfoResolved),
                                              TxInfo (txInfoInputs),
                                              TxOut (TxOut, txOutAddress, txOutDatumHash, txOutValue),
                                              ValidatorHash, Value, findDatum)
import           Plutus.V1.Ledger.Credential (Credential (PubKeyCredential, ScriptCredential))
import qualified PlutusTx
import           PlutusTx.Prelude            (Eq ((==)), Maybe (..), filter,
                                              find, fst, mapMaybe, mconcat,
                                              otherwise, snd, ($), (.), (<$>),
                                              (>>=))

{-# INLINABLE findOnlyOneDatumHashByValue #-}
-- | Find the hash of a datum, if it is part of the script's outputs.
--   Assume search failed if more than one correspondence is found.
findOnlyOneDatumHashByValue :: Value -> [(DatumHash, Value)] -> Maybe DatumHash
findOnlyOneDatumHashByValue val outs = fst <$> case filter f outs of
  [res] -> Just res
  _     -> Nothing
  where
    f (_, val') = val' == val

{-# INLINABLE findValueByDatumHash #-}
-- | Concat value of the script's outputs that have the specified hash of a datum
findValueByDatumHash :: DatumHash -> [(DatumHash, Value)] -> Value
findValueByDatumHash dh outs = mconcat $ mapMaybe f outs
  where
    f (dh', val) | dh' == dh = Just val
                 | otherwise = Nothing

{-# INLINABLE parseDatum #-}
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
