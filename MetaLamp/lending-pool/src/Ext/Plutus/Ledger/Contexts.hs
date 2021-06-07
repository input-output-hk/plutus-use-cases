{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# LANGUAGE NamedFieldPuns    #-}

module Ext.Plutus.Ledger.Contexts where

import           Ledger                      (Address (Address),
                                              Datum (getDatum), DatumHash,
                                              PubKeyHash,
                                              TxInInfo (txInInfoResolved),
                                              TxInfo (txInfoInputs),
                                              TxOut (TxOut, txOutAddress, txOutValue),
                                              Value, findDatum)
import           Plutus.V1.Ledger.Credential (Credential (PubKeyCredential))
import qualified PlutusTx
import           PlutusTx.Prelude            (Eq ((==)), Maybe (..),
                                              Monad ((>>=)), find, fst,
                                              mapMaybe, mconcat, snd, ($), (.),
                                              (<$>))

{-# INLINABLE findDatumHashByValue #-}
-- | Find the hash of a datum, if it is part of the pending transaction's
--   outputs
findDatumHashByValue :: Value -> [(DatumHash, Value)] -> Maybe DatumHash
findDatumHashByValue val outs = fst <$> find f outs
  where
    f (_, val') = val' == val

{-# INLINABLE findValueByDatumHash #-}
-- | Find the hash of a datum, if it is part of the pending transaction's
--   outputs
findValueByDatumHash :: DatumHash -> [(DatumHash, Value)] -> Maybe Value
findValueByDatumHash dh outs = snd <$> find f outs
  where
    f (dh', _) = dh' == dh

{-# INLINABLE parseDatum #-}
parseDatum :: PlutusTx.IsData a => TxInfo -> DatumHash -> Maybe a
parseDatum txInfo dh = findDatum dh txInfo >>= (PlutusTx.fromData . getDatum)

{-# INLINABLE valueSpentFrom #-}
valueSpentFrom :: TxInfo -> PubKeyHash -> Value
valueSpentFrom txInfo pk =
  let flt TxOut {txOutAddress = Address (PubKeyCredential pk') _, txOutValue}
        | pk == pk' = Just txOutValue
      flt _ = Nothing
   in mconcat $ mapMaybe flt (txInInfoResolved <$> txInfoInputs txInfo)
