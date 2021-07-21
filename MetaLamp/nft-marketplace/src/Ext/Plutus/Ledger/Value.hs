module Ext.Plutus.Ledger.Value where

import qualified Data.Map               as Map
import           Ledger                 (TxOut (txOutValue),
                                         TxOutTx (txOutTxOut), Value)
import           Ledger.AddressMap      (UtxoMap)
import           Plutus.V1.Ledger.Value (Value)

utxoValue :: UtxoMap -> Value
utxoValue = foldMap (txOutValue . txOutTxOut . snd) . Map.toList
