module Ext.Plutus.Ledger.Value where

import           Control.Lens           (view)
import qualified Data.Map               as Map
import           Data.Text              (Text)
import           Ledger                 (Address, TxOut (txOutValue), TxOutRef,
                                         TxOutTx (txOutTxOut), Value)
import           Ledger.AddressMap      (UtxoMap)
import           Ledger.Tx              (ChainIndexTxOut, ciTxOutValue, toTxOut)
import           Plutus.Contract
import           Plutus.V1.Ledger.Value (Value)

type ChainIndexTxMap = Map.Map TxOutRef ChainIndexTxOut

utxosValue :: Address -> Contract w s Text Value
utxosValue address = do
    os  <- map snd . Map.toList <$> utxosAt address
    return $ mconcat [view ciTxOutValue o | o <- os]
