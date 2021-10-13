module Ext.Plutus.Ledger.Value where

import qualified Data.Map               as Map
import           Ledger                 (TxOutRef, TxOut (txOutValue),
                                         TxOutTx (txOutTxOut), Value, Address)
import           Ledger.AddressMap      (UtxoMap)
import           Plutus.V1.Ledger.Value (Value)
import Ledger.Tx (toTxOut)
import           Ledger.Tx                   (ChainIndexTxOut, ciTxOutValue)
import           Plutus.Contract
import           Data.Text                                    (Text)
import           Control.Lens                     (view)

type ChainIndexTxMap = Map.Map TxOutRef ChainIndexTxOut

utxosValue :: Address -> Contract w s Text Value
utxosValue address = do
    os  <- map snd . Map.toList <$> utxosAt address
    return $ mconcat [view ciTxOutValue o | o <- os]