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
import qualified Ledger.Value                                             as V

type ChainIndexTxMap = Map.Map TxOutRef ChainIndexTxOut

utxosValue :: Address -> Contract w s Text Value
utxosValue address = do
    os  <- map snd . Map.toList <$> utxosAt address
    return $ mconcat [view ciTxOutValue o | o <- os]

isNftInValue :: Value -> V.CurrencySymbol -> V.TokenName -> Bool
isNftInValue value currency tokenName =
    length suitableTokensList == 1
    where
        suitableTokensList = filter isSuitableToken $ V.flattenValue value
        isSuitableToken (cs,tn,num) = cs == currency && tn == tokenName && num == 1

