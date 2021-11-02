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

-- Value (Map [(,Map [("",99996107)]),(50b69b375c08e6b43a5deca05e9f10214d86f9f84745594a26b4725e,Map [("QmPeoJnaDttpFrSySYBY3reRFCzL3qv4Uiqz376EBv9W16",1)])])
-- [(CurrencySymbol, TokenName, Integer)]
-- Value { getValue :: Map.Map CurrencySymbol (Map.Map TokenName Integer) }

findNftCurrencyByTokenName :: Value -> V.TokenName -> Maybe V.CurrencySymbol
findNftCurrencyByTokenName value tokenName =
    getCurrency $ V.flattenValue value
    where
        getCurrency [] = Nothing
        getCurrency ((cs,tn,1):items) = 
            if tn == tokenName 
                then Just cs
                else getCurrency items
        getCurrency (_:items) = getCurrency items
