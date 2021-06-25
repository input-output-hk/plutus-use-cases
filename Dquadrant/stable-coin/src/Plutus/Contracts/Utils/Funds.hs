{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}

module Plutus.Contracts.Utils.Funds
where

import Data.Map ( elems )
import Plutus.Contract

import Data.Text ( Text )
import Ledger ( pubKeyAddress, TxOut (txOutValue), TxOutTx (txOutTxOut, txOutTxTx), txOutTxDatum, TxOutRef, Address, DatumHash, Datum(..), Tx (txData), txOutDatum)
import Ledger.Value ( Value, flattenValue )
import Data.Monoid
import Control.Monad (void)
import qualified Data.Aeson.Types as Types
import qualified Data.Map as Map
import Data.Aeson (toJSON)
import Ledger.AddressMap
import PlutusTx
import Playground.Contract
import Data.Maybe ( isJust, fromJust, catMaybes, mapMaybe )
import Data.Functor ((<&>))
import Control.Lens (review)
import Plutus.Contract.Types
import Plutus.Contract.Constraints (MkTxError(TxOutRefNotFound, TxOutRefWrongType))


-- ownFunds'':: HasBlockchainActions s => Contract [Types.Value ] s Text  Types.Value
-- ownFunds'' = do
--     pk    <- ownPubKey
--     utxos <- utxoAt $ pubKeyAddress pk
--     let v = mconcat $ Map.elems $ txOutValue . txOutTxOut Prelude.<$> utxos
--     logInfo @String $ "own funds: " ++ show (flattenValue v)
--     tell [ toJSON v]
--     return $ toJSON  v

-- type UtilSchema=
--     BlockchainActions 
--     .\/  Endpoint "funds" String

-- utilEndpoints :: HasBlockchainActions s => Contract [Types.Value] s Text ()
-- utilEndpoints= handleError (\e ->logError e) $ void fundsEp 

-- fundsEp :: HasBlockchainActions s => Contract [Types.Value] s Text Types.Value
-- fundsEp=  endpoint @"funds" >> ownFunds''
