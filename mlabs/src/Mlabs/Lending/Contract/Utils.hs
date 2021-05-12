{-# OPTIONS_GHC -fno-specialize #-}
{-# OPTIONS_GHC -fno-specialize #-}
module Mlabs.Lending.Contract.Utils where

import Prelude (Maybe(..), ($))

import           PlutusTx.Prelude ((.), error)
import qualified PlutusTx.Prelude     as Plutus
import           Ledger               hiding (singleton)

import PlutusTx

{-# INLINABLE valueWithin #-}
valueWithin :: TxInInfo -> Value
valueWithin = txOutValue . txInInfoResolved

{-# INLINABLE findOwnInput' #-}
findOwnInput' :: ScriptContext -> TxInInfo
findOwnInput' ctx = Plutus.fromMaybe (error ()) (findOwnInput ctx)

-- | For off-chain code
readDatum :: IsData a => TxOutTx -> Maybe a
readDatum txOut = do
  h <- txOutDatumHash $ txOutTxOut txOut
  Datum e <- lookupDatum (txOutTxTx txOut) h
  PlutusTx.fromData e

