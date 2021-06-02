{-# OPTIONS_GHC -fno-specialize #-}
{-# OPTIONS_GHC -fno-specialize #-}
module Mlabs.Lending.Contract.Utils where

import Prelude (Maybe(..), ($))
import           Ledger               hiding (singleton)
import PlutusTx

-- | For off-chain code
readDatum :: IsData a => TxOutTx -> Maybe a
readDatum txOut = do
  h <- txOutDatumHash $ txOutTxOut txOut
  Datum e <- lookupDatum (txOutTxTx txOut) h
  PlutusTx.fromData e


