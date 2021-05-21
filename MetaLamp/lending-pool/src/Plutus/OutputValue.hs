{-# LANGUAGE DeriveFunctor #-}

module Plutus.OutputValue where

import           Ledger           (TxOutRef, TxOutTx)

import qualified PlutusTx.Prelude as PlutuxTx

data OutputValue a =
    OutputValue {
        ovOutRef :: TxOutRef,
        ovOutTx  :: TxOutTx,
        ovValue  :: a
    } deriving (Prelude.Show, Prelude.Functor)
