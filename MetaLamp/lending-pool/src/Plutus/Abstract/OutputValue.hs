{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE TemplateHaskell        #-}

module Plutus.Abstract.OutputValue where

import           Control.Lens     (makeClassy_)
import           Ledger           (TxOutRef, TxOutTx, Value)
import           Ledger.Tx        (txOutTxOut, txOutValue)
import qualified PlutusTx.Prelude as PlutuxTx

data OutputValue a =
    OutputValue {
        ovOutRef :: TxOutRef,
        ovOutTx  :: TxOutTx,
        ovValue  :: a
    } deriving (Prelude.Show, Prelude.Functor)

makeClassy_ ''OutputValue

getOutputValue :: OutputValue a -> Value
getOutputValue OutputValue {..} = txOutValue . txOutTxOut $ ovOutTx
