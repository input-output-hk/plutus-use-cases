{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}

module Plutus.AbstractTestnetMVP.OutputValue where

import           Control.Lens     (makeClassy_)
import           Ledger           (TxOutRef, ChainIndexTxOut)
import qualified PlutusTx.Prelude as PlutuxTx

data OutputValue a =
    OutputValue {
        ovOutRef :: TxOutRef,
        ovOutTx  :: ChainIndexTxOut,
        ovValue  :: a
    } deriving (Prelude.Show, Prelude.Functor)

makeClassy_ ''OutputValue
