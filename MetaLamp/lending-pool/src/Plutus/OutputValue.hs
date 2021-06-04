{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}

module Plutus.OutputValue where

import           Control.Lens     (makeClassy_)
import           Ledger           (TxOutRef, TxOutTx)
import qualified PlutusTx.Prelude as PlutuxTx

data OutputValue a =
    OutputValue {
        ovOutRef :: TxOutRef,
        ovOutTx  :: TxOutTx,
        ovValue  :: a
    } deriving (Prelude.Show, Prelude.Functor)

makeClassy_ ''OutputValue
