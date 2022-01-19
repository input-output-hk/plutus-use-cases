{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Mlabs.EfficientNFT.Burn (mkValidator, burnValidator) where

import PlutusTx qualified
import PlutusTx.Prelude

import Ledger (ScriptContext, mkValidatorScript)
import Ledger.Typed.Scripts (Any, TypedValidator, unsafeMkTypedValidator, wrapValidator)

-- TODO: Add utxo merging
{-# INLINEABLE mkValidator #-}
mkValidator :: BuiltinData -> BuiltinData -> ScriptContext -> Bool
mkValidator _ _ _ = False

burnValidator :: TypedValidator Any
burnValidator = unsafeMkTypedValidator v
  where
    v =
      mkValidatorScript
        ($$(PlutusTx.compile [||wrap||]) `PlutusTx.applyCode` $$(PlutusTx.compile [||mkValidator||]))
    wrap = wrapValidator @BuiltinData @BuiltinData
