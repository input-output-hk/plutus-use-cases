module Mlabs.EfficientNFT.Dao (mkValidator, daoValidator) where

import Ledger.Typed.Scripts (Any, TypedValidator, unsafeMkTypedValidator, wrapValidator)
import Plutus.V1.Ledger.Api (ScriptContext, mkValidatorScript)
import PlutusTx qualified
import PlutusTx.Prelude

mkValidator :: BuiltinData -> BuiltinData -> ScriptContext -> Bool
mkValidator _ _ _ = True

daoValidator :: TypedValidator Any
daoValidator = unsafeMkTypedValidator v
  where
    v =
      mkValidatorScript
        ($$(PlutusTx.compile [||wrap||]) `PlutusTx.applyCode` $$(PlutusTx.compile [||mkValidator||]))
    wrap = wrapValidator
