module Mlabs.EfficientNFT.Dao (mkValidator, daoValidator) where

import Ledger (txSignedBy)
import Ledger.Typed.Scripts (Any, TypedValidator, unsafeMkTypedValidator, wrapValidator)
import Plutus.V1.Ledger.Api (PubKeyHash, ScriptContext, mkValidatorScript, scriptContextTxInfo)
import PlutusTx qualified
import PlutusTx.Prelude

mkValidator :: [PubKeyHash] -> BuiltinData -> BuiltinData -> ScriptContext -> Bool
mkValidator pkhs _ _ ctx = traceIfFalse "Must be sighned by key from list" checkSigned
  where
    checkSigned :: Bool
    checkSigned = any (txSignedBy (scriptContextTxInfo ctx)) pkhs

daoValidator :: [PubKeyHash] -> TypedValidator Any
daoValidator pkhs = unsafeMkTypedValidator v
  where
    v =
      mkValidatorScript
        ( $$(PlutusTx.compile [||wrap||])
            `PlutusTx.applyCode` ( $$(PlutusTx.compile [||mkValidator||])
                                    `PlutusTx.applyCode` PlutusTx.liftCode pkhs
                                 )
        )
    wrap = wrapValidator
