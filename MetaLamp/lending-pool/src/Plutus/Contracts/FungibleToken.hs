{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}

module Plutus.Contracts.FungibleToken where

import           Ledger.Typed.Scripts      (MonetaryPolicy)
import qualified Ledger.Typed.Scripts      as Scripts
import           Plutus.V1.Ledger.Contexts (ScriptContext)
import qualified Plutus.V1.Ledger.Scripts  as Scripts
import           Plutus.V1.Ledger.Value    (TokenName, Value)
import qualified PlutusTx

validator :: TokenName -> ScriptContext -> Bool
validator _ _ = True

makeLiquidityPolicy :: TokenName -> MonetaryPolicy
makeLiquidityPolicy tokenName = Scripts.mkMonetaryPolicyScript $
  $$(PlutusTx.compile [|| Scripts.wrapMonetaryPolicy . validator ||])
    `PlutusTx.applyCode`
        PlutusTx.liftCode tokenName
