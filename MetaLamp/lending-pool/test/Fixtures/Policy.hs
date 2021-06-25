{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}

module Fixtures.Policy where

import           Ledger.Typed.Scripts      (MonetaryPolicy)
import qualified Ledger.Typed.Scripts      as Scripts
import           Plutus.V1.Ledger.Contexts (ScriptContext)
import qualified Plutus.V1.Ledger.Scripts  as Scripts
import           Plutus.V1.Ledger.Value    (TokenName)
import qualified PlutusTx

{-# INLINABLE validator #-}
validator :: TokenName -> ScriptContext -> Bool
validator _ _ = True

makePolicy :: TokenName -> MonetaryPolicy
makePolicy tokenName = Scripts.mkMonetaryPolicyScript $
  $$(PlutusTx.compile [|| Scripts.wrapMonetaryPolicy . validator ||])
    `PlutusTx.applyCode`
        PlutusTx.liftCode tokenName
