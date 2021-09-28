{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module Plutus.Contracts.Service.FungibleToken where

import           Ledger.Typed.Scripts      (MintingPolicy)
import qualified Ledger.Typed.Scripts      as Scripts
import           Plutus.V1.Ledger.Contexts (ScriptContext)
import qualified Plutus.V1.Ledger.Scripts  as Scripts
import           Plutus.V1.Ledger.Value    (TokenName)
import qualified PlutusTx
import           PlutusTx.Prelude

{-# INLINABLE validator #-}
validator :: TokenName -> BuiltinData -> ScriptContext -> Bool
validator _ _ _ = True

makeLiquidityPolicy :: TokenName -> MintingPolicy
makeLiquidityPolicy tokenName = Scripts.mkMintingPolicyScript $
  $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy . validator ||])
    `PlutusTx.applyCode`
        PlutusTx.liftCode tokenName
