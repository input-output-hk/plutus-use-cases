-- | Validation, on-chain code for governance application 
module Mlabs.Governance.Contract.Validation (
    scrAddress
  , inst
  , govValueOf
  ) where

import PlutusTx qualified
import PlutusTx.Prelude hiding (Semigroup(..), unless)
import Ledger hiding (singleton)
import Ledger.Typed.Scripts qualified as Scripts
import Plutus.V1.Ledger.Value (singleton, currencySymbol, tokenName)

{-# INLINABLE mkValidator #-}
mkValidator :: () -> () -> ScriptContext -> Bool
mkValidator _ _ _ = True -- todo

data Governance
instance Scripts.ScriptType Governance where
    type instance DatumType Governance = ()
    type instance RedeemerType Governance = ()
    
inst :: Scripts.ScriptInstance Governance
inst = Scripts.validator @Governance
  $$(PlutusTx.compile [|| mkValidator ||])
  $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @() @()

validator :: Validator
validator = Scripts.validatorScript inst

scrAddress :: Ledger.Address
scrAddress = scriptAddress validator

govValueOf :: Integer -> Value
govValueOf = singleton (currencySymbol "STAND_IN") (tokenName "GOV")
