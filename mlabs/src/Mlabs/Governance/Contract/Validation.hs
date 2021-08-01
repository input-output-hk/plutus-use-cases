-- | Validation, on-chain code for governance application 
module Mlabs.Governance.Contract.Validation (
    scrAddress
  , inst
  , govValueOf
  ) where

import PlutusTx qualified
import PlutusTx.Prelude hiding (Semigroup(..), unless)
import Ledger hiding (singleton)
import Ledger.Typed.Scripts      qualified as Scripts
import Plutus.V1.Ledger.Value    qualified as Value
import Plutus.V1.Ledger.Contexts qualified as Contexts

govToken, xgovToken :: TokenName
govToken = "GOV"
xgovToken = "xGOV"

-- Validator of the governance contract
{-# INLINABLE mkValidator #-}
mkValidator :: CurrencySymbol -> () -> () -> ScriptContext -> Bool
mkValidator cs _ _ _ = True -- todo

data Governance
instance Scripts.ScriptType Governance where
    type instance DatumType Governance = ()
    type instance RedeemerType Governance = ()
    
inst :: CurrencySymbol -> Scripts.ScriptInstance Governance
inst cs = Scripts.validator @Governance
  ($$(PlutusTx.compile [|| mkValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode cs)
  $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @() @()

validator :: CurrencySymbol -> Validator
validator = Scripts.validatorScript . inst

scrAddress :: CurrencySymbol -> Ledger.Address
scrAddress = scriptAddress . validator

govValueOf :: CurrencySymbol -> Integer -> Value
govValueOf csym = Value.singleton csym govToken

-- xGOV minting policy
{-# INLINABLE mkPolicy #-}
mkPolicy :: CurrencySymbol -> ScriptContext -> Bool
mkPolicy csym ctx = traceIfFalse "imbalance of GOV to xGOV" checkGovxGov
  -- some other checks needed? the script is in the transaction? tbd.
  where
    info = scriptContextTxInfo ctx

    checkGovxGov = case Value.flattenValue (Contexts.txInfoForge info) of
      [(cur, tn, val)] -> Contexts.ownCurrencySymbol ctx == cur && tn == xgovToken && val == spentGov
      _ -> False
    spentGov = Value.valueOf (valueSpent info) csym govToken
    
