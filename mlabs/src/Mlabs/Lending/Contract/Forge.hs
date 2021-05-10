module Mlabs.Lending.Contract.Forge(
    currencySymbol
  , currencyPolicy
) where

import PlutusTx.Prelude

import Ledger (CurrencySymbol)

import Ledger.Typed.Scripts (MonetaryPolicy)
import qualified Plutus.V1.Ledger.Scripts as Scripts
import qualified Ledger.Typed.Scripts     as Scripts
import qualified PlutusTx                 as PlutusTx
import Plutus.V1.Ledger.Contexts

{-# INLINABLE validate #-}
validate :: ScriptContext -> Bool
validate _ = True

currencyPolicy :: MonetaryPolicy
currencyPolicy = Scripts.mkMonetaryPolicyScript $
  $$(PlutusTx.compile [|| Scripts.wrapMonetaryPolicy validate ||])

currencySymbol :: CurrencySymbol
currencySymbol = scriptCurrencySymbol currencyPolicy

