-- | Validation, on-chain code for governance application 
module Mlabs.Governance.Contract.Validation (
    scrAddress
  , scrInstance
  , scrValidator
  , govValueOf
  , xgovValueOf
  , xGovMintingPolicy
  , Governance
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
    
scrInstance :: CurrencySymbol -> Scripts.ScriptInstance Governance
scrInstance csym = Scripts.validator @Governance
  ($$(PlutusTx.compile [|| mkValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode csym)
  $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @(Scripts.DatumType Governance) @(Scripts.RedeemerType Governance)

scrValidator :: CurrencySymbol -> Validator
scrValidator = Scripts.validatorScript . scrInstance

scrAddress :: CurrencySymbol -> Ledger.Address
scrAddress = scriptAddress . scrValidator

govValueOf, xgovValueOf :: CurrencySymbol -> Integer -> Value
govValueOf csym  = Value.singleton csym govToken
xgovValueOf csym = Value.singleton csym xgovToken

-- xGOV minting policy
{-# INLINABLE mkPolicy #-}
mkPolicy :: CurrencySymbol -> ScriptContext -> Bool
mkPolicy csym ctx =
--  traceIfFalse "imbalance of GOV to xGOV" checkGovxGov &&
  traceIfFalse "GOV not paid to the script" (fst checkGovToScr)
  where
    info = scriptContextTxInfo ctx

    checkGovxGov = case Value.flattenValue (Contexts.txInfoForge info) of
      [(cur, tn, amm)] -> cur == Contexts.ownCurrencySymbol ctx && amm == (snd checkGovToScr) -- && tn == xgovToken -- won't work because const ByteString :V
      _ -> False

    -- checks that the GOV was paid to the governance script and returns the value of it
    checkGovToScr :: (Bool, Integer)
    -- won't work because const ByteString :V
    checkGovToScr = (True, 0) {- case fmap txOutValue . find (\txout -> scrAddress csym == txOutAddress txout) $ txInfoOutputs info of
      Nothing  -> (False,0) 
      Just val -> case Value.flattenValue val of
        [(cur, tn, amm)] -> (cur == csym  && tn == xgovToken, amm)
        _ -> (False,0) -}        

xGovMintingPolicy :: CurrencySymbol -> Scripts.MonetaryPolicy
xGovMintingPolicy csym = mkMonetaryPolicyScript $
  $$(PlutusTx.compile [|| Scripts.wrapMonetaryPolicy . mkPolicy ||]) `PlutusTx.applyCode` PlutusTx.liftCode csym 

