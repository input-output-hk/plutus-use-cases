{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Mlabs.EfficientNFT.Marketplace (mkValidator, marketplaceValidator) where

import PlutusTx qualified
import PlutusTx.Prelude

import Ledger (
  CurrencySymbol,
  ScriptContext,
  TxInInfo (txInInfoResolved),
  TxInfo (txInfoMint),
  findOwnInput,
  mkValidatorScript,
  scriptContextTxInfo,
  txOutValue,
 )
import Ledger.Typed.Scripts (Any, TypedValidator, unsafeMkTypedValidator, wrapValidator)
import Ledger.Value qualified as Value

-- | An escrow-like validator, that holds an NFT until sold or pulled out
{-# INLINEABLE mkValidator #-}
mkValidator :: CurrencySymbol -> BuiltinData -> BuiltinData -> ScriptContext -> Bool
mkValidator nftCS _ _ ctx =
  traceIfFalse "Tokens can only be redeemed when the policy allows a remint" checkRemint
    && traceIfFalse "Inputs with more than one token are invalid" checkInputUTxO
  where
    !info = scriptContextTxInfo ctx
    !inputVals =
      maybe [] (Value.flattenValue . txOutValue . txInInfoResolved) $ findOwnInput ctx

    -- Check if the input UTxO contains a token and some Ada
    checkInputUTxO =
      length inputVals == 2

    -- Check if each token from the current input is reminted
    checkRemint =
      case filter (\(cs, _, _) -> cs == nftCS) $ Value.flattenValue $ txInfoMint info of
        [(_, tn, amt), (_, tn', amt')] -> tn /= tn' && amt + amt' == 0
        _ -> False

-- FIXME: Remove when proper validator is fixed
mkValidator' :: BuiltinData -> BuiltinData -> ScriptContext -> Bool
mkValidator' _ _ _ = True

marketplaceValidator :: TypedValidator Any
marketplaceValidator = unsafeMkTypedValidator v
  where
    v =
      mkValidatorScript
        ($$(PlutusTx.compile [||wrap||]) `PlutusTx.applyCode` $$(PlutusTx.compile [||mkValidator'||]))
    wrap = wrapValidator
