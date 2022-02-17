{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Mlabs.EfficientNFT.Marketplace (mkValidator, marketplaceValidator) where

import PlutusTx qualified
import PlutusTx.Prelude

import Ledger (
  CurrencySymbol,
  ScriptContext,
  TokenName,
  TxInInfo (txInInfoResolved),
  TxInfo (txInfoMint),
  TxOut,
  Value,
  findOwnInput,
  mkValidatorScript,
  scriptContextTxInfo,
  txOutValue,
 )
import Ledger.Typed.Scripts (Any, TypedValidator, unsafeMkTypedValidator, wrapValidator)
import Ledger.Value (getValue, valueOf)
import Plutus.V1.Ledger.Ada (adaSymbol)
import PlutusTx.AssocMap qualified as AssocMap

-- | An escrow-like validator, that holds an NFT until sold or pulled out
{-# INLINEABLE mkValidator #-}
mkValidator :: BuiltinData -> BuiltinData -> ScriptContext -> Bool
mkValidator _ _ ctx =
  traceIfFalse "All spent tokens must be reminted" checkRemint
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    getSeabug :: TxOut -> (CurrencySymbol, TokenName)
    getSeabug =
      head
        . filter (\(cs, _) -> cs /= adaSymbol)
        . fmap (\(cs, tn, _) -> (cs, tn))
        . flattenValue
        . txOutValue

    input :: TxOut
    input =
      txInInfoResolved
        . fromMaybe (traceError "Missing own input")
        . findOwnInput
        $ ctx

    checkRemint :: Bool
    checkRemint =
      let checkBurned (cs, tn) = valueOf (txInfoMint info) cs tn == -1
       in checkBurned . getSeabug $ input

-- FIXME: For some reason plutus can't compile `flattenValue` from lib
--        so here is a hack (literally copy-pasted from lib)
{-# INLINEABLE flattenValue #-}

-- | Convert a value to a simple list, keeping only the non-zero amounts.
flattenValue :: Value -> [(CurrencySymbol, TokenName, Integer)]
flattenValue v = goOuter [] (AssocMap.toList $ getValue v)
  where
    goOuter acc [] = acc
    goOuter acc ((cs, m) : tl) = goOuter (goInner cs acc (AssocMap.toList m)) tl

    goInner _ acc [] = acc
    goInner cs acc ((tn, a) : tl)
      | a /= 0 = goInner cs ((cs, tn, a) : acc) tl
      | otherwise = goInner cs acc tl

marketplaceValidator :: TypedValidator Any
marketplaceValidator = unsafeMkTypedValidator v
  where
    v =
      mkValidatorScript
        ($$(PlutusTx.compile [||wrap||]) `PlutusTx.applyCode` $$(PlutusTx.compile [||mkValidator||]))
    wrap = wrapValidator
