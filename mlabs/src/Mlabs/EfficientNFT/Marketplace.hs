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
  TxInfo (txInfoInputs, txInfoMint),
  TxOut (txOutAddress),
  Value,
  mkValidatorScript,
  ownHash,
  scriptContextTxInfo,
  scriptHashAddress,
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

    getSeabugs :: [TxOut] -> [(CurrencySymbol, TokenName)]
    getSeabugs =
      filter (\(cs, _) -> cs /= adaSymbol)
        . fmap (\(cs, tn, _) -> (cs, tn))
        . flattenValue
        . mconcat
        . fmap txOutValue

    inputs :: [TxOut]
    inputs =
      filter ((== scriptHashAddress (ownHash ctx)) . txOutAddress)
        . fmap txInInfoResolved
        . txInfoInputs
        $ info

    checkRemint :: Bool
    checkRemint =
      let checkRemintOne (cs, tn) = valueOf (txInfoMint info) cs tn == -1
       in all checkRemintOne (getSeabugs inputs)

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
