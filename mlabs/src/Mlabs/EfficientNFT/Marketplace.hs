module Mlabs.EfficientNFT.Marketplace (mkValidator, marketplaceValidator) where

import PlutusTx qualified
import PlutusTx.Prelude

import Ledger (
  ScriptContext,
  TxInfo (txInfoMint),
  mkValidatorScript,
  scriptContextTxInfo,
 )
import Ledger.Typed.Scripts (Any, TypedValidator, unsafeMkTypedValidator, wrapValidator)
import Ledger.Value (assetClassValueOf)

import Mlabs.EfficientNFT.Types

-- | An escrow-like validator, that holds an NFT until sold or pulled out
{-# INLINEABLE mkValidator #-}
mkValidator :: MarketplaceDatum -> BuiltinData -> ScriptContext -> Bool
mkValidator datum _ ctx =
  traceIfFalse "All spent tokens must be reminted" checkRemint
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    checkRemint :: Bool
    checkRemint = assetClassValueOf (txInfoMint info) (getMarketplaceDatum datum) == -1

marketplaceValidator :: TypedValidator Any
marketplaceValidator = unsafeMkTypedValidator v
  where
    v =
      mkValidatorScript
        ($$(PlutusTx.compile [||wrap||]) `PlutusTx.applyCode` $$(PlutusTx.compile [||mkValidator||]))
    wrap = wrapValidator
