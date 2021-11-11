module Mlabs.NFT.Epoch.Validation(
  epochMintPolicy,
  epochScript,
  )
where

import PlutusTx.Prelude 
import Mlabs.NFT.Epoch.Types
import Ledger
import PlutusTx qualified
import Ledger.Typed.Scripts

data EpochManage
instance ValidatorTypes EpochManage where
  type DatumType EpochManage    = EpochDatum
  type RedeemerType EpochManage = EpochAct

{-# INLINEABLE mkEpochScript #-}
mkEpochScript :: UniqueAppToken -> EpochDatum -> EpochAct -> ScriptContext -> Bool
mkEpochScript !_ !_ !act !_ =
  case act of
   EpochInit ->
     traceIfFalse "The uAP must be paid to the Address, together with the Head Epoch Token" True
   EpochCreateFirstEpoch ->
     traceIfFalse "The Head Epoch Token must be present in the transaction, and must be paid back." True
     && traceIfFalse "The Head Token Must be Active -- Not Pointing to Any other Token." True 
     && traceIfFalse "The Created Token must have the correct type of Datum." True
   EpochNext ->
     traceIfFalse "The Epoch Votes List is consumed" True 

{-# INLINEABLE epochScript #-}
epochScript :: UniqueAppToken -> TypedValidator EpochManage
epochScript uAT = mkTypedValidator @EpochManage
    ($$(PlutusTx.compile [|| mkEpochScript ||]) `PlutusTx.applyCode` PlutusTx.liftCode  uAT)
     $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = wrapValidator @EpochDatum @EpochAct

{-# INLINEABLE mkEpochMintPolicy #-}
-- | Minting policy for Epoch tokens.
mkEpochMintPolicy :: UniqueAppToken -> EpochAddress -> EpochAct -> ScriptContext -> Bool
mkEpochMintPolicy !_ !_ !act !_ =
  case act of
    EpochInit ->
       traceIfFalse "The uAP must be paid to the Address, together with the Head Epoch Token" True
    EpochCreateFirstEpoch ->
        traceIfFalse "The Head Epoch Token must be present in the transaction, and must be paid back." True
        && traceIfFalse "The Head Token Must be Active -- Not Pointing to Any other Token." True 
        && traceIfFalse "The Created Token must have the correct type of Datum." True
    EpochNext ->
        traceIfFalse "The Epoch Votes List is consumed" True 

{-# INLINEABLE epochMintPolicy #-}
-- | Epoch Minting Policy
epochMintPolicy ::  UniqueAppToken -> EpochAddress -> MintingPolicy
epochMintPolicy x y =
  mkMintingPolicyScript $
    $$(PlutusTx.compile [||\x' y' -> wrapMintingPolicy (mkEpochMintPolicy x' y') ||])
      `PlutusTx.applyCode` PlutusTx.liftCode x
      `PlutusTx.applyCode` PlutusTx.liftCode y

