{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE MonoLocalBinds     #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE ViewPatterns       #-}
{-# LANGUAGE TypeFamilies #-}
module Plutus.Contract.Blockchain.Nft

where
import PlutusTx.Prelude
import           Ledger.Contexts          (ScriptContext (..), TxInfo (..), TxOutRef)
import Ledger
import Ledger.Value
import PlutusTx ( applyCode, liftCode, compile )
import Ledger.Typed.Scripts (wrapMintingPolicy)

{-# INLINABLE mkPolicy #-}
mkPolicy :: TxOutRef -> TokenName ->() -> ScriptContext -> Bool
mkPolicy oref tn _ ctx@ScriptContext {scriptContextTxInfo=info@TxInfo{}} =
    traceIfFalse "UTxO not consumed"   hasUTxO           &&
    traceIfFalse "wrong amount minted" checkMintedAmount
    where

    hasUTxO :: Bool
    hasUTxO = any (\i -> txInInfoOutRef i == oref) $ txInfoInputs info

    checkMintedAmount :: Bool
    checkMintedAmount = case flattenValue (txInfoMint  info) of
        [(cs, tn', amt)] -> cs  == ownCurrencySymbol ctx && tn' == tn && amt == 1
        _                -> False

-- policy :: TxOutRef -> TokenName -> MintingPolicy 
-- policy oref tn = mkMintingPolicyScript $
--     $$(PlutusTx.compile [|| \oref' tn' -> wrapMintingPolicy (mkPolicy oref' tn') ||])
--     `PlutusTx.applyCode`
--     PlutusTx.liftCode oref
--     `PlutusTx.applyCode`
--     PlutusTx.liftCode tn

policy::TxOutRef -> TokenName -> MintingPolicy 
policy oref tn=mkMintingPolicyScript   $$(PlutusTx.compile [||a ||])
  where
    a _ _=()


curSymbol :: TxOutRef -> TokenName -> CurrencySymbol
curSymbol oref tn = scriptCurrencySymbol $ policy oref tn
