{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Mlabs.EfficientNFT.Marketplace (mkValidator) where

import Ledger (
  CurrencySymbol,
  ScriptContext,
  TxInInfo (txInInfoResolved),
  TxInfo (txInfoMint),
  findOwnInput,
  scriptContextTxInfo,
  txOutValue,
 )
import Ledger.Value qualified as Value
import PlutusTx.Prelude

-- | An escrow-like validator, that holds an NFT until sold or pulled out
{-# INLINEABLE mkValidator #-}
mkValidator :: CurrencySymbol -> BuiltinData -> ScriptContext -> Bool
mkValidator nftCS _ ctx =
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
