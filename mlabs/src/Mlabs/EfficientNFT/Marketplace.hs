{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}

module Mlabs.EfficientNFT.Marketplace (mkValidator) where

import Ledger (
  ScriptContext,
  TxInInfo (txInInfoResolved),
  TxInfo (txInfoOutputs),
  findOwnInput,
  scriptContextTxInfo,
  txOutValue,
 )
import Ledger.Value qualified as Value
import PlutusTx.Prelude

-- | An escrow-like validator, that holds an NFT until sold or pulled out
{-# INLINEABLE mkValidator #-}
mkValidator :: BuiltinData -> ScriptContext -> Bool
mkValidator _ ctx =
  traceIfFalse "Tokens can only be redeemed when the policy allows a remint" checkRemint
  where
    !info = scriptContextTxInfo ctx
    -- Check if each token from the current input is reminted
    checkRemint =
      let !inputVals =
            maybe [] (Value.flattenValue . txOutValue . txInInfoResolved) $ findOwnInput ctx

          outputVals = Value.flattenValue $ mconcat $ map txOutValue $ txInfoOutputs info
       in all
            ( \(inCS, inTN, _) ->
                maybe
                  False
                  (\(_, outTN, _) -> outTN /= inTN)
                  (find (\(outCS, _, _) -> inCS == outCS) outputVals)
            )
            inputVals
