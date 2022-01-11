{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}

module Mlabs.EfficientNFT.Marketplace (mkValidator) where

import Data.Map qualified as Map
import Ledger (
  ScriptContext,
  TxInInfo (txInInfoResolved),
  TxInfo (txInfoInputs, txInfoOutputs),
  TxOut (TxOut),
  ownHash,
  scriptContextTxInfo,
  scriptHashAddress,
  txOutValue,
  txSignedBy,
 )
import Ledger.Value qualified as Value
import Mlabs.EfficientNFT.Token (OwnerData (OwnerData), mkTokenName')
import PlutusTx.Prelude

-- | An escrow-like validator, that holds an NFT until sold or pulled out
mkValidator :: BuiltinData -> [OwnerData] -> ScriptContext -> Bool
mkValidator _ ownerData ctx =
  traceIfFalse "Only the owner can redeem tokens" checkSignedByOwner
  where
    !info = scriptContextTxInfo ctx
    -- Check if the transaction is signed by the owner of the NFT
    checkSignedByOwner =
      let inputs =
            filter
              (\(TxOut addr _ _) -> addr == scriptHashAddress (ownHash ctx))
              $ map txInInfoResolved $ txInfoInputs info
          inputCurSymbols =
            map (\(cs, _, _) -> cs) $ Value.flattenValue $ mconcat $ map txOutValue inputs

          matchingOutputVals =
            filter (\(cs, _, _) -> cs `elem` inputCurSymbols) $
              Value.flattenValue $ mconcat $ map txOutValue $ txInfoOutputs info
          tokenNameMap = Map.fromList $ zip (map mkTokenName' ownerData) ownerData
       in all
            ( \(_, tn, _) ->
                case Map.lookup tn tokenNameMap of
                  Nothing -> False
                  Just (OwnerData pkh _) -> txSignedBy info pkh
            )
            matchingOutputVals
