{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Mlabs.EfficientNFT.Marketplace (mkValidator, marketplaceValidator) where

import PlutusTx qualified
import PlutusTx.Prelude

import Ledger (
  ScriptContext,
  TxInInfo (txInInfoResolved),
  TxInfo (txInfoInputs, txInfoMint),
  TxOut (txOutAddress),
  getContinuingOutputs,
  mkValidatorScript,
  ownHash,
  scriptContextTxInfo,
  scriptHashAddress,
  txOutValue,
  txSignedBy,
  unPaymentPubKeyHash,
 )
import Ledger.Typed.Scripts (Any, TypedValidator, unsafeMkTypedValidator, wrapValidator)
import Ledger.Value (getValue, valueOf)
import Mlabs.EfficientNFT.Token (mkTokenName)
import Mlabs.EfficientNFT.Types (MarketplaceAct (Redeem, Update), nftId'owner)
import Plutus.V1.Ledger.Ada (adaSymbol)
import PlutusTx.AssocMap qualified as AssocMap
import PlutusTx.Bifunctor (second)

-- | An escrow-like validator, that holds an NFT until sold or pulled out
{-# INLINEABLE mkValidator #-}
mkValidator :: BuiltinData -> MarketplaceAct -> ScriptContext -> Bool
mkValidator _ act ctx =
  case act of
    Redeem nft ->
      traceIfFalse "Must be signed by the owner" (checkSignedByOwner nft)
    Update ->
      traceIfFalse "Token can only be updated when the policy allows a remint" checkRemint
  where
    (nftCS, nftTN) =
      let vals =
            fmap (second AssocMap.toList)
              . filter ((/= adaSymbol) . fst)
              . AssocMap.toList
              . getValue
              . txOutValue
              $ inTx
       in case vals of
            [(cs, [(tn, _)])] -> (cs, tn)
            _ -> error ()

    info = scriptContextTxInfo ctx

    inTx :: TxOut
    !inTx =
      ( \case
          [] -> traceError "Unreachable: No input"
          [tx] -> tx
          _ -> traceError "More than one input"
      )
        . filter ((== scriptHashAddress (ownHash ctx)) . txOutAddress)
        . fmap txInInfoResolved
        . txInfoInputs
        $ info

    outTx :: TxOut
    outTx =
      ( \case
          [] -> traceError "No CO"
          [tx] -> tx
          _ -> traceError "More than one CO"
      )
        . getContinuingOutputs
        $ ctx

    -- Check if owner of nft sign transaction
    checkSignedByOwner nft =
      let isSigned = txSignedBy info . unPaymentPubKeyHash . nftId'owner $ nft
          namesMatch = nftTN == mkTokenName nft
       in isSigned && namesMatch

    -- Check if token from the current input is reminted
    checkRemint =
      let getSg =
            head
              . AssocMap.keys
              . fromMaybe (traceError "Mising Sg in output")
              . AssocMap.lookup nftCS
              . getValue
              . txOutValue
          oldTN = getSg inTx
          burnOld = valueOf (txInfoMint info) nftCS oldTN == -1
          newTN = getSg outTx
          mintNew = valueOf (txInfoMint info) nftCS newTN == 1
       in burnOld && mintNew

marketplaceValidator :: TypedValidator Any
marketplaceValidator = unsafeMkTypedValidator v
  where
    v =
      mkValidatorScript
        ($$(PlutusTx.compile [||wrap||]) `PlutusTx.applyCode` $$(PlutusTx.compile [||mkValidator||]))
    wrap = wrapValidator
