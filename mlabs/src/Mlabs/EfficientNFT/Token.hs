{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}

module Mlabs.EfficientNFT.Token (
  mkPolicy,
  policy,
  mkTokenName,
) where

import Ledger (
  MintingPolicy,
  ScriptContext,
  TxInInfo (txInInfoOutRef, txInInfoResolved),
  TxInfo (txInfoData, txInfoInputs, txInfoMint, txInfoOutputs),
  TxOut (TxOut, txOutValue),
  TxOutRef,
  ownCurrencySymbol,
  pubKeyHashAddress,
  scriptContextTxInfo,
  txSignedBy,
 )
import Ledger.Ada qualified as Ada
import Ledger.Crypto (PubKeyHash (PubKeyHash))
import Ledger.Scripts qualified as Scripts
import Ledger.Typed.Scripts (wrapMintingPolicy)
import Ledger.Value (TokenName (TokenName))
import Ledger.Value qualified as Value
import PlutusTx qualified
import PlutusTx.Natural (Natural)

import PlutusTx.Prelude

import Mlabs.EfficientNFT.Types

-- todo: docs
{-# INLINEABLE mkPolicy #-}
mkPolicy ::
  TxOutRef ->
  PubKeyHash ->
  Natural ->
  PlatformConfig ->
  ContentHash ->
  MintAct ->
  ScriptContext ->
  Bool
mkPolicy oref authorPkh royalty platformConfig _ mintAct ctx =
  case mintAct of
    MintToken (OwnerData ownerPkh price) ->
      traceIfFalse "UTXo specified as the parameter must be consumed" checkConsumedUtxo
        && traceIfFalse "Exactly one NFT must be minted" checkMintedAmount
        -- && traceIfFalse "Owner must sign the transaction" (txSignedBy info ownerPkh)
        && traceIfFalse "The author must be the first owner of the NFT" (ownerPkh == authorPkh)
        && traceIfFalse
          "Token name must be the hash of the owner pkh and the price"
          (checkTokenName ownerPkh price)
    ChangePrice (OwnerData ownerPkh _) newPrice ->
      traceIfFalse "Owner must sign the transaction" (txSignedBy info ownerPkh)
        && traceIfFalse
          "Token name must be the hash of the owner pkh and the price"
          (checkTokenName ownerPkh newPrice)
        && traceIfFalse "Old version must be burnt when reminting" checkBurnOld
    ChangeOwner (OwnerData _ price) newOwnerPkh ->
      traceIfFalse
        "Token name must be the hash of the owner pkh and the price"
        (checkTokenName newOwnerPkh price)
        && traceIfFalse "Old version must be burnt when reminting" checkBurnOld
        && traceIfFalse
          "Royalties must be paid to the author and the marketplace when selling the NFT"
          (checkRoyaltyPaid price)
  where
    !info = scriptContextTxInfo ctx
    checkConsumedUtxo = any (\i -> txInInfoOutRef i == oref) $ txInfoInputs info

    -- Check if the tokenname is the hash of the owner's pkh and the price
    checkTokenName ownerPkh price =
      case Value.flattenValue (txInfoMint info) of
        [(_, actualTokenName, _)] ->
          let computedTokenName = mkTokenName ownerPkh price
           in actualTokenName == computedTokenName
        _ -> False

    -- Check if only one token is minted
    checkMintedAmount = case Value.flattenValue (txInfoMint info) of
      [(cs, _, amt)] -> cs == ownCurrencySymbol ctx && amt == 1
      _ -> False

    -- Check if the old token is burnt
    checkBurnOld =
      let outVal = mconcat $ map txOutValue $ txInfoOutputs info
          inVal = mconcat $ map (txOutValue . txInInfoResolved) $ txInfoInputs info
          oneInput =
            case filter (\(cs, _, _) -> cs == ownCurrencySymbol ctx) $ Value.flattenValue inVal of
              [(_, _, amt)] -> amt == 1
              _ -> False
          oneOutput =
            case filter (\(cs, _, _) -> cs == ownCurrencySymbol ctx) $ Value.flattenValue outVal of
              [(_, _, amt)] -> amt == 1
              _ -> False
       in oneInput && oneOutput

    -- Check that royalties are correctly paid, and the payment utxos have the correct datum attached
    checkRoyaltyPaid price =
      let outs = txInfoOutputs info
          price' = fromEnum price
          royalty' = fromEnum royalty
          mpShare = fromEnum $ pcMarketplaceShare platformConfig

          authorAddr = pubKeyHashAddress authorPkh
          authorShare = Ada.lovelaceValueOf $ price' * 10000 `divide` royalty'

          marketplAddr = pubKeyHashAddress (pcMarketplacePkh platformConfig)
          marketplShare = Ada.lovelaceValueOf $ price' * 10000 `divide` mpShare

          -- FIXME: Next line causes "Exception: Error: Unsupported feature: Type constructor: GHC.Prim.Addr#"
          -- !curSymDatum = Datum $ PlutusTx.toBuiltinData $ ownCurrencySymbol ctx
          -- related to issue above, underscore to name to suppress linter
          !_datums = txInfoData info

          checkPaymentTxOut addr val (TxOut addr' val' _) =
            addr == addr' && val == val'
       in -- FIXME: see `curSymDatum`
          -- && (dh >>= (`lookup` datums)) == Just curSymDatum
          any (checkPaymentTxOut authorAddr authorShare) outs
            && any (checkPaymentTxOut marketplAddr marketplShare) outs
            
-- todo: docs
{-# INLINEABLE mkTokenName #-}
mkTokenName :: PubKeyHash -> Natural -> TokenName
mkTokenName (PubKeyHash pkh) price =
  TokenName $ sha2_256 $ (pkh <> toBin (fromEnum price))

{-# INLINEABLE toBin #-}
toBin :: Integer -> BuiltinByteString
toBin n
  | n == 0 = "0"
  | n == 1 = "1"
  | even n = rest <> "0"
  | otherwise = rest <> "1"
  where
    rest = toBin (divide n 2)

policy :: TxOutRef -> PubKeyHash -> Natural -> PlatformConfig -> ContentHash -> MintingPolicy
policy oref authorPkh royalty platformConfig contentHash =
  Scripts.mkMintingPolicyScript $
    $$(PlutusTx.compile [||\oref' pkh roy pc ch -> wrapMintingPolicy (mkPolicy oref' pkh roy pc ch)||])
      `PlutusTx.applyCode` PlutusTx.liftCode oref
      `PlutusTx.applyCode` PlutusTx.liftCode authorPkh
      `PlutusTx.applyCode` PlutusTx.liftCode royalty
      `PlutusTx.applyCode` PlutusTx.liftCode platformConfig
      `PlutusTx.applyCode` PlutusTx.liftCode contentHash
