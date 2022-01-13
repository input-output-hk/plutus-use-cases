{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}

module Mlabs.EfficientNFT.Token (
  mkPolicy,
  policy,
  mkTokenName,
) where

import PlutusTx qualified
import PlutusTx.Prelude

import Ledger (
  Datum (Datum),
  MintingPolicy,
  PaymentPubKeyHash (PaymentPubKeyHash, unPaymentPubKeyHash),
  PubKeyHash (PubKeyHash),
  ScriptContext,
  TxInInfo (txInInfoOutRef, txInInfoResolved),
  TxInfo (txInfoInputs, txInfoMint, txInfoOutputs),
  TxOut (TxOut, txOutValue),
  TxOutRef,
  findDatum,
  ownCurrencySymbol,
  pubKeyHashAddress,
  scriptContextTxInfo,
  txSignedBy,
 )
import Ledger.Ada qualified as Ada
import Ledger.Scripts qualified as Scripts
import Ledger.Typed.Scripts (wrapMintingPolicy)
import Ledger.Value (TokenName (TokenName))
import Ledger.Value qualified as Value
import PlutusTx.Natural (Natural)

import Mlabs.EfficientNFT.Types

-- todo: docs
{-# INLINEABLE mkPolicy #-}
mkPolicy ::
  TxOutRef ->
  PaymentPubKeyHash ->
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
      traceIfFalse "Owner must sign the transaction" (txSignedBy info $ unPaymentPubKeyHash ownerPkh)
        && traceIfFalse
          "Token name must be the hash of the owner pkh and the price"
          (checkTokenName ownerPkh newPrice)
        && traceIfFalse "Old version must be burnt when reminting" checkBurnOld
    ChangeOwner (OwnerData ownerPkh price) newOwnerPkh ->
      traceIfFalse
        "Token name must be the hash of the owner pkh and the price"
        (checkTokenName newOwnerPkh price)
        && traceIfFalse "Old version must be burnt when reminting" checkBurnOld
        && traceIfFalse
          "Royalties must be paid to the author and the marketplace when selling the NFT"
          (checkPartiesGotCorrectPayments price ownerPkh)
  where
    !info = scriptContextTxInfo ctx
    -- ! force evaluation of `ownCs` causes policy compilation error
    ownCs = ownCurrencySymbol ctx
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
      [(cs, _, amt)] -> cs == ownCs && amt == 1
      _ -> False

    -- Check if the old token is burnt
    checkBurnOld =
      let outVal = mconcat $ map txOutValue $ txInfoOutputs info
          inVal = mconcat $ map (txOutValue . txInInfoResolved) $ txInfoInputs info
          oneInput =
            case filter (\(cs, _, _) -> cs == ownCs) $ Value.flattenValue inVal of
              [(_, _, amt)] -> amt == 1
              _ -> False
          oneOutput =
            case filter (\(cs, _, _) -> cs == ownCs) $ Value.flattenValue outVal of
              [(_, _, amt)] -> amt == 1
              _ -> False
       in oneInput && oneOutput

    -- Check that all parties received corresponding payments,
    -- and the payment utxos have the correct datum attached
    checkPartiesGotCorrectPayments price ownerPkh =
      let outs = txInfoOutputs info
          price' = fromEnum price
          royalty' = fromEnum royalty
          mpShare = fromEnum $ pcMarketplaceShare platformConfig

          authorAddr = pubKeyHashAddress authorPkh Nothing
          authorShare = Ada.lovelaceValueOf $ price' * 10000 `divide` royalty'

          marketplAddr = pubKeyHashAddress (pcMarketplacePkh platformConfig) Nothing
          marketplShare = Ada.lovelaceValueOf $ price' * 10000 `divide` mpShare

          ownerAddr = pubKeyHashAddress ownerPkh Nothing
          ownerShare = Ada.lovelaceValueOf (price' * 10000) - authorShare - marketplShare

          curSymDatum = Datum $ PlutusTx.toBuiltinData ownCs

          checkPaymentTxOut addr val (TxOut addr' val' dh) =
            addr == addr' && val == val'
              && (dh >>= \dh' -> findDatum dh' info) == Just curSymDatum
       in any (checkPaymentTxOut authorAddr authorShare) outs
            && any (checkPaymentTxOut marketplAddr marketplShare) outs
            && any (checkPaymentTxOut ownerAddr ownerShare) outs

-- todo: docs
{-# INLINEABLE mkTokenName #-}
mkTokenName :: PaymentPubKeyHash -> Natural -> TokenName
mkTokenName (PaymentPubKeyHash (PubKeyHash pkh)) price =
  TokenName $ sha2_256 (pkh <> toBin (fromEnum price))

{-# INLINEABLE toBin #-}
toBin :: Integer -> BuiltinByteString
toBin n = toBin' n mempty
  where
    toBin' n' rest
      | n' < 256 = consByteString n' rest
      | otherwise = toBin' (n' `divide` 256) (consByteString (n' `modulo` 256) rest)

policy :: TxOutRef -> PaymentPubKeyHash -> Natural -> PlatformConfig -> ContentHash -> MintingPolicy
policy oref authorPkh royalty platformConfig contentHash =
  Scripts.mkMintingPolicyScript $
    $$(PlutusTx.compile [||\oref' pkh roy pc ch -> wrapMintingPolicy (mkPolicy oref' pkh roy pc ch)||])
      `PlutusTx.applyCode` PlutusTx.liftCode oref
      `PlutusTx.applyCode` PlutusTx.liftCode authorPkh
      `PlutusTx.applyCode` PlutusTx.liftCode royalty
      `PlutusTx.applyCode` PlutusTx.liftCode platformConfig
      `PlutusTx.applyCode` PlutusTx.liftCode contentHash
