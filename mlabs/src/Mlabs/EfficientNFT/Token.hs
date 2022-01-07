{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}

module Mlabs.EfficientNFT.Token (
  mkPolicy,
  MintAct (MintToken, ChangePrice, ChangeOwner),
  OwnerData (..),
  PlatformConfig (..),
  policy,
) where

import Data.Binary qualified as Binary
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Ledger (
  ScriptContext,
  TxOut (TxOut),
  TxOutRef,
  ownCurrencySymbol,
  pubKeyHashAddress,
  scriptContextTxInfo,
  txInInfoOutRef,
  txInfoInputs,
  txInfoMint,
  txInfoOutputs,
  txSignedBy,
 )
import Ledger.Ada qualified as Ada
import Ledger.Crypto (PubKeyHash (PubKeyHash))
import Ledger.Value qualified as Value
import Plutus.V1.Ledger.Scripts qualified as Scripts
import PlutusTx qualified
import PlutusTx.Builtins (sha2_256, toBuiltin)
import PlutusTx.Natural (Natural)
import PlutusTx.Trace (traceIfFalse)
import Prelude

data MintAct
  = MintToken OwnerData ByteString
  | ChangePrice OwnerData Integer
  | ChangeOwner OwnerData PubKeyHash

data OwnerData = OwnerData
  { odOwnerPkh :: !PubKeyHash
  , odPrice :: !Natural
  }

data PlatformConfig = PlatformConfig
  { pcMarketplacePkh :: !PubKeyHash
  , pcMarketplaceShare :: !Natural
  }

mkPolicy :: TxOutRef -> PubKeyHash -> Natural -> PlatformConfig -> MintAct -> ScriptContext -> Bool
mkPolicy oref authorPkh royalty platformConfig mintAct ctx =
  case mintAct of
    MintToken (OwnerData ownerPkh price) contentHash ->
      traceIfFalse "UTXo specified as the parameter must be consumed" checkConsumedUtxo
        && traceIfFalse "Exactly one NFT must be minted" checkMintedAmount
        && traceIfFalse "Owner must sign the transaction" (txSignedBy ownerPkh)
        && traceIfFalse "The author must be the first owner of the NFT" (ownerPkh == authorPkh)
        && traceIfFalse
          "Token name must be the hash of the owner pkh and the price"
          (checkTokenName ownerPkh price)
    -- price
    ChangePrice (OwnerData ownerPkh price) newPrice ->
      traceIfFalse "Owner must sign the transaction" (txSignedBy ownerPkh)
        && traceIfFalse
          "Token name must be the hash of the owner pkh and the price"
          (checkTokenName ownerPkh newPrice)
    -- checkBurnOld
    ChangeOwner (OwnerData ownerPkh price) newOwnerPkh ->
      traceIfFalse "Owner must sign the transaction" (txSignedBy ownerPkh)
        && traceIfFalse
          "Token name must be the hash of the owner pkh and the price"
          (checkTokenName newOwnerPkh price)
        && traceIfFalse
          "Royalties must be paid to the author and the marketplace when selling the NFT"
          checkRoyaltyPaid
  where
    -- checkBurnOld

    !info = scriptContextTxInfo ctx
    checkConsumedUtxo = any (\i -> txInInfoOutRef i == oref) $ txInfoInputs info

    -- Check if the tokenname is the hash of the owner's pkh and the price
    checkTokenName ownerPkh price =
      case Value.flattenValue (txInfoMint ctx) of
        [(_, actualTokenName, _)] ->
          let computedTokenName = mkTokenName ownerPkh price
           in actualTokenName == computedTokenName
        _ -> False

    -- Check if only one token is minted
    checkMintedAmount = case Value.flattenValue (txInfoMint info) of
      [(cs, _, amt)] -> cs == ownCurrencySymbol ctx && amt == 1
      _ -> False

    checkBurnOld = case Value.flattenValue (txInfoOutputs info) of
      [(cs, oldTokenName, amt)] -> cs == ownCurrencySymbol ctx && amt == 1
      _ -> False

    -- Check that royalties are correctly paid
    checkRoyaltyPaid price =
      let outs = txInfoOutputs info
          authorAddr = pubKeyHashAddress authorPkh
          authorShare = Ada.lovelaceValueOf $ price / royalty / 10000

          marketplAddr = pubKeyHashAddress (pcMarketplacePkh platformConfig)
          marketplShare = Ada.lovelaceValueOf $ price / (pcMarketplaceShare platformConfig) / 10000
       in any (\(TxOut addr val _) -> addr == authorAddr && val == authorShare) outs
            && (\(TxOut addr val _) -> addr == marketplAddr && val == marketplShare) outs

{-# INLINEABLE mkTokenName #-}
mkTokenName :: PubKeyHash -> Integer -> ByteString
mkTokenName (PubKeyHash pkh) price =
  sha2_256 $ pkh <> toBuiltin $ Binary.encode price

policy :: TxOutRef -> PubKeyHash -> Natural -> PlatformConfig -> Scripts.MintingPolicy
policy oref authorPkh royalty platformConfig =
  Scripts.mkMintingPolicyScript $
    $$(PlutusTx.compile [||\oref' pkh roy pc -> mkPolicy oref' pkh roy pc ||])
      `PlutusTx.applyCode` PlutusTx.liftCode oref
      `PlutusTx.applyCode` PlutusTx.liftCode authorPkh
      `PlutusTx.applyCode` PlutusTx.liftCode royalty
      `PlutusTx.applyCode` PlutusTx.liftCode platformConfig
