{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}

module Mlabs.EfficientNFT.Token (
  mkPolicy,
  MintAct (MintToken, ChangePrice, ChangeOwner),
  OwnerData (..),
  PlatformConfig (..),
  -- policy,
) where

import Data.Binary qualified as Binary
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy (toStrict)
import Ledger (
  ScriptContext,
  TxOut (TxOut),
  TxOutRef,
  ownCurrencySymbol,
  pubKeyHashAddress,
  scriptContextTxInfo,
  txInInfoOutRef,
  txInInfoResolved,
  txInfoInputs,
  txInfoMint,
  txInfoOutputs,
  txOutValue,
  txSignedBy,
 )
import Ledger.Ada qualified as Ada
import Ledger.Crypto (PubKeyHash (PubKeyHash))
import Ledger.Typed.Scripts (MintingPolicy, wrapMintingPolicy)
import Ledger.Value (TokenName (TokenName))
import Ledger.Value qualified as Value
import Plutus.V1.Ledger.Scripts qualified as Scripts
import PlutusTx qualified
import PlutusTx.Builtins (BuiltinByteString, sha2_256, toBuiltin)
import PlutusTx.Enum (Enum (fromEnum))
import PlutusTx.Natural (Natural)
import PlutusTx.Trace (traceIfFalse)
import Prelude hiding (Enum (fromEnum))

data OwnerData = OwnerData
  { odOwnerPkh :: !PubKeyHash
  , odPrice :: !Natural
  }

PlutusTx.unstableMakeIsData ''OwnerData

data PlatformConfig = PlatformConfig
  { pcMarketplacePkh :: !PubKeyHash
  , pcMarketplaceShare :: !Natural
  }

PlutusTx.unstableMakeIsData ''PlatformConfig

data MintAct
  = MintToken OwnerData BuiltinByteString
  | ChangePrice OwnerData Natural
  | ChangeOwner OwnerData PubKeyHash

PlutusTx.unstableMakeIsData ''MintAct

mkPolicy :: TxOutRef -> PubKeyHash -> Natural -> PlatformConfig -> MintAct -> ScriptContext -> Bool
mkPolicy oref authorPkh royalty platformConfig mintAct ctx =
  case mintAct of
    MintToken (OwnerData ownerPkh price) contentHash ->
      traceIfFalse "UTXo specified as the parameter must be consumed" checkConsumedUtxo
        && traceIfFalse "Exactly one NFT must be minted" checkMintedAmount
        && traceIfFalse "Owner must sign the transaction" (txSignedBy info ownerPkh)
        && traceIfFalse "The author must be the first owner of the NFT" (ownerPkh == authorPkh)
        && traceIfFalse
          "Token name must be the hash of the owner pkh and the price"
          (checkTokenName ownerPkh price)
    -- price
    ChangePrice (OwnerData ownerPkh price) newPrice ->
      traceIfFalse "Owner must sign the transaction" (txSignedBy info ownerPkh)
        && traceIfFalse
          "Token name must be the hash of the owner pkh and the price"
          (checkTokenName ownerPkh newPrice)
    -- checkBurnOld
    ChangeOwner (OwnerData ownerPkh price) newOwnerPkh ->
      traceIfFalse "Owner must sign the transaction" (txSignedBy info ownerPkh)
        && traceIfFalse
          "Token name must be the hash of the owner pkh and the price"
          (checkTokenName newOwnerPkh price)
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

    checkBurnOld =
      let outVal = mconcat $ map txOutValue $ txInfoOutputs info
          inVal = mconcat $ map (txOutValue . txInInfoResolved) $ txInfoInputs info
          oneInput =
            case filter (\(cs, _, _) -> cs == ownCurrencySymbol ctx) $ Value.flattenValue inVal of
              [(_, oldTokenName, amt)] -> amt == 1
              _ -> False
          oneOutput =
            case filter (\(cs, _, _) -> cs == ownCurrencySymbol ctx) $ Value.flattenValue outVal of
              [(_, tokenName, amt)] -> amt == 1
              _ -> False
       in oneInput && oneOutput

    -- Check that royalties are correctly paid
    checkRoyaltyPaid price =
      let outs = txInfoOutputs info
          price' = fromEnum price
          royalty' = fromEnum royalty
          mpShare = fromEnum $ pcMarketplaceShare platformConfig

          authorAddr = pubKeyHashAddress authorPkh
          authorShare = Ada.lovelaceValueOf $ price' * 10000 `div` royalty'

          marketplAddr = pubKeyHashAddress (pcMarketplacePkh platformConfig)
          marketplShare = Ada.lovelaceValueOf $ price' * 10000 `div` mpShare
       in any (\(TxOut addr val _) -> addr == authorAddr && val == authorShare) outs
            && any (\(TxOut addr val _) -> addr == marketplAddr && val == marketplShare) outs

{-# INLINEABLE mkTokenName #-}
mkTokenName :: PubKeyHash -> Natural -> TokenName
mkTokenName (PubKeyHash pkh) price =
  TokenName $ sha2_256 $ pkh <> toBuiltin (toStrict (Binary.encode (fromEnum price)))

-- policy :: TxOutRef -> PubKeyHash -> Natural -> PlatformConfig -> MintingPolicy
-- policy oref authorPkh royalty platformConfig =
--   Scripts.mkMintingPolicyScript $
--     $$(PlutusTx.compile [||\oref' pkh roy pc -> wrapMintingPolicy $ mkPolicy oref' pkh roy pc ||])
--       `PlutusTx.applyCode` PlutusTx.liftCode oref
--       `PlutusTx.applyCode` PlutusTx.liftCode authorPkh
--       `PlutusTx.applyCode` PlutusTx.liftCode royalty
--       `PlutusTx.applyCode` PlutusTx.liftCode platformConfig
