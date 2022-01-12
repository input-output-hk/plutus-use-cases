module Mlabs.EfficientNFT.Contract.MarketplaceBuy (marketplaceBuy) where

import PlutusTx.Prelude hiding (mconcat)
import Prelude qualified as Hask

import Control.Monad (void)
import Ledger (Datum (Datum), scriptAddress)
import Ledger.Constraints qualified as Constraints
import Ledger.Typed.Scripts (Any, validatorHash, validatorScript)
import Plutus.Contract qualified as Contract
import Plutus.V1.Ledger.Ada (lovelaceValueOf)
import Plutus.V1.Ledger.Api (Redeemer (Redeemer), toBuiltinData)
import Plutus.V1.Ledger.Value (assetClass, assetClassValue, singleton, unAssetClass)
import PlutusTx.Numeric.Extra (addExtend)
import Text.Printf (printf)

import Mlabs.EfficientNFT.Contract.Aux
import Mlabs.EfficientNFT.Marketplace
import Mlabs.EfficientNFT.Token
import Mlabs.EfficientNFT.Types

marketplaceBuy :: PlatformConfig -> NftId -> UserContract ()
marketplaceBuy pc nft = do
  let curr = fst . unAssetClass . nftId'assetClass $ nft
      validator = marketplaceValidator curr
      scriptAddr = scriptAddress . validatorScript $ validator
  scriptUtxos <- getAddrUtxos scriptAddr
  userUtxos <- getUserUtxos
  pkh <- Contract.ownPaymentPubKeyHash
  let policy' = nftId'policy nft
      valHash = validatorHash validator
      nftPrice = nftId'price nft
      tn = mkTokenName pkh (nftId'price nft)
      newNftValue = singleton curr tn 1
      oldNftValue = assetClassValue (nftId'assetClass nft) (-1)
      ownerData = OwnerData pkh nftPrice
      mintRedeemer = Redeemer . toBuiltinData $ ChangeOwner ownerData pkh
      getShare share = lovelaceValueOf $ addExtend nftPrice * 10000 `divide` share
      authorShare = getShare (addExtend . nftId'authorShare $ nft)
      marketplaceShare = getShare (addExtend . pcMarketplaceShare $ pc)
      ownerShare = lovelaceValueOf (addExtend nftPrice) - authorShare - marketplaceShare
      datum = Datum . toBuiltinData $ curr
      lookup =
        Hask.mconcat
          [ Constraints.mintingPolicy policy'
          , Constraints.unspentOutputs (scriptUtxos Hask.<> userUtxos)
          ]
      tx =
        Hask.mconcat
          [ Constraints.mustMintValueWithRedeemer mintRedeemer (newNftValue <> oldNftValue)
          , Constraints.mustPayWithDatumToPubKey (nftId'author nft) datum authorShare
          , Constraints.mustPayWithDatumToPubKey (nftId'owner nft) datum ownerShare
          , Constraints.mustPayWithDatumToPubKey (pcMarketplacePkh pc) datum marketplaceShare
          , Constraints.mustPayToOtherScript valHash (Datum $ toBuiltinData ()) newNftValue
          ]
  void $ Contract.submitTxConstraintsWith @Any lookup tx
  Contract.tell . Hask.pure $
    NftId
      { nftId'assetClass = assetClass curr tn
      , nftId'policy = policy'
      , nftId'price = nftPrice
      , nftId'owner = pkh
      , nftId'author = nftId'author nft
      , nftId'authorShare = nftId'authorShare nft
      }
  Contract.logInfo @Hask.String $ printf "Change owner successful: %s" (Hask.show $ assetClass curr tn)
