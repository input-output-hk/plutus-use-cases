module Mlabs.EfficientNFT.Contract.ChangeOwner (changeOwner) where

import PlutusTx.Prelude hiding (mconcat)
import Prelude qualified as Hask

import Control.Monad (void)
import Data.Void (Void)
import Ledger (Redeemer (Redeemer))
import Ledger.Constraints qualified as Constraints
import Plutus.Contract qualified as Contract
import Plutus.V1.Ledger.Ada (lovelaceValueOf)
import Plutus.V1.Ledger.Api (ToData (toBuiltinData))
import Plutus.V1.Ledger.Value (assetClass, assetClassValue, singleton, unAssetClass)
import PlutusTx.Numeric.Extra (addExtend)
import Text.Printf (printf)

import Mlabs.EfficientNFT.Contract.Aux
import Mlabs.EfficientNFT.Token
import Mlabs.EfficientNFT.Types

changeOwner :: PlatformConfig -> ChangeOwnerParams -> UserContract ()
changeOwner pc cp = do
  pkh <- Contract.ownPubKeyHash
  utxos <- getUserUtxos
  let policy' = nftId'policy . cp'nftId $ cp
      nftPrice = nftId'price . cp'nftId $ cp
      curr = fst . unAssetClass . nftId'assetClass . cp'nftId $ cp
      tn = mkTokenName (cp'owner cp) (nftId'price . cp'nftId $ cp)
      newNftValue = singleton curr tn 1
      oldNftValue = assetClassValue (nftId'assetClass . cp'nftId $ cp) (-1)
      ownerData = OwnerData pkh nftPrice
      mintRedeemer = Redeemer . toBuiltinData $ ChangeOwner ownerData (cp'owner cp)
      getShare share = lovelaceValueOf $ addExtend nftPrice * 10000 `divide` share
      authorShare = getShare (addExtend . nftId'authorShare . cp'nftId $ cp)
      marketplaceShare = getShare (addExtend . pcMarketplaceShare $ pc)
      ownerShare = lovelaceValueOf (addExtend nftPrice) - authorShare - marketplaceShare
      lookup =
        Hask.mconcat
          [ Constraints.mintingPolicy policy'
          , Constraints.unspentOutputs utxos
          ]
      tx =
        Hask.mconcat
          [ Constraints.mustMintValueWithRedeemer mintRedeemer (newNftValue <> oldNftValue)
          , Constraints.mustPayToPubKey (nftId'author . cp'nftId $ cp) authorShare
          , Constraints.mustPayToPubKey (nftId'owner . cp'nftId $ cp) ownerShare
          , -- TODO: attach datum here. Blocked by plutus-apps update
            Constraints.mustPayToPubKey (pcMarketplacePkh pc) marketplaceShare
          ]
  void $ Contract.submitTxConstraintsWith @Void lookup tx
  Contract.tell . Hask.pure $
    NftId
      { nftId'assetClass = assetClass curr tn
      , nftId'policy = policy'
      , nftId'price = nftPrice
      , nftId'owner = cp'owner cp
      , nftId'author = nftId'author . cp'nftId $ cp
      , nftId'authorShare = nftId'authorShare . cp'nftId $ cp
      }
  Contract.logInfo @Hask.String $ printf "Change owner successful: %s" (Hask.show $ assetClass curr tn)
