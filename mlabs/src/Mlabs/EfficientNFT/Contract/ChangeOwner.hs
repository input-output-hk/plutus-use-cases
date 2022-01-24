module Mlabs.EfficientNFT.Contract.ChangeOwner (changeOwner) where

import PlutusTx qualified
import PlutusTx.Prelude hiding (mconcat)
import Prelude qualified as Hask

import Control.Monad (void)
import Data.Void (Void)
import Ledger (Datum (Datum), Redeemer (Redeemer), scriptCurrencySymbol)
import Ledger.Constraints qualified as Constraints
import Ledger.Typed.Scripts (validatorHash)
import Plutus.Contract qualified as Contract
import Plutus.V1.Ledger.Ada (lovelaceValueOf)
import Plutus.V1.Ledger.Api (ToData (toBuiltinData))
import Plutus.V1.Ledger.Value (assetClass, singleton)
import PlutusTx.Numeric.Extra (addExtend)
import Text.Printf (printf)

import Mlabs.EfficientNFT.Burn (burnValidator)
import Mlabs.EfficientNFT.Contract.Aux
import Mlabs.EfficientNFT.Token
import Mlabs.EfficientNFT.Types

changeOwner :: ChangeOwnerParams -> UserContract ()
changeOwner cp = do
  utxos <- getUserUtxos
  let burnHash = validatorHash burnValidator
      policy' = policy burnHash Nothing (nftId'collectionNft . cp'nftId $ cp)
      nftPrice = nftId'price . cp'nftId $ cp
      curr = scriptCurrencySymbol policy'
      newNft = (cp'nftId cp) {nftId'owner = cp'owner cp}
      oldName = mkTokenName . cp'nftId $ cp
      newName = mkTokenName newNft
      oldNftValue = singleton curr oldName (-1)
      newNftValue = singleton curr newName 1
      mintRedeemer = Redeemer . toBuiltinData $ ChangeOwner (cp'nftId cp) (cp'owner cp)
      getShare share = lovelaceValueOf $ (addExtend nftPrice * 10000) `divide` share
      authorShare = getShare (addExtend . nftId'authorShare . cp'nftId $ cp)
      marketplaceShare = getShare (addExtend . nftId'marketplaceShare . cp'nftId $ cp)
      ownerShare = lovelaceValueOf (addExtend nftPrice) - authorShare - marketplaceShare
      datum = Datum . PlutusTx.toBuiltinData $ curr
      lookup =
        Hask.mconcat
          [ Constraints.mintingPolicy policy'
          , Constraints.unspentOutputs utxos
          ]
      tx =
        Hask.mconcat
          [ Constraints.mustMintValueWithRedeemer mintRedeemer (newNftValue <> oldNftValue)
          , Constraints.mustPayToPubKey (cp'owner cp) newNftValue
          , Constraints.mustPayWithDatumToPubKey (nftId'author . cp'nftId $ cp) datum authorShare
          , Constraints.mustPayWithDatumToPubKey (nftId'owner . cp'nftId $ cp) datum ownerShare
          , Constraints.mustPayToOtherScript (nftId'marketplaceValHash . cp'nftId $ cp) datum marketplaceShare
          ]
  void $ Contract.submitTxConstraintsWith @Void lookup tx
  Contract.tell . Hask.pure $ newNft
  Contract.logInfo @Hask.String $ printf "Change owner successful: %s" (Hask.show $ assetClass curr newName)
