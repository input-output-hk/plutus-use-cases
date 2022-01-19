module Mlabs.EfficientNFT.Contract.MarketplaceBuy (marketplaceBuy) where

import PlutusTx.Prelude hiding (mconcat)
import Prelude qualified as Hask

import Control.Monad (void)
import Ledger (Datum (Datum), scriptAddress)
import Ledger.Constraints qualified as Constraints
import Ledger.Contexts (scriptCurrencySymbol)
import Ledger.Typed.Scripts (Any, validatorHash, validatorScript)
import Plutus.Contract qualified as Contract
import Plutus.V1.Ledger.Ada (lovelaceValueOf)
import Plutus.V1.Ledger.Api (Redeemer (Redeemer), toBuiltinData)
import Plutus.V1.Ledger.Value (assetClass, singleton)
import PlutusTx.Numeric.Extra (addExtend)
import Text.Printf (printf)

import Mlabs.EfficientNFT.Burn (burnValidator)
import Mlabs.EfficientNFT.Contract.Aux
import Mlabs.EfficientNFT.Marketplace
import Mlabs.EfficientNFT.Token
import Mlabs.EfficientNFT.Types

marketplaceBuy :: NftId -> UserContract ()
marketplaceBuy nft = do
  let burnHash = validatorHash burnValidator
      policy' = policy burnHash Nothing (nftId'collectionNft nft)
      curr = scriptCurrencySymbol policy'
      validator = marketplaceValidator curr
      scriptAddr = scriptAddress . validatorScript $ validator
  scriptUtxos <- getAddrUtxos scriptAddr
  userUtxos <- getUserUtxos
  pkh <- Contract.ownPaymentPubKeyHash
  let valHash = validatorHash validator
      nftPrice = nftId'price nft
      newNft = nft {nftId'owner = pkh}
      oldName = mkTokenName nft
      newName = mkTokenName newNft
      oldNftValue = singleton curr oldName (-1)
      newNftValue = singleton curr newName 1
      mintRedeemer = Redeemer . toBuiltinData $ ChangeOwner nft pkh
      getShare share = lovelaceValueOf $ addExtend nftPrice * 10000 `divide` share
      authorShare = getShare (addExtend . nftId'authorShare $ nft)
      marketplaceShare = getShare (addExtend . nftId'marketplaceShare $ nft)
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
          , Constraints.mustPayToOtherScript (nftId'marketplaceValHash nft) datum marketplaceShare
          , Constraints.mustPayToOtherScript valHash (Datum $ toBuiltinData ()) newNftValue
          ]
  void $ Contract.submitTxConstraintsWith @Any lookup tx
  Contract.tell . Hask.pure $ newNft
  Contract.logInfo @Hask.String $ printf "Change owner successful: %s" (Hask.show $ assetClass curr newName)
