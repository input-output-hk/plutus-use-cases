module Mlabs.EfficientNFT.Contract.MarketplaceBuy (marketplaceBuy) where

import PlutusTx.Prelude hiding (mconcat)
import Prelude qualified as Hask

import Control.Monad (void)
import Data.Map qualified as Map
import Data.Monoid (mconcat)
import Ledger (Datum (Datum), minAdaTxOut, scriptAddress, _ciTxOutValue)
import Ledger.Constraints qualified as Constraints
import Ledger.Contexts (scriptCurrencySymbol)
import Ledger.Typed.Scripts (Any, validatorHash, validatorScript)
import Plutus.Contract qualified as Contract
import Plutus.V1.Ledger.Ada (adaSymbol, adaToken, getLovelace, lovelaceValueOf, toValue)
import Plutus.V1.Ledger.Api (Redeemer (Redeemer), toBuiltinData)
import Plutus.V1.Ledger.Value (assetClass, singleton, valueOf)
import PlutusTx.Numeric.Extra (addExtend)
import Text.Printf (printf)

import Mlabs.EfficientNFT.Burn (burnValidator)
import Mlabs.EfficientNFT.Contract.Aux
import Mlabs.EfficientNFT.Marketplace
import Mlabs.EfficientNFT.Token
import Mlabs.EfficientNFT.Types

marketplaceBuy :: NftId -> UserContract ()
marketplaceBuy nft = do
  pkh <- Contract.ownPaymentPubKeyHash
  let burnHash = validatorHash burnValidator
      policy' = policy burnHash Nothing (nftId'collectionNft nft)
      curr = scriptCurrencySymbol policy'
      validator = marketplaceValidator curr
      scriptAddr = scriptAddress . validatorScript $ validator
      containsNft (_, tx) = valueOf (_ciTxOutValue tx) curr oldName == 1
      valHash = validatorHash validator
      nftPrice = nftId'price nft
      newNft = nft {nftId'owner = pkh}
      oldName = mkTokenName nft
      newName = mkTokenName newNft
      oldNftValue = singleton curr oldName (-1)
      newNftValue = singleton curr newName 1
      mintRedeemer = Redeemer . toBuiltinData $ ChangeOwner nft pkh
      getShare share
        | val < getLovelace minAdaTxOut = lovelaceValueOf 0
        | otherwise = lovelaceValueOf val
        where
          val = addExtend nftPrice * share `divide` 10000
      authorShare = getShare (addExtend . nftId'authorShare $ nft)
      marketplaceShare = getShare (addExtend . nftId'marketplaceShare $ nft)
      ownerShare = lovelaceValueOf (addExtend nftPrice) - authorShare - marketplaceShare
      datum = Datum . toBuiltinData $ curr
      filterLowValue v t
        | valueOf v adaSymbol adaToken < getLovelace minAdaTxOut = mempty
        | otherwise = t v
  userUtxos <- getUserUtxos
  utxo' <- find containsNft . Map.toList <$> getAddrUtxos scriptAddr
  (utxo, utxoIndex) <- case utxo' of
    Nothing -> Contract.throwError "NFT not found on marketplace"
    Just x -> Hask.pure x
  Contract.logInfo @Hask.String $ printf "UTXO: %s" (Hask.show $ _ciTxOutValue utxoIndex)
  Contract.logInfo @Hask.String $ printf "OwnerShare: %s" (Hask.show ownerShare)
  Contract.logInfo @Hask.String $ printf "AuthorShare: %s" (Hask.show authorShare)
  Contract.logInfo @Hask.String $ printf "MarketplaceShare: %s" (Hask.show marketplaceShare)
  let userValues = mconcat . fmap _ciTxOutValue . Map.elems $ userUtxos
      lookup =
        Hask.mconcat
          [ Constraints.mintingPolicy policy'
          , Constraints.typedValidatorLookups validator
          , Constraints.otherScript (validatorScript validator)
          , Constraints.unspentOutputs $ Map.insert utxo utxoIndex userUtxos
          , Constraints.ownPaymentPubKeyHash pkh
          ]
      tx =
        filterLowValue marketplaceShare (Constraints.mustPayToOtherScript (nftId'marketplaceValHash nft) datum)
          <> filterLowValue authorShare (Constraints.mustPayWithDatumToPubKey (nftId'author nft) datum)
          <> Hask.mconcat
            [ Constraints.mustMintValueWithRedeemer mintRedeemer (newNftValue <> oldNftValue)
            , Constraints.mustSpendScriptOutput utxo (Redeemer . toBuiltinData $ ())
            , Constraints.mustPayToPubKey (nftId'owner nft) ownerShare
            , Constraints.mustPayToOtherScript valHash (Datum $ toBuiltinData ()) (newNftValue <> toValue minAdaTxOut)
            , -- Hack to overcome broken balancing
              Constraints.mustPayToPubKey pkh (userValues - toValue (minAdaTxOut * 3) - lovelaceValueOf (addExtend nftPrice))
            ]
  void $ Contract.submitTxConstraintsWith @Any lookup tx
  Contract.tell . Hask.pure $ newNft
  Contract.logInfo @Hask.String $ printf "Change owner successful: %s" (Hask.show $ assetClass curr newName)
