module Mlabs.EfficientNFT.Contract.MarketplaceBuy (marketplaceBuy) where

import PlutusTx.Prelude hiding (mconcat)
import Prelude qualified as Hask

import Control.Monad (void)
import Data.Map qualified as Map
import Ledger (Datum (Datum), minAdaTxOut, scriptAddress, _ciTxOutValue)
import Ledger.Constraints qualified as Constraints
import Ledger.Contexts (scriptCurrencySymbol)
import Ledger.Typed.Scripts (Any, validatorHash, validatorScript)
import Plutus.Contract qualified as Contract
import Plutus.V1.Ledger.Ada (getLovelace, lovelaceValueOf, toValue)
import Plutus.V1.Ledger.Api (Redeemer (Redeemer), toBuiltinData)
import Plutus.V1.Ledger.Value (assetClass, singleton, valueOf)
import PlutusTx.Numeric.Extra (addExtend)
import Text.Printf (printf)

import Mlabs.EfficientNFT.Contract.Aux
import Mlabs.EfficientNFT.Marketplace
import Mlabs.EfficientNFT.Token
import Mlabs.EfficientNFT.Types

marketplaceBuy :: NftData -> UserContract NftData
marketplaceBuy nftData = do
  pkh <- Contract.ownPaymentPubKeyHash
  let policy' = policy . nftData'nftCollection $ nftData
      nft = nftData'nftId nftData
      curr = scriptCurrencySymbol policy'
      scriptAddr = scriptAddress . validatorScript $ marketplaceValidator
      containsNft (_, tx) = valueOf (_ciTxOutValue tx) curr oldName == 1
      valHash = validatorHash marketplaceValidator
      nftPrice = nftId'price nft
      newNft = nft {nftId'owner = pkh}
      oldName = mkTokenName nft
      newName = mkTokenName newNft
      oldNftValue = singleton curr oldName (-1)
      newNftValue = singleton curr newName 1
      mintRedeemer = Redeemer . toBuiltinData $ ChangeOwner nft pkh
      getShare share = (addExtend nftPrice * share) `divide` 10000
      authorShare = getShare (addExtend . nftCollection'authorShare . nftData'nftCollection $ nftData)
      daoShare = getShare (addExtend . nftCollection'daoShare . nftData'nftCollection $ nftData)
      shareToSubtract v
        | v < getLovelace minAdaTxOut = 0
        | otherwise = v
      ownerShare = lovelaceValueOf (addExtend nftPrice - shareToSubtract authorShare - shareToSubtract daoShare)
      datum = Datum . toBuiltinData $ (curr, oldName)
      filterLowValue v t
        | v < getLovelace minAdaTxOut = mempty
        | otherwise = t (lovelaceValueOf v)
      newNftData = NftData (nftData'nftCollection nftData) newNft
  userUtxos <- getUserUtxos
  utxo' <- find containsNft . Map.toList <$> getAddrUtxos scriptAddr
  (utxo, utxoIndex) <- case utxo' of
    Nothing -> Contract.throwError "NFT not found on marketplace"
    Just x -> Hask.pure x
  let lookup =
        Hask.mconcat
          [ Constraints.mintingPolicy policy'
          , Constraints.typedValidatorLookups marketplaceValidator
          , Constraints.otherScript (validatorScript marketplaceValidator)
          , Constraints.unspentOutputs $ Map.insert utxo utxoIndex userUtxos
          , Constraints.ownPaymentPubKeyHash pkh
          ]
      tx =
        filterLowValue
          daoShare
          (Constraints.mustPayToOtherScript (nftCollection'daoScript . nftData'nftCollection $ nftData) datum)
          <> filterLowValue
            authorShare
            (Constraints.mustPayWithDatumToPubKey (nftCollection'author . nftData'nftCollection $ nftData) datum)
          <> Hask.mconcat
            [ Constraints.mustMintValueWithRedeemer mintRedeemer (newNftValue <> oldNftValue)
            , Constraints.mustSpendScriptOutput utxo (Redeemer $ toBuiltinData ())
            , Constraints.mustPayWithDatumToPubKey (nftId'owner nft) datum ownerShare
            , Constraints.mustPayToOtherScript
                valHash
                (Datum . toBuiltinData . MarketplaceDatum $ assetClass curr newName)
                (newNftValue <> toValue minAdaTxOut)
            ]
  void $ Contract.submitTxConstraintsWith @Any lookup tx
  Contract.tell . Hask.pure $ newNftData
  Contract.logInfo @Hask.String $ printf "Buy successful: %s" (Hask.show $ assetClass curr newName)
  Hask.pure newNftData
