module Mlabs.EfficientNFT.Contract.MarketplaceRedeem (marketplaceRedeem) where

import PlutusTx.Prelude hiding (mconcat)
import Prelude qualified as Hask

import Control.Monad (void)
import Data.Map qualified as Map
import Ledger (ChainIndexTxOut (_ciTxOutValue), Redeemer (Redeemer), minAdaTxOut, scriptAddress)
import Ledger.Constraints qualified as Constraints
import Ledger.Contexts (scriptCurrencySymbol)
import Ledger.Typed.Scripts (Any, validatorScript)
import Plutus.Contract qualified as Contract
import Plutus.V1.Ledger.Ada (toValue)
import Plutus.V1.Ledger.Api (toBuiltinData)
import Plutus.V1.Ledger.Value (assetClass, singleton, valueOf)
import Text.Printf (printf)

import Mlabs.EfficientNFT.Contract.Aux (getAddrUtxos)
import Mlabs.EfficientNFT.Marketplace
import Mlabs.EfficientNFT.Token (mkTokenName, policy)
import Mlabs.EfficientNFT.Types

-- | Redeem nft from the marketplace
marketplaceRedeem :: NftData -> UserContract ()
marketplaceRedeem nftData = do
  let policy' = policy . nftData'nftCollection $ nftData
      curr = scriptCurrencySymbol policy'
      scriptAddr = scriptAddress . validatorScript $ marketplaceValidator
      nft = nftData'nftId nftData
      tn = mkTokenName nft
      containsNft (_, tx) = valueOf (_ciTxOutValue tx) curr tn == 1
  utxo' <- find containsNft . Map.toList <$> getAddrUtxos scriptAddr
  (utxo, utxoIndex) <- case utxo' of
    Nothing -> Contract.throwError "NFT not found on marketplace"
    Just x -> Hask.pure x
  pkh <- Contract.ownPaymentPubKeyHash
  let nftValue = singleton curr tn 1
      lookup =
        Hask.mconcat
          [ Constraints.mintingPolicy policy'
          , Constraints.unspentOutputs $ Map.singleton utxo utxoIndex
          , Constraints.typedValidatorLookups marketplaceValidator
          , Constraints.otherScript (validatorScript marketplaceValidator)
          , Constraints.ownPaymentPubKeyHash pkh
          ]
      tx =
        Hask.mconcat
          [ Constraints.mustPayToPubKey pkh (nftValue <> toValue minAdaTxOut)
          , Constraints.mustSpendScriptOutput utxo (Redeemer . toBuiltinData $ Redeem nft)
          , Constraints.mustBeSignedBy pkh
          ]
  void $ Contract.submitTxConstraintsWith @Any lookup tx
  Contract.tell . Hask.pure $ nftData
  Contract.logInfo @Hask.String $ printf "Redeem successful: %s" (Hask.show $ assetClass curr tn)
