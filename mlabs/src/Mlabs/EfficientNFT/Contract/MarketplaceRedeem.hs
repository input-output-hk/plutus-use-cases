module Mlabs.EfficientNFT.Contract.MarketplaceRedeem (marketplaceRedeem) where

import PlutusTx.Prelude hiding (mconcat)
import Prelude qualified as Hask

import Control.Monad (void)
import Ledger (scriptAddress)
import Ledger.Constraints qualified as Constraints
import Ledger.Typed.Scripts (Any, validatorScript)
import Plutus.Contract qualified as Contract
import Plutus.V1.Ledger.Value (assetClassValue, unAssetClass)
import Text.Printf (printf)

import Mlabs.EfficientNFT.Contract.Aux
import Mlabs.EfficientNFT.Marketplace
import Mlabs.EfficientNFT.Types

-- | Redeem nft from the marketplace
marketplaceRedeem :: PlatformConfig -> NftId -> UserContract ()
marketplaceRedeem _ nft = do
  let curr = fst . unAssetClass . nftId'assetClass $ nft
      validator = marketplaceValidator curr
      scriptAddr = scriptAddress . validatorScript $ validator
  utxos <- getAddrUtxos scriptAddr
  pkh <- Contract.ownPaymentPubKeyHash
  let policy' = nftId'policy nft
      nftValue = assetClassValue (nftId'assetClass nft) 1
      lookup =
        Hask.mconcat
          [ Constraints.mintingPolicy policy'
          , Constraints.unspentOutputs utxos
          ]
      tx =
        Hask.mconcat
          [ Constraints.mustPayToPubKey pkh nftValue
          ]
  void $ Contract.submitTxConstraintsWith @Any lookup tx
  Contract.tell . Hask.pure $ nft
  Contract.logInfo @Hask.String $ printf "Redeem successful: %s" (Hask.show . nftId'assetClass $ nft)
