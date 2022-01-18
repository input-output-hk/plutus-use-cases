module Mlabs.EfficientNFT.Contract.MarketplaceRedeem (marketplaceRedeem) where

import PlutusTx.Prelude hiding (mconcat)
import Prelude qualified as Hask

import Control.Monad (void)
import Ledger (scriptAddress)
import Ledger.Constraints qualified as Constraints
import Ledger.Contexts (scriptCurrencySymbol)
import Ledger.Typed.Scripts (Any, validatorHash, validatorScript)
import Plutus.Contract qualified as Contract
import Plutus.V1.Ledger.Value (assetClass, singleton)
import Text.Printf (printf)

import Mlabs.EfficientNFT.Burn (burnValidator)
import Mlabs.EfficientNFT.Contract.Aux
import Mlabs.EfficientNFT.Marketplace
import Mlabs.EfficientNFT.Token (mkTokenName, policy)
import Mlabs.EfficientNFT.Types

-- | Redeem nft from the marketplace
marketplaceRedeem :: NftId -> UserContract ()
marketplaceRedeem nft = do
  let burnHash = validatorHash burnValidator
      policy' = policy burnHash Nothing (nftId'collectionNft nft)
      curr = scriptCurrencySymbol policy'
      validator = marketplaceValidator curr
      scriptAddr = scriptAddress . validatorScript $ validator
  utxos <- getAddrUtxos scriptAddr
  pkh <- Contract.ownPaymentPubKeyHash
  let tn = mkTokenName nft
      nftValue = singleton curr tn 1
      lookup =
        Hask.mconcat
          [ Constraints.mintingPolicy policy'
          , Constraints.unspentOutputs utxos
          ]
      tx =
        Hask.mconcat
          [ Constraints.mustPayToPubKey pkh nftValue
          , Constraints.mustBeSignedBy pkh
          ]
  void $ Contract.submitTxConstraintsWith @Any lookup tx
  Contract.tell . Hask.pure $ nft
  Contract.logInfo @Hask.String $ printf "Redeem successful: %s" (Hask.show $ assetClass curr tn)
