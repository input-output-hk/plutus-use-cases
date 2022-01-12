module Mlabs.EfficientNFT.Contract.MarketplaceDeposit (marketplaceDeposit) where

import PlutusTx.Prelude hiding (mconcat)
import Prelude qualified as Hask

import Control.Monad (void)
import Ledger (Datum (Datum))
import Ledger.Constraints qualified as Constraints
import Ledger.Typed.Scripts (Any, validatorHash)
import Plutus.Contract qualified as Contract
import Plutus.V1.Ledger.Api (ToData (toBuiltinData))
import Plutus.V1.Ledger.Value (assetClassValue, unAssetClass)
import Text.Printf (printf)

import Mlabs.EfficientNFT.Contract.Aux
import Mlabs.EfficientNFT.Marketplace
import Mlabs.EfficientNFT.Types

-- | Deposit nft in the marketplace
marketplaceDeposit :: PlatformConfig -> NftId -> UserContract ()
marketplaceDeposit _ nft = do
  utxos <- getUserUtxos
  let policy' = nftId'policy nft
      curr = fst . unAssetClass . nftId'assetClass $ nft
      nftValue = assetClassValue (nftId'assetClass nft) 1
      valHash = validatorHash $ marketplaceValidator curr
      lookup =
        Hask.mconcat
          [ Constraints.mintingPolicy policy'
          , Constraints.unspentOutputs utxos
          ]
      tx =
        Hask.mconcat
          [ Constraints.mustPayToOtherScript valHash (Datum $ toBuiltinData ()) nftValue
          ]
  void $ Contract.submitTxConstraintsWith @Any lookup tx
  Contract.tell . Hask.pure $ nft
  Contract.logInfo @Hask.String $ printf "Deposit successful: %s" (Hask.show . nftId'assetClass $ nft)
