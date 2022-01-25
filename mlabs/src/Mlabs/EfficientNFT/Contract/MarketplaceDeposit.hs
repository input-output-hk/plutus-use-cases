module Mlabs.EfficientNFT.Contract.MarketplaceDeposit (marketplaceDeposit) where

import PlutusTx.Prelude hiding (mconcat)
import Prelude qualified as Hask

import Control.Monad (void)
import Ledger (Datum (Datum), minAdaTxOut)
import Ledger.Constraints qualified as Constraints
import Ledger.Contexts (scriptCurrencySymbol)
import Ledger.Typed.Scripts (Any, validatorHash, validatorScript)
import Plutus.Contract qualified as Contract
import Plutus.V1.Ledger.Ada (toValue)
import Plutus.V1.Ledger.Api (ToData (toBuiltinData))
import Plutus.V1.Ledger.Value (assetClass, singleton)
import Text.Printf (printf)

import Mlabs.EfficientNFT.Contract.Aux
import Mlabs.EfficientNFT.Marketplace
import Mlabs.EfficientNFT.Token (mkTokenName, policy)
import Mlabs.EfficientNFT.Types

-- | Deposit nft in the marketplace
marketplaceDeposit :: NftData -> UserContract ()
marketplaceDeposit nftData = do
  let policy' = policy . nftData'nftCollection $ nftData
      curr = scriptCurrencySymbol policy'
      tn = mkTokenName . nftData'nftId $ nftData
      nftValue = singleton curr tn 1
      valHash = validatorHash marketplaceValidator
  utxos <- getUserUtxos
  let lookup =
        Hask.mconcat
          [ Constraints.mintingPolicy policy'
          , Constraints.unspentOutputs utxos
          , Constraints.typedValidatorLookups marketplaceValidator
          , Constraints.otherScript (validatorScript marketplaceValidator)
          ]
      tx =
        Hask.mconcat
          [ Constraints.mustPayToOtherScript
              valHash
              (Datum $ toBuiltinData ())
              (nftValue <> toValue minAdaTxOut)
          ]
  void $ Contract.submitTxConstraintsWith @Any lookup tx
  Contract.tell . Hask.pure $ nftData
  Contract.logInfo @Hask.String $ printf "Deposit successful: %s" (Hask.show $ assetClass curr tn)
