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

import Mlabs.EfficientNFT.Burn (burnValidator)
import Mlabs.EfficientNFT.Contract.Aux
import Mlabs.EfficientNFT.Marketplace
import Mlabs.EfficientNFT.Token (mkTokenName, policy)
import Mlabs.EfficientNFT.Types

-- | Deposit nft in the marketplace
marketplaceDeposit :: NftId -> UserContract ()
marketplaceDeposit nft = do
  let burnHash = validatorHash burnValidator
      policy' = policy burnHash Nothing (nftId'collectionNft nft)
      curr = scriptCurrencySymbol policy'
      tn = mkTokenName nft
      nftValue = singleton curr tn 1
      validator = marketplaceValidator curr
      valHash = validatorHash validator
  utxos <- getUserUtxos
  let lookup =
        Hask.mconcat
          [ Constraints.mintingPolicy policy'
          , Constraints.unspentOutputs utxos
          , Constraints.typedValidatorLookups validator
          , Constraints.otherScript (validatorScript validator)
          ]
      tx =
        Hask.mconcat
          [ Constraints.mustPayToOtherScript
              valHash
              (Datum $ toBuiltinData ())
              (nftValue <> toValue minAdaTxOut)
          ]
  void $ Contract.submitTxConstraintsWith @Any lookup tx
  Contract.tell . Hask.pure $ nft
  Contract.logInfo @Hask.String $ printf "Deposit successful: %s" (Hask.show $ assetClass curr tn)
