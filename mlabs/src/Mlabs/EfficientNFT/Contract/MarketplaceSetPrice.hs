module Mlabs.EfficientNFT.Contract.MarketplaceSetPrice (marketplaceSetPrice) where

import PlutusTx.Prelude hiding (mconcat)
import Prelude qualified as Hask

import Control.Monad (void)
import Ledger (Datum (Datum), Redeemer (Redeemer), scriptAddress)
import Ledger.Constraints qualified as Constraints
import Ledger.Contexts (scriptCurrencySymbol)
import Ledger.Typed.Scripts (Any, validatorHash, validatorScript)
import Plutus.Contract qualified as Contract
import Plutus.V1.Ledger.Api (ToData (toBuiltinData))
import Plutus.V1.Ledger.Value (assetClass, singleton)
import Text.Printf (printf)

import Mlabs.EfficientNFT.Burn (burnValidator)
import Mlabs.EfficientNFT.Contract.Aux
import Mlabs.EfficientNFT.Marketplace
import Mlabs.EfficientNFT.Token
import Mlabs.EfficientNFT.Types

marketplaceSetPrice :: SetPriceParams -> UserContract ()
marketplaceSetPrice sp = do
  let burnHash = validatorHash burnValidator
      policy' = policy burnHash Nothing (nftId'collectionNft . sp'nftId $ sp)
      curr = scriptCurrencySymbol policy'
      validator = marketplaceValidator curr
      scriptAddr = scriptAddress . validatorScript $ validator
  scriptUtxos <- getAddrUtxos scriptAddr
  pkh <- Contract.ownPaymentPubKeyHash
  let valHash = validatorHash validator
      newNft = (sp'nftId sp) {nftId'price = sp'price sp}
      oldName = mkTokenName . sp'nftId $ sp
      newName = mkTokenName newNft
      oldNftValue = singleton curr oldName (-1)
      newNftValue = singleton curr newName 1
      mintRedeemer = Redeemer . toBuiltinData $ ChangePrice (sp'nftId sp) (sp'price sp)
      lookup =
        Hask.mconcat
          [ Constraints.mintingPolicy policy'
          , Constraints.unspentOutputs scriptUtxos
          ]
      tx =
        Hask.mconcat
          [ Constraints.mustMintValueWithRedeemer mintRedeemer (newNftValue <> oldNftValue)
          , Constraints.mustBeSignedBy pkh
          , Constraints.mustPayToOtherScript valHash (Datum $ toBuiltinData ()) newNftValue
          ]
  void $ Contract.submitTxConstraintsWith @Any lookup tx
  Contract.tell . Hask.pure $ newNft
  Contract.logInfo @Hask.String $ printf "Set price successful: %s" (Hask.show $ assetClass curr newName)
