module Mlabs.EfficientNFT.Contract.MarketplaceSetPrice (marketplaceSetPrice) where

import PlutusTx.Prelude hiding (mconcat)
import Prelude qualified as Hask

import Control.Monad (void)
import Ledger (Datum (Datum), Redeemer (Redeemer), scriptAddress)
import Ledger.Constraints qualified as Constraints
import Ledger.Typed.Scripts (Any, validatorHash, validatorScript)
import Plutus.Contract qualified as Contract
import Plutus.V1.Ledger.Api (ToData (toBuiltinData))
import Plutus.V1.Ledger.Value (assetClass, assetClassValue, singleton, unAssetClass)
import Text.Printf (printf)

import Mlabs.EfficientNFT.Contract.Aux
import Mlabs.EfficientNFT.Marketplace
import Mlabs.EfficientNFT.Token
import Mlabs.EfficientNFT.Types

marketplaceSetPrice :: PlatformConfig -> SetPriceParams -> UserContract ()
marketplaceSetPrice _ sp = do
  let curr = fst . unAssetClass . nftId'assetClass . sp'nftId $ sp
      validator = marketplaceValidator curr
      scriptAddr = scriptAddress . validatorScript $ validator
  scriptUtxos <- getAddrUtxos scriptAddr
  pkh <- Contract.ownPaymentPubKeyHash
  let policy' = nftId'policy . sp'nftId $ sp
      valHash = validatorHash validator
      tn = mkTokenName pkh (sp'price sp)
      newNftValue = singleton curr tn 1
      oldNftValue = assetClassValue (nftId'assetClass . sp'nftId $ sp) (-1)
      ownerData = OwnerData pkh (sp'price sp)
      mintRedeemer = Redeemer . toBuiltinData $ ChangePrice ownerData (sp'price sp)
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
  Contract.tell . Hask.pure $
    NftId
      { nftId'assetClass = assetClass curr tn
      , nftId'policy = policy'
      , nftId'price = sp'price sp
      , nftId'owner = pkh
      , nftId'author = nftId'author . sp'nftId $ sp
      , nftId'authorShare = nftId'authorShare . sp'nftId $ sp
      }
  Contract.logInfo @Hask.String $ printf "Set price successful: %s" (Hask.show $ assetClass curr tn)
