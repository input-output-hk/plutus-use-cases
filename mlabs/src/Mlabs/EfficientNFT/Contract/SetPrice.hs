module Mlabs.EfficientNFT.Contract.SetPrice (setPrice) where

import PlutusTx.Prelude hiding (mconcat)
import Prelude qualified as Hask

import Control.Monad (void)
import Data.Map qualified as Map
import Data.Void (Void)
import Ledger (Redeemer (Redeemer))
import Ledger.Constraints qualified as Constraints
import Plutus.Contract qualified as Contract
import Plutus.V1.Ledger.Api (ToData (toBuiltinData))
import Plutus.V1.Ledger.Value (assetClass, assetClassValue, singleton, unAssetClass)
import Text.Printf (printf)

import Mlabs.EfficientNFT.Contract.Aux
import Mlabs.EfficientNFT.Token
import Mlabs.EfficientNFT.Types

setPrice :: PlatformConfig -> SetPriceParams -> UserContract ()
setPrice _ sp = do
  pkh <- Contract.ownPubKeyHash
  (utxo, utxoIndex) <- getFirstUtxo
  let policy' = nftId'policy . sp'nftId $ sp
      curr = fst . unAssetClass . nftId'assetClass . sp'nftId $ sp
      tn = mkTokenName pkh (sp'price sp)
      newNftValue = singleton curr tn 1
      oldNftValue = assetClassValue (nftId'assetClass . sp'nftId $ sp) (-1)
      ownerData = OwnerData pkh (sp'price sp)
      mintRedeemer = Redeemer . toBuiltinData $ ChangePrice ownerData (sp'price sp)
      lookup =
        Hask.mconcat
          [ Constraints.mintingPolicy policy'
          , Constraints.unspentOutputs $ Map.singleton utxo utxoIndex
          ]
      tx =
        Hask.mconcat
          [ Constraints.mustMintValueWithRedeemer mintRedeemer (newNftValue <> oldNftValue)
          , Constraints.mustBeSignedBy pkh
          ]
  void $ Contract.submitTxConstraintsWith @Void lookup tx
  Contract.tell . Hask.pure $ NftId (assetClass curr tn) policy'
  Contract.logInfo @Hask.String $ printf "Set price successful: %s" (Hask.show $ assetClass curr tn)
