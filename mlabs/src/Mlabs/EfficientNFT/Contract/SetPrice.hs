module Mlabs.EfficientNFT.Contract.SetPrice (setPrice) where

import PlutusTx.Prelude hiding (mconcat)
import Prelude qualified as Hask

import Control.Monad (void)
import Data.Void (Void)
import Ledger (Redeemer (Redeemer), minAdaTxOut, scriptCurrencySymbol)
import Ledger.Constraints qualified as Constraints
import Plutus.Contract qualified as Contract
import Plutus.V1.Ledger.Ada (toValue)
import Plutus.V1.Ledger.Api (ToData (toBuiltinData))
import Plutus.V1.Ledger.Value (assetClass, singleton)
import Text.Printf (printf)

import Mlabs.EfficientNFT.Token
import Mlabs.EfficientNFT.Types
import Mlabs.NFT.Contract.Aux (getUserUtxos)

setPrice :: SetPriceParams -> UserContract ()
setPrice sp = do
  pkh <- Contract.ownPaymentPubKeyHash
  utxos <- getUserUtxos
  let collection = nftData'nftCollection . sp'nftData $ sp
      policy' = policy collection
      curr = scriptCurrencySymbol policy'
      oldNft = nftData'nftId . sp'nftData $ sp
      newNft = oldNft {nftId'price = sp'price sp}
      oldName = mkTokenName oldNft
      newName = mkTokenName newNft
      oldNftValue = singleton curr oldName (-1)
      newNftValue = singleton curr newName 1
      mintRedeemer = Redeemer . toBuiltinData $ ChangePrice oldNft (sp'price sp)
      lookup =
        Hask.mconcat
          [ Constraints.mintingPolicy policy'
          , Constraints.unspentOutputs utxos
          , Constraints.ownPaymentPubKeyHash pkh
          ]
      tx =
        Hask.mconcat
          [ Constraints.mustMintValueWithRedeemer mintRedeemer (newNftValue <> oldNftValue)
          , Constraints.mustPayToPubKey pkh (newNftValue <> toValue minAdaTxOut)
          , Constraints.mustBeSignedBy pkh
          ]
  void $ Contract.submitTxConstraintsWith @Void lookup tx
  Contract.tell . Hask.pure $ NftData collection newNft
  Contract.logInfo @Hask.String $ printf "Set price successful: %s" (Hask.show $ assetClass curr newName)
