module Mlabs.EfficientNFT.Contract.ChangeOwner (changeOwner) where

import PlutusTx qualified
import PlutusTx.Prelude hiding (mconcat)
import Prelude qualified as Hask

import Control.Monad (void)
import Data.Void (Void)
import Ledger (Datum (Datum), Redeemer (Redeemer), scriptCurrencySymbol)
import Ledger.Constraints qualified as Constraints
import Plutus.Contract qualified as Contract
import Plutus.V1.Ledger.Ada (lovelaceValueOf)
import Plutus.V1.Ledger.Api (ToData (toBuiltinData))
import Plutus.V1.Ledger.Value (assetClass, singleton)
import PlutusTx.Numeric.Extra (addExtend)
import Text.Printf (printf)

import Mlabs.EfficientNFT.Contract.Aux
import Mlabs.EfficientNFT.Token
import Mlabs.EfficientNFT.Types

changeOwner :: ChangeOwnerParams -> UserContract ()
changeOwner cp = do
  utxos <- getUserUtxos
  let collection = nftData'nftCollection . cp'nftData $ cp
      policy' = policy collection
      curr = scriptCurrencySymbol policy'
      oldNft = nftData'nftId . cp'nftData $ cp
      newNft = oldNft {nftId'owner = cp'owner cp}
      oldName = mkTokenName oldNft
      newName = mkTokenName newNft
      oldNftValue = singleton curr oldName (-1)
      newNftValue = singleton curr newName 1
      nftPrice = nftId'price oldNft
      mintRedeemer = Redeemer . toBuiltinData $ ChangeOwner oldNft (cp'owner cp)
      getShare share = lovelaceValueOf $ (addExtend nftPrice * 10000) `divide` share
      authorShare = getShare (addExtend . nftCollection'authorShare $ collection)
      daoShare = getShare (addExtend . nftCollection'daoShare $ collection)
      ownerShare = lovelaceValueOf (addExtend nftPrice) - authorShare - daoShare
      datum = Datum . PlutusTx.toBuiltinData $ (curr, oldName)
      lookup =
        Hask.mconcat
          [ Constraints.mintingPolicy policy'
          , Constraints.unspentOutputs utxos
          ]
      tx =
        Hask.mconcat
          [ Constraints.mustMintValueWithRedeemer mintRedeemer (newNftValue <> oldNftValue)
          , Constraints.mustPayToPubKey (cp'owner cp) newNftValue
          , Constraints.mustPayWithDatumToPubKey (nftCollection'author collection) datum authorShare
          , Constraints.mustPayWithDatumToPubKey (nftId'owner oldNft) datum ownerShare
          , Constraints.mustPayToOtherScript (nftCollection'daoScript collection) datum daoShare
          ]
  void $ Contract.submitTxConstraintsWith @Void lookup tx
  Contract.tell . Hask.pure $ NftData collection newNft
  Contract.logInfo @Hask.String $ printf "Change owner successful: %s" (Hask.show $ assetClass curr newName)
