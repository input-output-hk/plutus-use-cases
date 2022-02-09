module Mlabs.EfficientNFT.Contract.MarketplaceSetPrice (marketplaceSetPrice) where

import PlutusTx.Prelude hiding (mconcat)
import Prelude qualified as Hask

import Control.Monad (void)
import Data.Map qualified as Map
import Data.Monoid (mconcat)
import Ledger (Datum (Datum), Redeemer (Redeemer), minAdaTxOut, scriptHashAddress, _ciTxOutValue)
import Ledger.Constraints qualified as Constraints
import Ledger.Contexts (scriptCurrencySymbol)
import Ledger.Typed.Scripts (Any, validatorHash, validatorScript)
import Plutus.Contract qualified as Contract
import Plutus.V1.Ledger.Ada (toValue)
import Plutus.V1.Ledger.Api (ToData (toBuiltinData))
import Plutus.V1.Ledger.Value (assetClass, singleton, valueOf)
import Text.Printf (printf)

import Mlabs.EfficientNFT.Contract.Aux
import Mlabs.EfficientNFT.Marketplace
import Mlabs.EfficientNFT.Token
import Mlabs.EfficientNFT.Types

marketplaceSetPrice :: SetPriceParams -> UserContract ()
marketplaceSetPrice sp = do
  let collection = nftData'nftCollection . sp'nftData $ sp
      policy' = policy collection
      curr = scriptCurrencySymbol policy'
      valHash = validatorHash marketplaceValidator
      scriptAddr = scriptHashAddress valHash
      oldNft = nftData'nftId . sp'nftData $ sp
      newNft = oldNft {nftId'price = sp'price sp}
      oldName = mkTokenName oldNft
      newName = mkTokenName newNft
      oldNftValue = singleton curr oldName (-1)
      newNftValue = singleton curr newName 1
      mintRedeemer = Redeemer . toBuiltinData $ ChangePrice oldNft (sp'price sp)
      containsNft (_, tx) = valueOf (_ciTxOutValue tx) curr oldName == 1
  utxo' <- find containsNft . Map.toList <$> getAddrUtxos scriptAddr
  (utxo, utxoIndex) <- case utxo' of
    Nothing -> Contract.throwError "NFT not found on marketplace"
    Just x -> Hask.pure x
  pkh <- Contract.ownPaymentPubKeyHash
  userUtxos <- getUserUtxos
  Contract.logInfo @Hask.String $ printf "Script UTXOs: %s" (Hask.show . _ciTxOutValue $ utxoIndex)
  let userValues = mconcat . fmap _ciTxOutValue . Map.elems $ userUtxos
      lookup =
        Hask.mconcat
          [ Constraints.mintingPolicy policy'
          , Constraints.typedValidatorLookups marketplaceValidator
          , Constraints.otherScript (validatorScript marketplaceValidator)
          , Constraints.unspentOutputs $ Map.insert utxo utxoIndex userUtxos
          , Constraints.ownPaymentPubKeyHash pkh
          ]
      tx =
        Hask.mconcat
          [ Constraints.mustMintValueWithRedeemer mintRedeemer (newNftValue <> oldNftValue)
          , Constraints.mustBeSignedBy pkh
          , Constraints.mustSpendScriptOutput utxo (Redeemer . toBuiltinData $ Update)
          , Constraints.mustPayToOtherScript valHash (Datum $ toBuiltinData ()) (newNftValue <> toValue minAdaTxOut)
          , -- Hack to overcome broken balancing
            Constraints.mustPayToPubKey pkh (userValues - toValue (minAdaTxOut * 3))
          ]
  void $ Contract.submitTxConstraintsWith @Any lookup tx
  Contract.tell . Hask.pure $ NftData collection newNft
  Contract.logInfo @Hask.String $ printf "Marketplace set price successful: %s" (Hask.show $ assetClass curr newName)
