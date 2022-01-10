module Mlabs.EfficientNFT.Contract.Mint (mint) where

import PlutusTx.Prelude hiding (mconcat)
import Prelude qualified as Hask

import Control.Monad (void)
import Data.Map qualified as Map
import Data.Void (Void)
import Ledger (Redeemer (Redeemer))
import Ledger.Constraints qualified as Constraints
import Ledger.Contexts (scriptCurrencySymbol)
import Plutus.Contract qualified as Contract
import Plutus.V1.Ledger.Api (ToData (toBuiltinData))
import Plutus.V1.Ledger.Value (assetClass, singleton)
import Text.Printf (printf)

import Mlabs.EfficientNFT.Contract.Aux
import Mlabs.EfficientNFT.Token
import Mlabs.EfficientNFT.Types

mint :: PlatformConfig -> MintParams -> UserContract ()
mint pc mp = do
  pkh <- Contract.ownPubKeyHash
  (utxo, utxoIndex) <- getFirstUtxo
  let policy' = policy utxo pkh (mp'price mp) pc (getContent . mp'content $ mp)
      curr = scriptCurrencySymbol policy'
      tn = mkTokenName pkh (mp'price mp)
      nftValue = singleton curr tn 1
      ownerData = OwnerData pkh (mp'price mp)
      mintRedeemer = Redeemer . toBuiltinData . MintToken $ ownerData
      lookup =
        Hask.mconcat
          [ Constraints.mintingPolicy policy'
          , Constraints.unspentOutputs $ Map.singleton utxo utxoIndex
          ]
      tx =
        Hask.mconcat
          [ Constraints.mustMintValueWithRedeemer mintRedeemer nftValue
          , Constraints.mustBeSignedBy pkh
          ]
  void $ Contract.submitTxConstraintsWith @Void lookup tx
  Contract.tell . Hask.pure $ NftId (assetClass curr tn) policy'
  Contract.logInfo @Hask.String $ printf "Mint successful: %s" (Hask.show $ assetClass curr tn)
