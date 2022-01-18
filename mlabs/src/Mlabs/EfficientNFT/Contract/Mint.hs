module Mlabs.EfficientNFT.Contract.Mint (mint) where

import PlutusTx.Prelude hiding (mconcat)
import Prelude qualified as Hask

import Control.Monad (void)
import Data.Text (pack)
import Data.Void (Void)
import Ledger (Redeemer (Redeemer))
import Ledger.Constraints qualified as Constraints
import Ledger.Contexts (scriptCurrencySymbol)
import Ledger.Typed.Scripts (validatorHash)
import Plutus.Contract qualified as Contract
import Plutus.V1.Ledger.Api (ToData (toBuiltinData), TokenName (TokenName))
import Plutus.V1.Ledger.Value (AssetClass, assetClass, singleton)
import Text.Printf (printf)

{- Drop-in replacement for
import Plutus.Contracts.Currency (CurrencyError, mintContract)
import Plutus.Contracts.Currency qualified as MC
till it will be fixed, see `Mlabs.Plutus.Contracts.Currency.mintContract`
for details -}
import Mlabs.Plutus.Contracts.Currency (CurrencyError, mintContract)
import Mlabs.Plutus.Contracts.Currency qualified as MC

import Mlabs.EfficientNFT.Burn (burnValidator)
import Mlabs.EfficientNFT.Contract.Aux
import Mlabs.EfficientNFT.Marketplace
import Mlabs.EfficientNFT.Token
import Mlabs.EfficientNFT.Types

mint :: MintParams -> UserContract ()
mint mp = do
  pkh <- Contract.ownPaymentPubKeyHash
  utxos <- getUserUtxos
  ac <- generateNft
  let burnHash = validatorHash burnValidator
      policy' = policy burnHash Nothing ac
      curr = scriptCurrencySymbol policy'
      nft =
        NftId
          { nftId'content = mp'content mp
          , nftId'price = mp'price mp
          , nftId'owner = pkh
          , nftId'author = pkh
          , nftId'authorShare = mp'share mp
          , nftId'collectionNft = ac
          , nftId'marketplaceValHash = validatorHash . marketplaceValidator $ curr
          , nftId'marketplaceShare = toEnum 5
          }
      tn = mkTokenName nft
      nftValue = singleton curr tn 1
      mintRedeemer = Redeemer . toBuiltinData . MintToken $ nft
      lookup =
        Hask.mconcat
          [ Constraints.mintingPolicy policy'
          , Constraints.unspentOutputs utxos
          ]
      tx =
        Hask.mconcat
          [ Constraints.mustMintValueWithRedeemer mintRedeemer nftValue
          , Constraints.mustPayToPubKey pkh nftValue
          ]
  void $ Contract.submitTxConstraintsWith @Void lookup tx
  Contract.tell . Hask.pure $ nft
  Contract.logInfo @Hask.String $ printf "Mint successful: %s" (Hask.show $ assetClass curr tn)

generateNft :: GenericContract AssetClass
generateNft = do
  self <- Contract.ownPaymentPubKeyHash
  let tn = TokenName "NFT"
  x <-
    Contract.mapError
      (pack . Hask.show @CurrencyError)
      (mintContract self [(tn, 1)])
  return $ assetClass (MC.currencySymbol x) tn
