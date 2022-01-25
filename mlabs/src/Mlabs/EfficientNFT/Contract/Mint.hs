module Mlabs.EfficientNFT.Contract.Mint (mint, mintWithCollection) where

import PlutusTx.Prelude hiding (mconcat)
import Prelude qualified as Hask

import Control.Monad (void)
import Data.Text (pack)
import Data.Void (Void)
import Ledger (Datum (Datum), Redeemer (Redeemer), minAdaTxOut)
import Ledger.Constraints qualified as Constraints
import Ledger.Contexts (scriptCurrencySymbol)
import Ledger.Typed.Scripts (validatorHash)
import Plutus.Contract qualified as Contract
import Plutus.V1.Ledger.Ada (toValue)
import Plutus.V1.Ledger.Api (ToData (toBuiltinData), TokenName (TokenName))
import Plutus.V1.Ledger.Value (AssetClass, assetClass, assetClassValue, singleton, unAssetClass)
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
  ac <- generateNft
  mintWithCollection (ac, mp)

mintWithCollection :: (AssetClass, MintParams) -> UserContract ()
mintWithCollection (ac, mp) = do
  pkh <- Contract.ownPaymentPubKeyHash
  utxos <- getUserUtxos
  let nft =
        NftId
          { nftId'price = mp'price mp
          , nftId'owner = pkh
          , nftId'collectionNftTn = snd . unAssetClass $ ac
          }
      collection =
        NftCollection
          { nftCollection'collectionNftCs = fst . unAssetClass $ ac
          , nftCollection'lockingScript = validatorHash burnValidator
          , nftCollection'author = pkh
          , nftCollection'authorShare = mp'share mp
          , nftCollection'marketplaceScript = validatorHash marketplaceValidator
          , nftCollection'marketplaceShare = toEnum 5
          }
      policy' = policy collection
      curr = scriptCurrencySymbol policy'
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
          , Constraints.mustPayToPubKey pkh (nftValue <> toValue minAdaTxOut)
          , Constraints.mustPayToOtherScript
              (nftCollection'lockingScript collection)
              (Datum $ toBuiltinData ())
              (assetClassValue ac 1 <> toValue minAdaTxOut)
          ]
  void $ Contract.submitTxConstraintsWith @Void lookup tx
  Contract.tell . Hask.pure $ NftData collection nft
  Contract.logInfo @Hask.String $ Hask.show nft
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
