module Test.EfficientNFT.Script.Values (
  authorPkh,
  burnHash,
  collectionNft,
  mintTxOutRef,
  nft1,
  newPriceNft1,
  otherPkh,
  platformPkh,
  tokenName,
  newPriceTokenName,
) where

import PlutusTx.Prelude

import Ledger (
  AssetClass,
  PaymentPubKeyHash (PaymentPubKeyHash),
  TokenName,
  TxOutRef (TxOutRef),
  ValidatorHash,
 )
import Plutus.V1.Ledger.Value (assetClass)

import Data.Aeson (FromJSON, decode)
import Data.ByteString.Lazy (ByteString)
import Data.Maybe (fromJust)
import Ledger.Typed.Scripts (validatorHash)

import Mlabs.EfficientNFT.Burn
import Mlabs.EfficientNFT.Marketplace
import Mlabs.EfficientNFT.Token (mkTokenName)
import Mlabs.EfficientNFT.Types

mintTxOutRef :: TxOutRef
mintTxOutRef = TxOutRef txId 1
  where
    txId =
      unsafeDecode
        "{\"getTxId\" : \"3a9e96cbb9e2399046e7b653e29e2cc27ac88b3810b15f448b91425a9a27ef3a\"}"

authorPkh :: PaymentPubKeyHash
authorPkh =
  PaymentPubKeyHash $
    unsafeDecode
      "{\"getPubKeyHash\" : \"25bd24abedaf5c68d898484d757f715c7b4413ad91a80d3cb0b3660d\"}"

platformPkh :: PaymentPubKeyHash
platformPkh =
  PaymentPubKeyHash $
    unsafeDecode
      "{\"getPubKeyHash\" : \"bcd6bceeb0d22a7ca6ba1cd00669f7eb60ca8938d853666d30d56a56\"}"

otherPkh :: PaymentPubKeyHash
otherPkh =
  PaymentPubKeyHash $
    unsafeDecode
      "{\"getPubKeyHash\" : \"75bd24abfdaf5c68d898484d757f715c7b4413ad91a80d3cb0b3660d\"}"

tokenName :: TokenName
tokenName = mkTokenName nft1

newPriceTokenName :: TokenName
newPriceTokenName = mkTokenName newPriceNft1

-- newPrice :: Natural
-- newPrice = nftPrice + nftPrice

-- newPriceTokenName :: TokenName
-- newPriceTokenName = mkTokenName authorPkh newPrice

unsafeDecode :: FromJSON a => ByteString -> a
unsafeDecode = fromJust . decode

collectionNft :: AssetClass
collectionNft = assetClass "abcd" "NFT"

nft1 :: NftId
nft1 =
  NftId
    { nftId'content = Content "NFT content"
    , nftId'price = toEnum 10_000_000
    , nftId'owner = authorPkh
    , nftId'author = authorPkh
    , nftId'authorShare = toEnum 10
    , nftId'collectionNft = collectionNft
    , nftId'marketplaceValHash = validatorHash . marketplaceValidator $ "ff"
    , nftId'marketplaceShare = toEnum 5
    }

newPriceNft1 :: NftId
newPriceNft1 = nft1 {nftId'price = nftId'price nft1 * toEnum 2}

burnHash :: ValidatorHash
burnHash = validatorHash burnValidator
