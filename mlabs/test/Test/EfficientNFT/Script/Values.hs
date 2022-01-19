module Test.EfficientNFT.Script.Values
  ( mintTxOutRef,
    authorPkh,
    platformPkh,
    nftPrice,
    tokenName,
    newPrice,
    newPriceTokenName,
    platformCfg,
    contentHash,
    otherPkh,
  )
where

import Data.Aeson (FromJSON, decode)
import Data.ByteString.Lazy (ByteString)
import Data.Maybe (fromJust)
import Ledger
  ( PaymentPubKeyHash (PaymentPubKeyHash),
    TokenName,
    TxOutRef (TxOutRef),
  )
import Mlabs.EfficientNFT.Token (mkTokenName)
import Mlabs.EfficientNFT.Types
  ( ContentHash,
    PlatformConfig (PlatformConfig, pcMarketplacePkh, pcMarketplaceShare),
  )
import PlutusTx.Natural (Natural)
import PlutusTx.Prelude

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

nftPrice :: Natural
nftPrice = toEnum 2_000_000

tokenName :: TokenName
tokenName = mkTokenName authorPkh nftPrice

newPrice :: Natural
newPrice = nftPrice + nftPrice

newPriceTokenName :: TokenName
newPriceTokenName = mkTokenName authorPkh newPrice

unsafeDecode :: FromJSON a => ByteString -> a
unsafeDecode = fromJust . decode

platformCfg :: PlatformConfig
platformCfg =
  PlatformConfig
    { pcMarketplacePkh = platformPkh,
      pcMarketplaceShare = nftPrice
    }

contentHash :: ContentHash
contentHash = sha2_256 "Some NFT content"
