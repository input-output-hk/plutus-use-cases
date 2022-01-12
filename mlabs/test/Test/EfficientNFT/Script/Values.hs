module Test.EfficientNFT.Script.Values (
  mintTxOutRef,
  authorPkh,
  platformPkh,
  nftPrice,
  tokenName
) where

import PlutusTx.Prelude

import Ledger (
  TokenName,
  PubKeyHash,
  TxOutRef(TxOutRef),
  TxId(TxId)
  )

import Data.Aeson (FromJSON, decode)
import Data.Maybe (fromJust)
import Data.ByteString.Lazy (ByteString)
import PlutusTx.Natural (Natural)
import Mlabs.EfficientNFT.Token (mkTokenName)

mintTxOutRef :: TxOutRef
mintTxOutRef = TxOutRef txId 1
  where
    txId = unsafeDecode $ 
          "{\"getTxId\" : \"3a9e96cbb9e2399046e7b653e29e2cc27ac88b3810b15f448b91425a9a27ef3a\"}"

authorPkh :: PubKeyHash
authorPkh = unsafeDecode $
              "{\"getPubKeyHash\" : \"25bd24abedaf5c68d898484d757f715c7b4413ad91a80d3cb0b3660d\"}"

platformPkh :: PubKeyHash
platformPkh = unsafeDecode $
              "{\"getPubKeyHash\" : \"bcd6bceeb0d22a7ca6ba1cd00669f7eb60ca8938d853666d30d56a56\"}"


nftPrice :: Natural
nftPrice =  toEnum 2_000_000

tokenName :: TokenName
tokenName = mkTokenName authorPkh nftPrice

unsafeDecode :: FromJSON a => ByteString -> a
unsafeDecode = fromJust . decode