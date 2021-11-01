{-# LANGUAGE NoImplicitPrelude #-}
module Plutus.Contracts.NftMarketplace.OffChain.Serialization where

import           Data.ByteString    (ByteString)
import           Data.Text          (Text)
import qualified Data.Text.Encoding as T
import           PlutusTx.Prelude

deserializeByteString :: Text -> BuiltinByteString
deserializeByteString = toBuiltin . T.encodeUtf8

serializeByteString :: BuiltinByteString -> Text
serializeByteString = T.decodeUtf8 . fromBuiltin
