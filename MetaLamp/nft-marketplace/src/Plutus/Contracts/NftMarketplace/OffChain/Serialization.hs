{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
module Plutus.Contracts.NftMarketplace.OffChain.Serialization where

import qualified Data.Aeson         as J
import qualified Data.Aeson.Types   as J
import           Data.Text          (Text)
import qualified Data.Text.Encoding as T
import qualified GHC.Generics       as Haskell
import qualified PlutusTx           as PlutusTx
import           PlutusTx.Prelude
import qualified Prelude            as Haskell

newtype PlutusBuiltinByteString = PlutusBuiltinByteString { getPlutusBuiltinByteString :: BuiltinByteString }
    deriving (Haskell.Eq, Haskell.Show, Haskell.Generic)

PlutusTx.unstableMakeIsData ''PlutusBuiltinByteString

PlutusTx.makeLift ''PlutusBuiltinByteString

instance J.ToJSON PlutusBuiltinByteString where
  toJSON (PlutusBuiltinByteString s) = J.String (serializeByteString s)

instance J.FromJSON PlutusBuiltinByteString where
  parseJSON (J.String s) = Haskell.pure . PlutusBuiltinByteString . deserializeByteString $ s
  parseJSON invalid = J.prependFailure "parsing PlutusBuiltinByteString failed, " (J.typeMismatch "String" invalid)

deserializePlutusBuiltinBS :: Text -> PlutusBuiltinByteString
deserializePlutusBuiltinBS = PlutusBuiltinByteString . deserializeByteString

deserializeByteString :: Text -> BuiltinByteString
deserializeByteString = toBuiltin . T.encodeUtf8

serializeByteString :: BuiltinByteString -> Text
serializeByteString = T.decodeUtf8 . fromBuiltin

