{-# LANGUAGE ImportQualifiedPost #-}

module Contracts.Oracle.Conversion
  where

import Cardano.Address.Derivation (XPub, xprvFromBytes, xprvToBytes, xpubFromBytes)
import Cardano.Codec.Bech32.Prefixes qualified as CIP5
import Cardano.Crypto.Wallet (XPrv, unXPrv, xprv)
import Codec.Binary.Bech32 (HumanReadablePart, humanReadablePartToText)
import Codec.Binary.Encoding (AbstractEncoding (..), encode, fromBech32)
import Control.Monad (when)
import Data.ByteString (ByteString)
import Data.ByteString.Base64 qualified as B64
import Data.ByteString.Char8 qualified as B8
import Data.Text qualified as T
import Ledger.Crypto (PrivateKey)

getBech32 :: String -> Either String (HumanReadablePart, ByteString)
getBech32 raw = do
    (hrp, bytes) <- fromBech32 (const id) (B8.pack raw)
    when (hrp `notElem` allowedPrefixes) $ Left
        $ "Invalid human-readable prefix. Prefix ought to be one of: "
        <> show (showHrp <$> allowedPrefixes)
    return (hrp, bytes)
  where
    showHrp :: HumanReadablePart -> String
    showHrp = T.unpack . humanReadablePartToText

    allowedPrefixes =
        [ CIP5.addr_xsk]

-- | Read an encoded private key, or fail.
getXPrv :: String -> Either String XPrv
getXPrv h = do
  (_, bytes) <- getBech32 h
  case xprvFromBytes bytes of
    Nothing  -> Left "Couldn't convert bytes into extended private key."
    Just key -> pure key

-- | Read an encoded public key or fail.
getXPub :: String -> Either String XPub
getXPub h = do
  (_, bytes) <- getBech32 h
  case xpubFromBytes bytes of
    Nothing  -> Left "Couldn't convert bytes into extended public key."
    Just key -> pure key

privateKeyToString :: PrivateKey -> String
privateKeyToString prv = do
  B8.unpack $ encode (EBech32 CIP5.addr_xsk) (xprvToBytes prv)

encodeKeyToDto :: XPrv -> String
encodeKeyToDto = B8.unpack . B64.encode . unXPrv

decodeKeyFromDto :: String -> Either String XPrv
decodeKeyFromDto = xprv . B64.decodeLenient . B8.pack
