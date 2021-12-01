module Contracts.Oracle.Conversion
  where

import           Codec.Binary.Bech32 ( HumanReadablePart, humanReadablePartToText )
import           Codec.Binary.Encoding( AbstractEncoding (..), Encoding, encode, fromBech32)
import qualified Cardano.Codec.Bech32.Prefixes as CIP5
import           Control.Monad                    (when)
import           Data.ByteString( ByteString )
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Base64           as B64
import qualified Data.ByteString.Char8 as B8
import           Cardano.Address.Derivation ( XPub, xprvFromBytes, xpubFromBytes, xprvToBytes )
import          Ledger.Crypto (PrivateKey, PubKey)
import          Cardano.Crypto.Wallet  (XPrv, unXPrv, xprv)

getBech32 :: String -> Either String (HumanReadablePart, ByteString)
getBech32 raw = do
    (hrp, bytes) <- fromBech32 (const id) (B8.pack raw)
    when (hrp `notElem` allowedPrefixes) $ Left
        $ "Invalid human-readable prefix. Prefix ought to be one of: "
        <> show (showHrp <$> allowedPrefixes)
    return (hrp, bytes)
  where
    decode :: (String -> Either String (HumanReadablePart, ByteString)) 
      -> String 
      -> IO (HumanReadablePart, ByteString)
    decode from = either fail pure . from

    showHrp :: HumanReadablePart -> String
    showHrp = T.unpack . humanReadablePartToText

    allowedPrefixes =
        [ CIP5.addr_xsk]

-- | Read an encoded private key, or fail.
getXPrv :: String -> Either String XPrv
getXPrv h = do
  (hrp, bytes) <- getBech32 h
  case xprvFromBytes bytes of
    Nothing  -> Left "Couldn't convert bytes into extended private key."
    Just key -> pure key

-- | Read an encoded public key or fail.
getXPub :: String -> Either String XPub
getXPub h = do
  (hrp, bytes) <- getBech32 h
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