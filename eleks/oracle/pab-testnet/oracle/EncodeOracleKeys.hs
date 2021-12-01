{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NumericUnderscores  #-}
import           Prelude
import           Data.Maybe                         (fromMaybe)
import           Data.Aeson
import           Data.Aeson.Types
import           Data.ByteString                 (ByteString)            
import           Cardano.Address.Derivation     (xprvFromBytes, toXPub , XPub (..))
import qualified Cardano.Codec.Bech32.Prefixes as CIP5
import           Cardano.Crypto.Wallet  (XPrv, unXPrv)
import           Contracts.Oracle
import           System.IO                      (hPutStrLn, stderr, stdout, stdin)
import System.IO.Extra ( hGetXP__, hPutBytes )
import Codec.Binary.Encoding
    ( 
    fromBech32
    )
import           System.Environment
import Ledger.Crypto (PrivateKey, PubKey)
import           Ledger.Crypto             (toPublicKey, pubKeyHash)
import           Wallet.Emulator.Wallet             (emptyWalletState, ownPublicKey, ownPrivateKey)
import Wallet.Emulator.Types (Wallet (Wallet))
import           Data.Text                 (Text, pack)
import Codec.Binary.Bech32
    ( HumanReadablePart, humanReadablePartToText )
import Codec.Binary.Encoding
    ( AbstractEncoding (..)
    , Encoding
    , encode
    , fromBech32
    )
import qualified Cardano.Codec.Bech32.Prefixes as CIP5
import           Control.Monad                    (when)
import Data.ByteString
    ( ByteString )
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Char8 as B8
import System.IO
    ( stdin, stdout )
import Ledger.Crypto (PrivateKey, PubKey)
--import Control.Monad.Freer.Error (Error, throwError)
import Control.Exception (throw)
import           Wallet.Emulator        (Wallet(..), walletPubKeyHash)
import           Plutus.Contract.Test    (w3) 
import System.IO.Error (userError)

import qualified Ledger.CardanoWallet  as CW

oraclePrivateKey :: PrivateKey
oraclePrivateKey = CW.privateKey $ CW.fromWalletNumber $ CW.WalletNumber 2

instance Show XPrv where 
    show xprv = show $ B8.unpack $ unXPrv xprv

-- cabal exec -- encode-oracle-keys "addr_xsk1fqnzh9c4d2g8mpgp7zpyhq8jz9l0h3alr2zr2u20qpsesh056azzv7w93qpt4s2d29r6vsmm9wtavklxrz5smy5d99fyvrk49s2ef48q662p7xx7wjjrxrv44gk57j2tzp4wytdcjx3emwa8t6funtvhj5qqlqyw"

main :: IO ()
main = do
  args <- getArgs
  let nargs = length args
  let keyBep = if nargs > 0 then args!!0  else ""
  let decodedPrvE = getXPrv keyBep
  decodePrv:: XPrv <- either (ioError . userError ) pure decodedPrvE
  let dtoEncoded = encodeKeyToDto decodePrv
  putStrLn "encoded prv:" 
  putStrLn $ show dtoEncoded

  let params =  OracleParams
                { opFees =2_000_000
                , opCollateral = 2_000_000
                , opSigner = dtoEncoded
                }
  putStrLn $ show $ Data.Aeson.encode params
--   decodePriv:: XPrv <- either (ioError . userError ) pure decodePrivE
--   let strPriv1 = privateKeyToString $ decodePriv
--   let hash = pubKeyHash $ toPublicKey oraclePrivateKey
--   putStrLn $ "original pr : " ++ show oraclePrivateKey
--   putStrLn $ "original pub hash : " ++ show hash
--   putStrLn $ "original pub hashx: " ++ (show $ CW.pubKeyHash $ CW.fromWalletNumber $ CW.WalletNumber 2)
--   putStrLn $ "original pub: " ++ (show $ toPublicKey oraclePrivateKey)
--   let strPriv = privateKeyToString $ oraclePrivateKey
--   putStrLn $ "ecrypted priv str: " ++ show strPriv
--   let decodePrivE = getXPrv strPriv
--   decodePriv:: XPrv <- either (ioError . userError ) pure decodePrivE
--   let strPriv1 = privateKeyToString $ decodePriv
--   putStrLn $ "ecrypted priv str: " ++ show strPriv1
--   let decodeHash = pubKeyHash $ toPublicKey decodePriv 
--   putStrLn $ "original pub hash: " ++ show decodeHash
--   let strPriv2 = privateKeyToString $ decodePriv
--   let decodePrivE2 = getXPrv strPriv2
--   decodePriv2:: XPrv <- either (ioError . userError ) pure decodePrivE2
--   let decodeHash2 = pubKeyHash $ toPublicKey decodePriv2 
--   putStrLn $ "original pub hash: " ++ show decodeHash2
--   putStrLn $ "original pub: " ++ (show $ toPublicKey decodePriv2)
--   putStrLn $ "original pub: " ++ (show $ decodePriv2)
  


--   let strPriv =  "addr_xsk1fqnzh9c4d2g8mpgp7zpyhq8jz9l0h3alr2zr2u20qpsesh056azzv7w93qpt4s2d29r6vsmm9wtavklxrz5smy5d99fyvrk49s2ef48q662p7xx7wjjrxrv44gk57j2tzp4wytdcjx3emwa8t6funtvhj5qqlqyw"
--   putStrLn $ "ecrypted priv: " ++ show strPriv
--   let decodePrivE = getXPrv strPriv
--   decodePriv:: XPrv <- either (ioError . userError ) pure decodePrivE

--   let hash = pubKeyHash $ toPublicKey decodePriv
--   putStrLn $ "original pub hash: " ++ show hash
--   let strPriv1 = privateKeyToString decodePriv
--   putStrLn $ "dcrypted priv: " ++ show strPriv1 
--   let decodePrivE1 = getXPrv strPriv1
--   decodePriv1:: XPrv <- either (ioError . userError ) pure decodePrivE1
--   let hash1 = pubKeyHash $ toPublicKey decodePriv1
--   putStrLn $ "original pub hash: " ++ show hash1

  
  -- () <- hGetXP__ stdin allowedPrefixes >>= \case
  --   Left (hrp, xpub) -> do
  --     hPutStrLn stdout "PubKey"
  --     let pub:: XPub = xpub
  --     hPutStrLn stdout $ show $ encode pub

  --   Right (hrp, xprv) -> do
  --     hPutStrLn stdout "PrivKey"
  --     -- hPutStrLn stdout $ show $  encode xprv
  -- hPutStrLn stdout "Ura"

  where
    allowedPrefixes =
      [ CIP5.root_xsk
      , CIP5.acct_xsk
      , CIP5.acct_xvk
      , CIP5.root_shared_xsk
      , CIP5.acct_shared_xsk
      , CIP5.acct_shared_xvk
      ]

-- oraclePrivateKey :: PrivateKey
-- oraclePrivateKey = getWalletPrivKey oracleWallet

-- getWalletPrivKey:: Wallet -> PrivateKey
-- getWalletPrivKey = ownPrivateKey . fromMaybe (error "not a mock wallet") . emptyWalletState

-- oracleWallet:: Wallet
-- oracleWallet = w3
