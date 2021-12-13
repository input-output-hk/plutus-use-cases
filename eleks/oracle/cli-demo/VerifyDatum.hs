{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE NumericUnderscores     #-}
{-# LANGUAGE ScopedTypeVariables    #-}

import           Prelude
import           System.Environment

import           Data.Aeson                
import           Data.Maybe (Maybe(..))
import           Data.Either (fromRight)

import           Cardano.Api
import           Cardano.Api.Shelley

import qualified Cardano.Ledger.Alonzo.Data     as Alonzo
import qualified Plutus.V1.Ledger.Api           as Plutus
import           Plutus.V1.Ledger.Bytes         ( LedgerBytes (..))
import qualified Data.ByteString.Short          as SBS
import qualified Data.ByteString                as B
import qualified Data.ByteString.Lazy.Char8     as LB8
import           Data.Text                      (Text, pack)
import           Contracts.Oracle.RequestToken
import           Contracts.Oracle.Types
import           Plutus.Contract.Oracle         (SignedMessage(..), signMessage, verifySignedMessageConstraints, checkHashConstraints)
import           Types.Game
import           Contracts.Oracle.OnChain
import           Ledger
import           Ledger.Ada                     as Ada
-- import qualified Data.ByteString.UTF8           as BSU 
import           Wallet.Emulator.Types          (Wallet (..))
import           Ledger.Crypto                  (PrivateKey(..), toPublicKey, pubKeyHash, xPubToPublicKey)
import           Cardano.Crypto.Wallet          (xprv, xpub, XPub)
import           System.Exit                    (exitWith, ExitCode(..))
import           System.IO                      (hPutStrLn, stderr)
import qualified PlutusTx 
{-
First request 
cabal exec -- encode-oracle-request 1 \
"keys/client/payment.vkey" 

Oracle Update
cabal exec -- encode-oracle-request 1 \
"keys/client/payment.vkey" \
"keys/oracle/payment.skey" \
0 FT

cabal exec -- encode-oracle-request 1 \
"keys/client/payment.vkey" \
"keys/oracle/payment.skey" \
0 FT
-}

main :: IO ()
main = do
  args <- getArgs
  let nargs = length args
  let datumM:: Maybe Datum= if nargs > 0 then decode (LB8.pack $ args!!0) else Nothing
  let oracleVerKeyPath = if nargs > 1 then args!!1  else ""
  (Datum e)  <- maybe(exitWithErrorMessage "Wrong datum") pure datumM
  print (e)

  oracleData::OracleData <- maybe (exitWithErrorMessage "datum has wrong type")
            pure
            (PlutusTx.fromBuiltinData e)
  print (show oracleData)
  requestVkeyEither <- readFileTextEnvelope (AsVerificationKey AsPaymentExtendedKey) oracleVerKeyPath
  requestVkey <- either (\_ -> exitWithErrorMessage $ "Vkey not found") pure requestVkeyEither
  let xpubE = xpub $ serialiseToRawBytes requestVkey
  xpub <- either (\_ -> exitWithErrorMessage $ "cannot convert to xpub") pure xpubE
  let oraclePubKey = xPubToPublicKey xpub
  let signedMessageM = ovSignedMessage oracleData
  signedMessage <- maybe (exitWithErrorMessage "no signed message in datum") pure signedMessageM
  case verifySignedMessageConstraints oraclePubKey signedMessage of
    Left err -> exitWithErrorMessage $ "verify error: " ++ show err
    Right _ -> print "verify success"

exitWithErrorMessage :: String -> IO a
exitWithErrorMessage str = hPutStrLn stderr str >> exitWith (ExitFailure 1)