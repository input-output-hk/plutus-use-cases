{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE NumericUnderscores     #-}
{-# LANGUAGE ScopedTypeVariables    #-}

import           Prelude
import           System.Environment

import           Data.Aeson                

import           Cardano.Api
import           Cardano.Api.Shelley

import qualified Cardano.Ledger.Alonzo.Data     as Alonzo
import qualified Plutus.V1.Ledger.Api           as Plutus
import           Plutus.V1.Ledger.Bytes ( LedgerBytes (..))
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
  let gameId = if nargs > 0 then read (args!!0) else 0
  let clientVkeyPath = if nargs > 1 then args!!1  else ""
  let oracleSignKeyPath = if nargs > 2 then args!!2  else ""
  let winnerId = if nargs > 3 then read (args!!3) else 0
  let statusM = if nargs > 4 then decode (LB8.pack $ args!!4) else Just NS
  status <- maybe(exitWithErrorMessage "Wrong status") pure statusM
  clientVkeyEither <- readFileTextEnvelope (AsVerificationKey AsPaymentExtendedKey) clientVkeyPath
 
  clientVkey <- either (\_ -> exitWithErrorMessage $ "Oracle Vkey not found") pure clientVkeyEither
  let clientXpubE = xpub $ serialiseToRawBytes clientVkey
  clientXpub <- either (\_ -> exitWithErrorMessage $ "cannot convert to oracle xpub") pure clientXpubE
  let pk = xPubToPublicKey clientXpub
  let pkh = pubKeyHash pk
  if oracleSignKeyPath == "" 
    then
      showData gameId pkh Nothing
    else do
      signerKeyEither <- readFileTextEnvelope (AsSigningKey AsPaymentExtendedKey) oracleSignKeyPath
      signSkey <- either (\_ -> exitWithErrorMessage $ "sign Skey not found") pure signerKeyEither
      let signXprvE = xprv $ serialiseToRawBytes signSkey
      signXprv <- either (\_ -> exitWithErrorMessage $ "sign XPrv not found") pure signXprvE
      let message = OracleSignedMessage{
          osmWinnerId = winnerId,
          osmGameId = gameId,
          osmGameStatus = status
      }
      let signedMessage = signMessage message signXprv ""

      showData gameId pkh (Just signedMessage)

showData:: GameId -> PubKeyHash -> Maybe (SignedMessage OracleSignedMessage) -> IO ()
showData gameId pkh signeMessage = do
    let od = OracleData { 
                ovRequestAddress = pkh,
                ovGame = gameId,
                ovSignedMessage = signeMessage
                }
    let oracleData = encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData $ Plutus.toData od)
    print (oracleData)

exitWithErrorMessage :: String -> IO a
exitWithErrorMessage str = hPutStrLn stderr str >> exitWith (ExitFailure 1)