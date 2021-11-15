{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE NumericUnderscores     #-}

import           Prelude
import           System.Environment

import           Data.Aeson                

import           Cardano.Api
import           Cardano.Api.Shelley

import qualified Cardano.Ledger.Alonzo.Data     as Alonzo
import qualified Plutus.V1.Ledger.Api           as Plutus

import qualified Data.ByteString.Short          as SBS
import qualified Data.ByteString                as B

import qualified  Data.ByteString.Lazy.Char8    as LB8
import           Contracts.Oracle.RequestToken
import           Contracts.Oracle.Types
import           Ledger.Oracle                  (SignedMessage(..), signMessage)
import           Types.Game
import           Contracts.Oracle.OnChain
import           Ledger
import           Ledger.Ada                     as Ada

import           Wallet.Emulator.Types          (Wallet (..), walletPubKey)
import           Ledger.Crypto                  (PrivateKey(..))
import           Cardano.Crypto.Wallet           (xprv)

{-
First request 
cabal exec -- encode-oracle-request 1 \
"demo/keys/client/payment.vkey" 

Oracle Update
cabal exec -- encode-oracle-request 1 \
"demo/keys/client/payment.vkey" \
"demo/keys/oracle/payment.skey" \
0 FT

-}

main :: IO ()
main = do
    args <- getArgs
    let nargs = length args
    let gameId = if nargs > 0 then read (args!!0) else 0
    let requestVkeyPath = if nargs > 1 then args!!1  else ""
    let oracleSignKey = if nargs > 2 then args!!2  else ""
    let winnerId = if nargs > 3 then read (args!!3) else 0
    -- let status = if nargs > 4 then decode (args!!4) else FT
    let status = FT
    requestVkeyEither <- readFileTextEnvelope (AsVerificationKey AsPaymentKey) requestVkeyPath
    signerKeyEither <- readFileTextEnvelope (AsSigningKey AsPaymentKey) oracleSignKey
    putStrLn $ show signerKeyEither

    case requestVkeyEither of 
      Left _  -> putStrLn $ "Vkey not found"
      Right requestVkey -> do
        let pk = PubKey $ Plutus.fromBytes $ serialiseToRawBytes requestVkey
        let pkh = pubKeyHash pk

        case signerKeyEither of
            Left _ -> do
                showData gameId pkh Nothing
            Right sKey -> do
                putStrLn $ show $ serialiseToRawBytes sKey
                let signKeyE = xprv $ serialiseToRawBytes sKey
                case signKeyE of 
                    Left error -> do 
                        putStrLn $error
                        showData gameId pkh Nothing
                    Right signKey -> do
                        let message = OracleSignedMessage{
                            osmWinnerId = winnerId,
                            osmGameId = gameId,
                            osmGameStatus = FT
                        }
                        let signedMessage = Just $ signMessage message signKey
                        showData gameId pkh signedMessage
            
showData:: GameId -> PubKeyHash -> Maybe (SignedMessage OracleSignedMessage) -> IO ()
showData gameId pkh signeMessage = do
    let od = OracleData { 
                ovRequestAddress = pkh,
                ovGame = gameId,
                ovSignedMessage = signeMessage
                }
    let oracleData = encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData $ Plutus.toData od)
    print (oracleData)