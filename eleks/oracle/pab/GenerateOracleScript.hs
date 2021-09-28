{-# LANGUAGE OverloadedStrings      #-}


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
import           Contracts.Oracle.OnChain
import           Ledger
import           Ledger.Ada                     as Ada

import           Wallet.Emulator.Types          (Wallet (..), walletPubKey)

-- cabal exec -- gs 42 1500000 "payment.vkey"
main :: IO ()
main = do
    args <- getArgs
    let nargs = length args
    let scriptnum = if nargs > 0 then read (args!!0) else 42
    let feeInt = if nargs > 1 then read(args!!1) else 1000000
        fee = Ada.lovelaceOf feeInt
    let oracleScriptFile = "oracle.plutus"
        requestTokenScriptFile = "requesttoken.plutus"
  
    let vkeyPath = if nargs > 2 then args!!2  else ""
    vkeyEither <- readFileTextEnvelope (AsVerificationKey AsPaymentKey) vkeyPath
    case vkeyEither of 
      Left _  -> putStrLn $ "Vkey not fouund"
      Right vkey -> do
        let pk = PubKey $ Plutus.fromBytes $ serialiseToRawBytes vkey
        let pkh = pubKeyHash pk
        putStrLn $ "public key: " ++ show pk
        putStrLn $ "public key hash: " ++ show pkh

        let oracleRequestTokenInfo = OracleRequestToken
                { ortOperator = pkh
                , ortFee =fee
                }
        let oracle = Oracle
                { --oSymbol = opSymbol op
                  oRequestTokenSymbol = requestTokenSymbol oracleRequestTokenInfo
                , oOperator = pkh
                , oOperatorKey = pk
                , oFee = fee
                }
        let oracleData = OracleData
              { ovGame = 1
              , ovRequestAddress = pkh
              , ovSignedMessage = Nothing
              }

        writePlutusScript scriptnum oracleScriptFile (oraclePlutusScript oracle) (oracleScriptAsShortBs oracle)
        writePlutusScript scriptnum requestTokenScriptFile (mintingScript oracleRequestTokenInfo) (mintingScriptShortBs oracleRequestTokenInfo)

writePlutusScript :: Integer -> FilePath -> PlutusScript PlutusScriptV1 -> SBS.ShortByteString -> IO ()
writePlutusScript scriptnum filename scriptSerial scriptSBS =
  do
  case Plutus.defaultCostModelParams of
        Just m ->
          let Alonzo.Data pData = toAlonzoData (ScriptDataNumber scriptnum)
              (logout, e) = Plutus.evaluateScriptCounting Plutus.Verbose m scriptSBS [pData]
          in do print ("Log output" :: String) >> print logout
                case e of
                  Left evalErr -> print ("Eval Error" :: String) >> print evalErr
                  Right exbudget -> print ("Ex Budget" :: String) >> print exbudget
        Nothing -> error "defaultCostModelParams failed"
  result <- writeFileTextEnvelope filename Nothing scriptSerial
  case result of
    Left err -> print $ displayError err
    Right () -> return ()
