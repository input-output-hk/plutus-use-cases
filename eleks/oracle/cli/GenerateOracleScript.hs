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
import           Contracts.Oracle.OnChain
import           Ledger
import           Ledger.Ada                     as Ada

import           Wallet.Emulator.Types          (Wallet (..), walletPubKey)

-- cabal exec -- gs 1500000 1000000 "payment.vkey"
main :: IO ()
main = do
    args <- getArgs
    let nargs = length args
    let feeInt = if nargs > 0 then read(args!!0) else 0
        fee = Ada.lovelaceOf feeInt
    let collateralInt = if nargs > 1 then read(args!!1) else 0
        collateral = Ada.lovelaceOf collateralInt

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
        putStrLn $ "fee: " ++ show fee
        putStrLn $ "collateral: " ++ show collateral 
        let oracleRequestTokenInfo = OracleRequestToken
                { ortOperator = pkh
                , ortFee = fee
                , ortCollateral = collateral
                }
        let oracle = Oracle
                { --oSymbol = opSymbol op
                  oRequestTokenSymbol = requestTokenSymbol oracleRequestTokenInfo
                , oOperator = pkh
                , oOperatorKey = pk
                , oFee = fee
                , oCollateral = collateral
                }

        writePlutusScript oracleScriptFile (oraclePlutusScript oracle) (oracleScriptAsShortBs oracle)
        writePlutusScript requestTokenScriptFile (mintingScript oracleRequestTokenInfo) (mintingScriptShortBs oracleRequestTokenInfo)

writePlutusScript :: FilePath -> PlutusScript PlutusScriptV1 -> SBS.ShortByteString -> IO ()
writePlutusScript filename scriptSerial scriptSBS =
  do
  case Plutus.defaultCostModelParams of
        Just m ->
          let Alonzo.Data pData = toAlonzoData (ScriptDataNumber 42)
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
