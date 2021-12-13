{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE NumericUnderscores     #-}

import           Prelude
import           System.Environment

import           Data.Aeson                

import           Cardano.Api
import           Cardano.Api.Shelley

import qualified Cardano.Ledger.Alonzo.Data     as Alonzo
import           Cardano.Crypto.Wallet          (xprv, xpub, XPub)
import qualified Plutus.V1.Ledger.Api           as Plutus

import qualified Data.ByteString.Short          as SBS
import qualified Data.ByteString                as B

import qualified  Data.ByteString.Lazy.Char8    as LB8
import           Contracts.Oracle.RequestToken
import           Contracts.Oracle.Types
import           Contracts.Oracle.OnChain
import           Contracts.Oracle.OffChain (UpdateOracleParams(..), RedeemOracleParams(..))
import           Ledger
import           Ledger.Ada                     as Ada
import           System.Exit                    (exitWith, ExitCode(..))
import           System.IO                      (hPutStrLn, stderr)

import           Wallet.Emulator.Types          (Wallet (..))
import           Types.Game

-- cabal exec -- gs 2000000 2000000 "keys/oracle/payment.vkey"
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
    
    let oracleVkeyPath = if nargs > 2 then args!!2  else ""
    oracleVkeyEither <- readFileTextEnvelope (AsVerificationKey AsPaymentExtendedKey) oracleVkeyPath
    oracleVkey <- either (\_ -> exitWithErrorMessage $ "Oracle Vkey not found") pure oracleVkeyEither
    let orackeXpubE = xpub $ serialiseToRawBytes oracleVkey
    oracleXpub <- either (\_ -> exitWithErrorMessage $ "cannot convert to oracle xpub") pure orackeXpubE
    let pk = xPubToPublicKey oracleXpub
    let pkh = pubKeyHash pk
    putStrLn $ "public key: " ++ show pk
    putStrLn $ "public key hash: " ++ show pkh
    putStrLn $ "fee: " ++ show fee
    putStrLn $ "collateral: " ++ show collateral 

    let useOr = RedeemOracleParams 
                  { roGame = 1}
    putStrLn $ show $ encode useOr
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

exitWithErrorMessage :: String -> IO a
exitWithErrorMessage str = hPutStrLn stderr str >> exitWith (ExitFailure 1)