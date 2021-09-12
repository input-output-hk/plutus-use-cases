{-# LANGUAGE OverloadedStrings      #-}


import           Prelude
import           System.Environment

import           Data.Aeson                

import           Cardano.Api
import           Cardano.Api.Shelley

import qualified Cardano.Ledger.Alonzo.Data as Alonzo
import qualified Plutus.V1.Ledger.Api as Plutus

import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString as B

import qualified  Data.ByteString.Lazy.Char8 as LB8
import           Contracts.Oracle
import           Ledger (PubKeyHash, PubKey (..), Slot, pubKeyHash)

import           Wallet.Emulator.Types               (Wallet (..), walletPubKey)
main :: IO ()
main = do
    args <- getArgs
    let nargs = length args
    let scriptnum = if nargs > 0 then read (args!!0) else 42
    let fee = if nargs > 1 then read(args!!1) else  1000000
    let oracleScriptFile = "oracle.plutus"
        requestTokenScriptFile = "requesttoken.plutus"
    let pkMaybe = if nargs > 2 then decode(LB8.pack $ args!!2):: Maybe PubKey else Nothing

    case pkMaybe of 
        Nothing ->   putStrLn $ "Incorrect PK"
        Just pk -> do
            let pkh = pubKeyHash pk
            putStrLn $ "Writing output to: " ++ oracleScriptFile
            putStrLn $ "Writing output to: " ++ requestTokenScriptFile
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
