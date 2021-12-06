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
import           Contracts.Oracle.OffChain (UpdateOracleParams(..), UseOracleParams(..))
import           Ledger
import           Ledger.Ada                     as Ada

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
    
    let vkeyPath = if nargs > 2 then args!!2  else ""
    vkeyEither <- readFileTextEnvelope (AsVerificationKey AsPaymentExtendedKey) vkeyPath
    case vkeyEither of 
      Left _  -> putStrLn $ "Vkey not fouund"
      Right vkey -> do
        let pkE = xpub $ serialiseToRawBytes vkey
        case pkE of
          Left err -> putStrLn $ "XPub not found" ++ show err
          Right pkPub -> do
            let pk = xPubToPublicKey pkPub
            let pkh = pubKeyHash pk
            putStrLn $ "public key: " ++ show pk
            putStrLn $ "public key hash: " ++ show pkh
            putStrLn $ "fee: " ++ show fee
            putStrLn $ "collateral: " ++ show collateral 

            let updatePr =  UpdateOracleParams
                            { uoGameId  = 1   
                            , uoWinnerId = 55
                            , uoGameStatus = LIVE
                            }

            putStrLn $ show $ encode  updatePr

            let useOr = UseOracleParams 
                         { uoGame = 1}
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



 {-
readBech32Bip32SigningKeyFile
  :: SigningKeyFile
  -> IO (Either (FileError CardanoAddressSigningKeyConversionError) Crypto.XPrv)
readBech32Bip32SigningKeyFile (SigningKeyFile fp) = do
  eStr <- Exception.try $ readFile fp
  case eStr of
    Left e -> pure . Left $ FileIOError fp e
    Right str ->
      case decodeBech32 (Text.concat $ Text.words str) of
        Left err ->
          pure $ Left $
            FileError fp (CardanoAddressSigningKeyBech32DecodeError err)
        Right (_hrPart, _dataPart, bs) ->
          pure $ first (FileError fp) (convertBip32SigningKey bs)

-}