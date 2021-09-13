{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
import qualified Plutus.V1.Ledger.Api as Plutus
import Cardano.Api (PlutusScript, PlutusScriptV1, ScriptData (ScriptDataNumber), writeFileTextEnvelope, Error (displayError), serialiseToRawBytesHex)
import qualified Data.ByteString.Short as SBS
import qualified Cardano.Ledger.Alonzo.Data as Alonzo
import Cardano.Api.Shelley (toAlonzoData, PlutusScript (PlutusScriptSerialised))
import System.Environment (getArgs)
import qualified Data.ByteString.Char8 as C
import Data.Text.Encoding
import           Codec.Serialise
import qualified Data.ByteString.Lazy as LBS
import Plutus.Contract.Blockchain.MarketPlace (marketValidator, Market (..), DirectSale (DirectSale), SellType (Primary))
import Plutus.Contract.Wallet.MarketPlace (Market(Market))
import Wallet.Emulator
import Ledger (pubKeyHash)
import Data.Text.Conversions (Base16(Base16, unBase16), ToText (toText), FromText (fromText), UTF8 (unUTF8, UTF8), Base64)
import Data.ByteString(ByteString(), split)
import Data.Text (Text)
import Data.Functor ((<&>))
import Plutus.V1.Ledger.Api (toBuiltin, BuiltinByteString)
import Plutus.V1.Ledger.Value (assetClass, tokenName)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Plutus.V1.Ledger.Api (CurrencySymbol(CurrencySymbol))
import Plutus.V1.Ledger.Api (TokenName(TokenName))
import Text.Read (readMaybe, Lexeme (Number))


test :: Base16 ByteString
test=Base16  ""

unHex ::  String -> Maybe  ByteString
unHex v = fromText (toText v) <&> unBase16


-- unHex :: String -> ByteString 
-- unHex input =toText test
--   where 
--     hexText= t  input

main :: IO ()
defaultMarket :: Market
defaultMarket = Market{
  mOperator   = Plutus.PubKeyHash  "1b5486c2b84712b4efa13e4bc2478bac76cb4d1569addfb9117cef39",
  mAuctionFee =5_000_000 ,  -- 5%
  mPrimarySaleFee =5_000_000, -- 5%
  mSecondarySaleFee=2_500_000 -- 2.5%
}



main = do
  args <- getArgs
  let nargs = length args
  let scriptname = "marketplace.plutus"
  case nargs of
    0 -> printHelp
    _ ->
      case  head args of
        "datum"-> case args!!1 of
              "sell"  -> if length args /= 5 then putStrLn  "market-cli datum sell SellerPubKeyHash CurrencySymbol:TokenName CostInAda" else printSellDatum $ drop 2 args
              _       ->  putStrLn  "Expecting at least 5 fields for sell datum"
        "compile" -> do
          writePlutusScript 42 scriptname (marketScriptPlutus defaultMarket) (marketScriptBS defaultMarket)
          putStrLn $ "Script Written to : " ++ scriptname

        _ -> printHelp

  where
    printHelp =do
      args <- getArgs
      putStrLn $ "Unknown options " ++show args
printSellDatum :: [String] -> IO ()
printSellDatum [pkhStr, tokenStr,costStr] =do
    case saleData of
      Right ds -> print ds
      Left error -> putStrLn  error


  where
    saleData= case maybePkh of
      Just pkh -> case maybeAclass of
        Just aClass -> case maybeCost of
            Just cost -> Right $ DirectSale pkh [] aClass ( round (cost * 1000000)) Primary
            _ -> Left $ "Failed to parse \""++costStr++"\" as Ada value"
        _           -> Left $  "Failed to parse \""++ tokenStr ++ "\" as Native token"
      _             -> Left $  "Failed to parse \""++ pkhStr ++ "\" as PubKeyHash hex"

    maybePkh=do
          pk <- unHex pkhStr
          pure $ Plutus.PubKeyHash (toBuiltin pk)

    maybeCost:: Maybe Float
    maybeCost  = readMaybe costStr

    maybeAclass= case splitByColon  of
      [cBytes,tBytes]     -> do
                            c <- fromText (toText $ Base16  cBytes) <&> unBase16
                            return $ assetClass (CurrencySymbol $ toBuiltinBs c) (TokenName $ toBuiltinBs tBytes )
      _         -> Nothing

    toBuiltinBs :: ByteString -> BuiltinByteString
    toBuiltinBs  = toBuiltin

    splitByColon :: [ByteString]
    splitByColon=C.split  ':' $ encodeUtf8 . T.pack $ tokenStr



marketScriptPlutus :: Market -> PlutusScript PlutusScriptV1
marketScriptPlutus market =PlutusScriptSerialised $ marketScriptBS market

marketScriptBS :: Market -> SBS.ShortByteString
marketScriptBS market = SBS.toShort . LBS.toStrict $ serialise script
  where
  script  = Plutus.unValidatorScript $ marketValidator market

writePlutusScript :: Integer -> FilePath -> PlutusScript PlutusScriptV1 -> SBS.ShortByteString -> IO ()
writePlutusScript scriptnum filename scriptSerial scriptSBS =
  do
  case Plutus.defaultCostModelParams of
        Just m ->
          let Alonzo.Data pData = toAlonzoData (ScriptDataNumber scriptnum)
              (logout, e) = Plutus.evaluateScriptCounting Plutus.Verbose m scriptSBS [pData]
          in case e of
                  Left evalErr -> print $ ("Eval Error" :: String) ++ show  evalErr
                  Right exbudget -> print  $ ("Ex Budget" :: String) ++ show exbudget
        Nothing -> error "defaultCostModelParams failed"
  result <- writeFileTextEnvelope filename Nothing scriptSerial
  case result of
    Left err -> print $ displayError err
    Right () -> return ()