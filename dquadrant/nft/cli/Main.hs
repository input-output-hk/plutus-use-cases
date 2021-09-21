{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NamedFieldPuns #-}
import           Control.Monad.Trans.Except.Extra (firstExceptT, handleIOExceptT, hoistEither,
                   hoistMaybe, left, newExceptT)
import qualified Plutus.V1.Ledger.Api as Plutus
import Cardano.Api (PlutusScript, PlutusScriptV1, ScriptData (ScriptDataNumber), writeFileTextEnvelope, Error (displayError), serialiseToRawBytesHex, AnyCardanoEra (AnyCardanoEra), NetworkId)
import qualified Data.ByteString.Short as SBS
import qualified Cardano.Ledger.Alonzo.Data as Alonzo
import Cardano.Api.Shelley (toAlonzoData, PlutusScript (PlutusScriptSerialised))
import System.Environment (getArgs)
import qualified Data.ByteString.Char8 as C
import Data.Text.Encoding
import GHC.Exception
import qualified Data.ByteString.Lazy as LBS
import Plutus.Contract.Blockchain.MarketPlace (marketValidator, Market (..), DirectSale (DirectSale), SellType (Primary), marketAddress, MarketRedeemer (Buy))
import Wallet.Emulator
import Ledger (pubKeyHash, datumHash, Datum (Datum), Tx)
import Data.Text.Conversions (Base16(Base16, unBase16), ToText (toText), FromText (fromText), UTF8 (unUTF8, UTF8), Base64, DecodeText (decodeText), convertText)
import Data.ByteString(ByteString(), split, unpack)
import Data.Text (Text)
import Data.Functor ((<&>))
import Plutus.V1.Ledger.Api (toBuiltin, BuiltinByteString, builtinDataToData, fromBuiltin, toData, FromData)
import Plutus.V1.Ledger.Value (assetClass, tokenName)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Plutus.V1.Ledger.Api (CurrencySymbol(CurrencySymbol))
import Plutus.V1.Ledger.Api (TokenName(TokenName))
import Text.Read (readMaybe, Lexeme (Number))
import Cardano.Api
import Cardano.Api.Shelley
import Cardano.CLI.Types (SocketPath(SocketPath), TxOutChangeAddress (TxOutChangeAddress))
import PlutusTx.Builtins (sha2_256, sha3_256, addInteger)
import Data.ByteString.Lazy (toStrict)
import Data.Aeson (ToJSON(toJSON), encode)
import Codec.Serialise (serialise)
import Plutus.V1.Ledger.Address (Address(Address))
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Sequence.Strict as Seq

import Cardano.CLI.Shelley.Run.Transaction
import Control.Monad.Trans.Except
import Cardano.CLI.Shelley.Commands
import Cardano.CLI.Environment (readEnvSocketPath)
import Control.Monad (unless, void)
import System.Posix.Internals (withFilePath)
import Cardano.Api.Byron (LocalStateQueryClient(LocalStateQueryClient))
import Cardano.Api.NetworkId.Extra (testnetNetworkId)
import Control.Exception (try, handle, handleJust)
import GHC.Exception
import Cardano.Ledger.Alonzo.Tx (ValidatedTx(ValidatedTx))
import qualified Cardano.Ledger.Alonzo.TxBody as Alonzo
import qualified Cardano.Ledger.ShelleyMA.AuxiliaryData as Allegra
import qualified Cardano.Ledger.ShelleyMA.AuxiliaryData as Mary
import qualified Cardano.Ledger.ShelleyMA.TxBody as Allegra
import qualified Cardano.Ledger.ShelleyMA.TxBody as Mary
import Cardano.Ledger.ShelleyMA.TxBody (ValidityInterval(ValidityInterval, invalidBefore, invalidHereafter))

import qualified Shelley.Spec.Ledger.Genesis as Shelley
import qualified Shelley.Spec.Ledger.Metadata as Shelley
import qualified Shelley.Spec.Ledger.Tx as Shelley
import qualified Shelley.Spec.Ledger.TxBody as Shelley
import qualified Shelley.Spec.Ledger.UTxO as Shelley

import qualified Ouroboros.Network.Protocol.LocalStateQuery.Client as Net.Query
import           Ouroboros.Network.Protocol.LocalStateQuery.Type (AcquireFailure (..))
import qualified Ouroboros.Network.Protocol.LocalStateQuery.Type as Net.Query
import qualified Ouroboros.Network.Protocol.LocalTxSubmission.Client as Net.Tx
import Data.Maybe.Strict
import Ouroboros.Network.Protocol.LocalTxSubmission.Type
import qualified Data.Text as Text
import GHC.Exception.Type (Exception)
import Cardano.CLI.Run (renderClientCommandError, runClientCommand, ClientCommand (ShelleyCommand))
import Control.Monad.Trans.Except.Exit (orDie)
import Data.Maybe (mapMaybe)
import Data.Set (toList)

unHex ::  ToText a => a -> Maybe  ByteString
unHex v = convertText (toText v) <&> unBase16

unHexBs :: ByteString -> Maybe ByteString
unHexBs v =  decodeText (UTF8 v) >>= convertText  <&> unBase16

toHexString :: (FromText a1, ToText (Base16 a2)) => a2 -> a1
toHexString bs = fromText $  toText (Base16 bs )

-- unHex :: String -> ByteString 
-- unHex input =toText test
--   where 
--     hexText= t  input


main :: IO ()
defaultMarket :: Market
defaultMarket = Market{
  mOperator   = Plutus.PubKeyHash  "1b5486c2b84712b4efa13e4bc2478bac76cb4d1569addfb9117cef39",
  mAuctionFee =1_000_000 ,  -- 5%
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
        "sell"-> if length args /= 4 then putStrLn  "Usage \n\tmarket-cli datum sell SellerPubKeyHash CurrencySymbol:TokenName CostInAda" else printSellDatum $ tail args
        "compile" -> do
          writePlutusScript 42 scriptname (marketScriptPlutus defaultMarket) (marketScriptBS defaultMarket)
          putStrLn $ "Script Written to : " ++ scriptname
          putStrLn $ "Market  :: " ++ ( show $ marketAddress defaultMarket )
        "balance" -> runBalanceCommand (map toText $ tail args)
        "connect" -> runConnectionTest
        "pay" -> runPayCommand (tail args)

        _ -> printHelp

  where
    printHelp =do
      args <- getArgs
      putStrLn $ "Unknown options " ++show args
printSellDatum :: [String] -> IO ()
printSellDatum [pkhStr, tokenStr,costStr] =do
    case saleData of
      Right ds -> do
        let binary=serialise  $   toData  ds
        putStr "\n"
        print ds
        putStrLn $ "Datum          : " ++ toHexString  binary
        putStrLn $ "Datum Hash     : " ++  ( toHexString $ fromBuiltin $ sha3_256 $ toBuiltinBs $ toStrict  binary)
        putStrLn $ "Buy Redeemer   : " ++ (toHexString $ serialise $ toData Buy)
        putStrLn $ "Datum (Json)     : " ++ dataToJsonString ds
        putStrLn $ "Redeemer (Json):" ++  dataToJsonString Buy

      Left error -> putStrLn  error
  where
    dataToJsonString :: (FromData a,ToJSON a) => a -> String
    dataToJsonString v=   fromText $ decodeUtf8  $ toStrict $ Data.Aeson.encode   $ toJSON v


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
      [cHexBytes,tBytes]     ->  case unHexBs cHexBytes of
        Just b  ->  Just $  toAc [b ,tBytes]
        _       -> Nothing
      _         -> Nothing

    toAc [cBytes,tBytes]= assetClass (CurrencySymbol $ toBuiltinBs cBytes) (TokenName $ toBuiltinBs tBytes )

    toBuiltinBs :: ByteString -> BuiltinByteString
    toBuiltinBs  = toBuiltin

    splitByColon :: [ByteString]
    splitByColon=C.split  '.' $ encodeUtf8 . T.pack $ tokenStr

printSellDatum _ = putStrLn $ "Shouldn't happen"



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

data SomeError = SomeError String deriving  (Show)

instance Exception SomeError


runPayCommand::[String] -> IO()
runPayCommand [strAddr,lovelaceValue] =do
  handleJust catcher handler ( getLocalChainTip localNodeConnInfo >>=print )
  v<-queryNodeLocalState localNodeConnInfo Nothing protocolParamsQuery
  addr<-case maybeAddr of
    Nothing -> throw $ SomeError "Address unrecognized"
    Just a  ->pure a
  (_in,out)<-querySufficientUtxo lovelaceValue addr
  pParam<-case v of
    Left af -> throw $ SomeError  "Acquire Failure"
    Right e -> case e of
      Left em -> throw $ SomeError "Missmatched Era"
      Right pp -> return pp
  alonzoAddr<-case anyAddressInEra AlonzoEra addr of
      Nothing -> throw $ SomeError "This adddress isnot in Alonzo Era"
      Just a  -> pure a
  let body =
          (TxBodyContent {
            txIns=[(_in, BuildTxWith $ KeyWitness KeyWitnessForSpending)],
            txInsCollateral=TxInsCollateral CollateralInAlonzoEra  [  ],
            txOuts=[TxOut alonzoAddr (TxOutValue MultiAssetInAlonzoEra (lovelaceToValue $ Lovelace 1000000)) TxOutDatumHashNone ],
            txFee=TxFeeExplicit TxFeesExplicitInAlonzoEra $ Lovelace 100000,
            -- txValidityRange = (
            --     TxValidityLowerBound ValidityLowerBoundInAlonzoEra 0
            --     , TxValidityUpperBound ValidityUpperBoundInAlonzoEra 6969),
            txValidityRange=(TxValidityNoLowerBound,TxValidityNoUpperBound ValidityNoUpperBoundInAlonzoEra),
            txMetadata=TxMetadataNone ,
            txAuxScripts=TxAuxScriptsNone,
            txExtraScriptData=BuildTxWith TxExtraScriptDataNone ,
            txExtraKeyWits=TxExtraKeyWitnessesNone,
            txProtocolParams=BuildTxWith (Just  pParam),
            txWithdrawals=TxWithdrawalsNone,
            txCertificates=TxCertificatesNone,
            txUpdateProposal=TxUpdateProposalNone,
            txMintValue=TxMintNone,
            txScriptValidity=TxScriptValidityNone
          })
  res <- case makeTransactionBody $ body of
          Left tbe -> throw $ SomeError $  "Tx Body has error : " ++  (show tbe)
          Right txBody -> do
            let tx = makeSignedTransaction [] (txBody) -- witness and txBody
            submitTxToNodeLocal localNodeConnInfo $  TxInMode tx AlonzoEraInCardanoMode
  case res of
    SubmitSuccess ->  putTextLn "Transaction successfully submitted."
    SubmitFail reason ->
      case reason of
        TxValidationErrorInMode err _eraInMode ->  print err
        TxValidationEraMismatch mismatchErr -> print mismatchErr

  -- let query   = QueryInShelleyBasedEra ShelleyBasedEraAlonzo 
  --     qInMode = QueryInEra eInMode query
  -- result <- executeQuery
  --             era
  --             cModeParams
  --             localNodeConnInfo
  --             qInMode
  print $ "something"

  where
    maybeAddr=deserialiseAddress AsAddressAny $ toText strAddr
    protocolParamsQuery= QueryInEra AlonzoEraInCardanoMode
                    $ QueryInShelleyBasedEra ShelleyBasedEraAlonzo QueryProtocolParameters
    dummyFee = Just $ Lovelace 0
    tx_mode body =TxInMode ( ShelleyTx ShelleyBasedEraAlonzo body )  AlonzoEraInCardanoMode
    handler e = putStrLn e
    catcher e = case e of
      DivideByZero ->Nothing
      _            -> Just "Node error or something"

runPayCommand _ = putStrLn "Invalid no of args"

runConnectionTest :: IO ()
runConnectionTest = do
  handleJust catcher handler ( getLocalChainTip localNodeConnInfo >>=print )
  v<-queryNodeLocalState localNodeConnInfo Nothing protocolParamsQuery

  pParam<-case v of
    Left af -> throw $ SomeError  "Acquire Failure"
    Right e -> case e of
      Left em -> throw $ SomeError "Missmatched Era"
      Right pp -> return pp


  res <- case makeTransactionBody $ body pParam of
          Left tbe -> throw $ SomeError "Tx Body has error"
          Right txBody -> do
            let tx = makeSignedTransaction [] (txBody) -- witness and txBody
            submitTxToNodeLocal localNodeConnInfo $  TxInMode tx AlonzoEraInCardanoMode
  case res of
    SubmitSuccess ->  putTextLn "Transaction successfully submitted."
    SubmitFail reason ->
      case reason of
        TxValidationErrorInMode err _eraInMode ->  print err
        TxValidationEraMismatch mismatchErr -> print mismatchErr

  -- let query   = QueryInShelleyBasedEra ShelleyBasedEraAlonzo 
  --     qInMode = QueryInEra eInMode query
  -- result <- executeQuery
  --             era
  --             cModeParams
  --             localNodeConnInfo
  --             qInMode
  print $ "something"

  where
    protocolParamsQuery= QueryInEra AlonzoEraInCardanoMode
                    $ QueryInShelleyBasedEra ShelleyBasedEraAlonzo QueryProtocolParameters
    dummyFee = Just $ Lovelace 0
    tx_mode body =TxInMode ( ShelleyTx ShelleyBasedEraAlonzo body )  AlonzoEraInCardanoMode
    handler e = putStrLn e
    catcher e = case e of
      DivideByZero ->Nothing
      _            -> Just "Node error or something"

    body pp =
              (TxBodyContent {
                txIns=[],
                txInsCollateral=TxInsCollateral CollateralInAlonzoEra  [  ],
                txOuts=[],
                txFee=TxFeeExplicit TxFeesExplicitInAlonzoEra $ Lovelace 100,
                txValidityRange = (
                    TxValidityLowerBound ValidityLowerBoundInAlonzoEra 0
                   , TxValidityUpperBound ValidityUpperBoundInAlonzoEra 6969),
                txMetadata=TxMetadataNone ,
                txAuxScripts=TxAuxScriptsNone,
                txExtraScriptData=BuildTxWith TxExtraScriptDataNone ,
                txExtraKeyWits=TxExtraKeyWitnessesNone,
                txProtocolParams=BuildTxWith (Just  pp),
                txWithdrawals=TxWithdrawalsNone,
                txCertificates=TxCertificatesNone,
                txUpdateProposal=TxUpdateProposalNone,
                txMintValue=TxMintNone,
                txScriptValidity=TxScriptValidityNone
              })

runBalanceCommand args =do
  let addressIfAny = mapMaybe (\x -> deserialiseAddress AsAddressAny x) ( args)
  v2<-case addressIfAny of
    [] -> throw $ SomeError "Address couldn't be serialized"
    a -> queryNodeLocalState localNodeConnInfo Nothing $ utxoQuery a
  print v2

  where
  utxoQuery qfilter= QueryInEra AlonzoEraInCardanoMode
                    $ QueryInShelleyBasedEra ShelleyBasedEraAlonzo (QueryUTxO (QueryUTxOByAddress (Set.fromList qfilter)) )

queryUtxos :: AddressAny -> IO (UTxO AlonzoEra)
queryUtxos addr=do
  a <-queryNodeLocalState localNodeConnInfo Nothing $ utxoQuery [addr]
  case a of
    Left af -> throw $ SomeError $ show af
    Right e -> case e of
      Left em -> throw $ SomeError $ show em
      Right uto -> return uto

  where
  utxoQuery qfilter= QueryInEra AlonzoEraInCardanoMode
                    $ QueryInShelleyBasedEra ShelleyBasedEraAlonzo (QueryUTxO (QueryUTxOByAddress (Set.fromList qfilter)) )

querySufficientUtxo :: p -> AddressAny -> IO (TxIn, TxOut AlonzoEra)
querySufficientUtxo value addr=queryUtxos addr <&> head . toList
  where
    toList (UTxO m) = Map.toList m


putTextLn :: t0 -> IO ()
putTextLn = error "not implemented"

localNodeConnInfo :: LocalNodeConnectInfo CardanoMode
localNodeConnInfo = LocalNodeConnectInfo (CardanoModeParams (EpochSlots 21600))  (Testnet  (NetworkMagic 1097911063))  "/home/sysadmin/.cardano/testnet/node.socket"


