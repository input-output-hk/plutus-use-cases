{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}

import qualified Plutus.V1.Ledger.Api as Plutus
import qualified Data.ByteString.Short as SBS
import qualified Cardano.Ledger.Alonzo.Data as Alonzo
import System.Environment (getArgs, getEnv)
import qualified Data.ByteString.Char8 as C
import Data.Text.Encoding ( decodeUtf8, encodeUtf8 )
import qualified Data.ByteString.Lazy as LBS
import Plutus.Contract.Blockchain.MarketPlace (marketValidator, Market (..), DirectSale (DirectSale, dsAsset, dsSeller), SellType (Primary), marketAddress, MarketRedeemer (Buy), dsFee, dsPaymentValueList)
-- import           Cardano.Api.Byron hiding (SomeByronSigningKey (..))
import Cardano.Api
import qualified Cardano.Api.Shelley
import Cardano.Api.Shelley (fromPlutusData, Lovelace (Lovelace), Tx (ShelleyTx), PlutusScript (PlutusScriptSerialised), toAlonzoData, ProtocolParameters (protocolParamTxFeeFixed), TxBody (ShelleyTxBody), Address (ShelleyAddress), toPlutusData)


import Ledger
    ( pubKeyHash,
      datumHash,
      Datum(Datum),
      Tx,
      txId,
      PubKeyHash(PubKeyHash),
      TxOut(txOutValue) )
import Data.Text.Conversions (Base16(Base16, unBase16), ToText (toText), FromText (fromText), UTF8 (unUTF8, UTF8), Base64, DecodeText (decodeText), convertText)
import Data.ByteString(ByteString(), split, unpack)
import Data.Text (Text)
import Data.Functor ((<&>))
import System.Posix.Types
import Plutus.V1.Ledger.Api
    ( toBuiltin,
      BuiltinByteString,
      builtinDataToData,
      fromBuiltin,
      toData,
      FromData,
      CurrencySymbol(CurrencySymbol, unCurrencySymbol),
      TokenName(TokenName), ToData (toBuiltinData), BuiltinData (BuiltinData), dataToBuiltinData, DatumHash (DatumHash), fromData )
import Plutus.V1.Ledger.Value (assetClass, tokenName, AssetClass (AssetClass))
import qualified Data.Text as T
import Text.Read (readMaybe, Lexeme (Number))
import Cardano.CLI.Types (SocketPath(SocketPath), TxOutChangeAddress (TxOutChangeAddress))
import PlutusTx.Builtins (sha2_256, sha3_256)
import Data.ByteString.Lazy (toStrict, fromStrict)
import Data.Aeson (ToJSON(toJSON), encode, decode)
import Codec.Serialise (serialise, deserialise)
import Plutus.V1.Ledger.Address (Address(Address))
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Sequence.Strict as Seq

import Control.Monad (unless, void)
import System.Posix.Internals (withFilePath)
import Cardano.Api.NetworkId.Extra (testnetNetworkId)
import Control.Exception (try, handle, handleJust, throw)
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
import Ouroboros.Network.Protocol.LocalTxSubmission.Type
import qualified Data.Text as Text
import GHC.Exception.Type (Exception)
import Cardano.CLI.Run (renderClientCommandError, runClientCommand, ClientCommand (ShelleyCommand))
import Control.Monad.Trans.Except.Exit (orDie)
import Data.Maybe (mapMaybe)
import Data.Set (toList)
import Cardano.CLI.Shelley.Key (deserialiseInputAnyOf)
import Data.Text.Prettyprint.Doc (Pretty(pretty))
import Ouroboros.Network.AnchoredFragment (prettyPrint)
import System.FilePath (joinPath)
import qualified Data.ByteString as BS
import System.Directory
import GHC.IO.Exception (IOException(IOError))
import qualified Ledger.AddressMap
import           Data.Map.Strict (Map)
import Data.String (IsString)
import GHC.Exts (IsString(fromString))
import Shelley.Spec.Ledger.API (Credential(ScriptHashObj, KeyHashObj), Addr (Addr, AddrBootstrap), KeyPair (sKey), Globals (systemStart))
import PlutusTx.IsData (FromData(fromBuiltinData))
import qualified Cardano.Ledger.Mary.Value (Value(Value))
import qualified Cardano.Ledger.Alonzo.TxBody as LedgerBody
import GHC.Generics
import Cardano.Ledger.Address (serialiseAddr, deserialiseAddr)
import Data.List (intersperse, intercalate)
import Data.Ratio
import GHC.Real (Ratio((:%)))
import Cardano.Ledger.Keys (KeyHash(KeyHash))
import Cardano.Ledger.Hashes (ScriptHash(ScriptHash))
import qualified Data.Text.Lazy.Builder as Text
import GHC.Num (integerToWord)
import qualified System.Directory.Internal.Prelude as Set
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Text as JSON
import Data.Aeson.Text (encodeToLazyText)


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


newtype SomeError =  SomeError String
instance Show SomeError where
  show   (SomeError m) = m

instance IsString SomeError where
  fromString v = SomeError v

instance Exception SomeError

unMaybe :: Applicative f =>  SomeError -> Maybe a  -> f a
unMaybe  e m= case m of
  Just v -> pure v
  Nothing -> throw  e

unEither :: Applicative f =>   Either SomeError a  -> f a
unEither e=case e of
    Left v -> throw v
    Right r -> pure r

maybeToEither ::  SomeError ->Maybe  a-> Either SomeError a
maybeToEither e m = case m of
  Nothing -> Left e
  Just a -> Right a


defaultMarket =  do
  let operator :: String="addr_test1vqd4fpkzhpr39d805ylyhsj83wk8dj6dz456mhaez97w7wguunmt6"
  pkh <-  deserialiseAddress AsShelleyAddress  (toText operator ) >>= addrToMaybePkh'
  Just Market{
        mOperator         = pkh
    ,   mPrimarySaleFee   = 5_000_000
    ,   mSecondarySaleFee = 3_000_000
    ,   mAuctionFee       = 2_000_000
    }


main :: IO ()
main = do
  market <-unMaybe "Configuration Error: invalid operator address" defaultMarket
  args <- getArgs
  sockEnv <- try $ getEnv "CARDANO_NODE_SOCKET_PATH"
  socketPath <-case  sockEnv of
    Left (e::IOError) -> do
          defaultSockPath<- getWorkPath ["testnet","node.socket"]
          exists<-doesFileExist defaultSockPath
          if exists then return defaultSockPath else throw (SomeError $ "Socket File is Missing: "++defaultSockPath ++"\n\tSet environment variable CARDANO_NODE_SOCKET_PATH  to use different path")
    Right s -> pure s
  let nargs = length args
  let scriptname = "marketplace.plutus"
  let conn= localNodeConnInfo socketPath

  case nargs of
    0 -> printHelp
    _ ->
      case  head args of
        "sell"-> if length args /= 3 then putStrLn  "Usage \n\tmarket-cli datum sell SellerPubKeyHash CurrencySymbol:TokenName CostInAda" else placeOnMarket market conn $ tail args
        "buy"-> if length args /= 3 then putStrLn  "Usage \n\tmarket-cli  buy txHash#UtxoIndex datumHex" else buyToken market conn $ tail args
        "compile" -> do
          writePlutusScript 42 scriptname (marketScriptPlutus market) (marketScriptBS market)
          putStrLn $ "Script Written to : " ++ scriptname
          putStrLn $ "Market  :: " ++ ( show $ marketAddress market )
        "ls"  -> listMarket market conn
        "balance" -> runBalanceCommand conn (map toText $ tail args)
        "connect" -> runConnectionTest conn
        "keygen"  -> runGenerateKey
        "import"  -> runImportKey $ tail args
        "pay" -> runPayCommand conn (tail args)

        _ -> printHelp

  where
    printHelp =do
      args <- getArgs
      putStrLn $ "Unknown options " ++show args


placeOnMarket market conn [tokenStr,costStr] =do
    lockedToken<- unEither   eitherLockedValue
    sKey <-getDefaultSignKey
    ds <- unEither $ saleData (sKeyToPkh sKey)
    paramQueryResult<-queryNodeLocalState conn Nothing protocolParamsQuery
    let walletAddr=toAddressAny $ skeyToAddr sKey
        walletPkh=sKeyToPkh sKey
        lockedValue= lockedToken <> (lovelaceToValue $ Lovelace 2000000)
        scriptData= dataToScriptData ds
        dataHash=hashScriptData  scriptData
    walletUtxos <- queryUtxos conn walletAddr

    pParam<-case paramQueryResult of
      Left af -> throw $ SomeError  "Acquire Failure"
      Right e -> case e of
        Left em -> throw $ SomeError "Missmatched Era"
        Right pp -> return pp
    alonzoWalletAddr<-unMaybe "Address not supported in alonzo era" $ anyAddressInEra AlonzoEra walletAddr
    let body =
            (TxBodyContent {
              txIns=[] ,
              txInsCollateral=TxInsCollateral CollateralInAlonzoEra  [  ],
              txOuts=[
                  TxOut marketCardanoApiAddress (TxOutValue MultiAssetInAlonzoEra lockedValue) (TxOutDatumHash ScriptDataInAlonzoEra dataHash)
                  ],
              txFee=TxFeeExplicit TxFeesExplicitInAlonzoEra $ Lovelace 0,
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
    let dataJson  =  scriptDataToJson ScriptDataJsonDetailedSchema   scriptData
    let dataJsonStr :: String = convertText $ encodeToLazyText $ dataJson
    txId<-balanceAndSubmitBody conn sKey body walletUtxos (lovelaceToValue $ Lovelace 0)

    putStrLn "Transaction successfully submitted."
    putStrLn $ "\nData (JSON)  : " ++ (dataJsonStr )
    putStrLn $ "Data         : " ++  dataToHex ds
    putStrLn $ "Data  Hash   :" ++ (show $ hashScriptData scriptData)
    putStrLn $ "UtxoId       : " ++  init (tail (show txId)) ++ "#0"

  where
    getSignKey content=deserialiseInputAnyOf [FromSomeType (AsSigningKey AsPaymentKey) id] [FromSomeType (AsSigningKey AsPaymentKey) id] content
    _witness=KeyWitness KeyWitnessForSpending

    requiredMemory = 700000000
    requiredSteps  = 700000000

    marketScript=PlutusScript PlutusScriptV1  $ marketScriptPlutus market
    marketHash=hashScript   marketScript
    marketCardanoApiAddress=makeShelleyAddressInEra (Testnet (NetworkMagic 1097911063)) scriptCredential NoStakeAddress
    scriptCredential=PaymentCredentialByScript marketHash
    protocolParamsQuery= QueryInEra AlonzoEraInCardanoMode
                    $ QueryInShelleyBasedEra ShelleyBasedEraAlonzo QueryProtocolParameters
    dummyFee = Just $ Lovelace 0
    tx_mode body =TxInMode ( ShelleyTx ShelleyBasedEraAlonzo body )  AlonzoEraInCardanoMode

    dataToJsonString v=   fromText $ decodeUtf8  $ toStrict $ Data.Aeson.encode   $ toJSON v

    dataToHex :: (ToData  a) => a -> String
    dataToHex d= toHexString  $ serialise $  toData  d

    saleData:: PubKeyHash -> Either SomeError DirectSale
    saleData pkh = case splitByDot  of
      [cHexBytes,tBytes]     ->  case unHexBs cHexBytes of
        Just b  ->  case maybeCost of
            Just cost -> Right $ DirectSale pkh [] (toAc ["" ,""]) ( round (cost * 1000000)) Primary
            _ -> Left $ SomeError $ "Failed to parse \""++costStr++"\" as Ada value"
        _           -> Left $ SomeError $  "Failed to parse \""++ tokenStr ++ "\" : Invalid PolicyId"
      _         -> Left "Invalid format for Native token representation (Too many `.` character)"

    -- skey is the owner of the key who can withdraw from market

    maybeCost:: Maybe Double
    maybeCost  = readMaybe costStr

    eitherLockedValue :: Either SomeError Value
    eitherLockedValue= case C.split  '.' $ encodeUtf8 . T.pack $ tokenStr  of
      [cHexBytes,tBytes]     ->  case unHexBs cHexBytes of
        Just policyBytes  -> case  deserialiseFromRawBytes AsPolicyId  policyBytes of
          Just policyId -> case deserialiseFromRawBytes AsAssetName tBytes of
            Just assetName -> Right  $ valueFromList  [ (AssetId policyId assetName , 1)]
            _ -> Left $ SomeError $ "TokenName couldn't be converrted to CardanoAPI type for value "++ show tBytes
          _ -> Left $ SomeError  $ "Policy Id  couldn't converted to CardanoAPI type  for value "++ show policyBytes
        _       -> Left $SomeError $ "Policy id is not hex string given value is :"++ show cHexBytes
      _         -> Left "Too many `.` character in tokenName"


    toAc [cBytes,tBytes]= assetClass (CurrencySymbol $ toBuiltinBs cBytes) (TokenName $ toBuiltinBs tBytes )

    toBuiltinBs :: ByteString -> BuiltinByteString
    toBuiltinBs  = toBuiltin

    splitByDot :: [ByteString]
    splitByDot=C.split  '.' $ encodeUtf8 . T.pack $ tokenStr

printSellDatum _ _  = putStrLn $ "Shouldn't happen"

listMarket market conn = do
  utxos <- queryNodeLocalState   conn Nothing (QueryInEra AlonzoEraInCardanoMode
                    $ QueryInShelleyBasedEra ShelleyBasedEraAlonzo (QueryUTxO (QueryUTxOByAddress (Set.singleton $ toAddressAny marketCardanoApiAddress)) ))
  case utxos of
    Left af -> putStrLn "Acquire failure thing "
    Right e -> case e of
      Left em -> putStrLn "Era mismatch !! Either you are in older era or you need to check if there's new version of market-cli"
      Right (UTxO txouts) ->  mapM_ (putStrLn . toStrings) $ Map.toList txouts
  where
    toStrings (TxIn txId (TxIx index),TxOut addr value hash )=    (init $ tail $ show txId) ++ "#" ++  show index ++"\t:\t" ++ (case value of
       TxOutAdaOnly oasie (Lovelace v) -> show hash ++show v
       TxOutValue masie va ->  show hash ++intercalate " +" (map vToString $valueToList va ) )
    vToString (AssetId policy asset,Quantity v)=show v ++ " " ++ (init $ tail $ show  policy) ++ "." ++ (init $ tail $ show  asset)
    vToString (AdaAssetId, Quantity v) = if v >99999
      then  case v % 1000000 of { n :% i -> if i==1
              then show n ++ " Ada"
              else show n ++"." ++ show i++ " Ada"
      }
      else show v ++ " Lovelace"
    marketScript=PlutusScript PlutusScriptV1  $ marketScriptPlutus market
    marketHash=hashScript   marketScript
    marketCardanoApiAddress=makeShelleyAddress (Testnet (NetworkMagic 1097911063)) scriptCredential NoStakeAddress
    scriptCredential=PaymentCredentialByScript marketHash

buyToken market@Market{mOperator, mPrimarySaleFee} conn [utxoId, datumStr] =do
  consumedData <- unEither $ decodeJsonAsScriptData datumStr
  directSale  <- unMaybe "Failed to convertData to DirectSale"  (fromData $ toPlutusData consumedData)
  paymentAsset <-unEither $ dsAssetId directSale
  sKey <-getDefaultSignKey
  operatorAddr <- unMaybe "Operator  pubKeyHash couldn't be converted to Cardano API address" $  pkhToMaybeAddr (Testnet (NetworkMagic 1097911063)) mOperator
  UTxO uMap<-queryUtxos conn $ toAddressAny marketCardanoApiAddress
  paramQueryResult<-queryNodeLocalState conn Nothing protocolParamsQuery
  -- let ourUtxos=Map.filterWithKey (k -> txin -> Bool) (Map k a)
  txIn<-unEither parseTxIn
  let marketUtxos =Map.filterWithKey  (\k v-> txIn == k) uMap
      walletAddr=toAddressAny $ skeyToAddr sKey
      walletPkh=sKeyToPkh sKey
      toPartyUtxo (pkh@(PubKeyHash binary),v) =case pkhToMaybeAddr (Testnet (NetworkMagic 1097911063)) pkh of
        Just addr -> Right $ singletonTxOut addr paymentAsset (Quantity v)
        _ -> Left $ SomeError $ "Party pubKeyHash couldn't be converted to Cardano API address :" ++ toHexString (fromBuiltin binary)
  (txin,txout)<-if  null marketUtxos then throw (SomeError $ "Utxos Not found " ++ show txIn) else pure  $ head $ Map.toList marketUtxos
  partyUtxos<-unEither $ mapM toPartyUtxo ( dsPaymentValueList market directSale)
  walletUtxos <- queryUtxos conn  $toAddressAny $ skeyToAddr sKey
  pParam<-case paramQueryResult of
    Left af -> throw $ SomeError  "Acquire Failure"
    Right e -> case e of
      Left em -> throw $ SomeError "Missmatched Era"
      Right pp -> return pp
  alonzoWalletAddr<-unMaybe "Address not supported in alonzo era" $ anyAddressInEra AlonzoEra walletAddr
  pparam <- queryProtocolParam conn
  let txIns=[(txin,BuildTxWith $ ScriptWitness ScriptWitnessForSpending $  plutusScriptWitness directSale)]
      txInsCollateral=TxInsCollateral CollateralInAlonzoEra  [  ]
      txOuts=singletonTxOut operatorAddr paymentAsset (Quantity $ dsFee market directSale)
              :
            partyUtxos
      body= mkBody txIns txOuts txInsCollateral pparam
      consumedValue=case txout of { TxOut aie tov todh -> case tov of
                                      TxOutAdaOnly oasie lo -> lovelaceToValue lo
                                      TxOutValue masie va -> va }
  systemStart <-querySystemStart conn
  eraHistory <-queryEraHistory conn
  balancedBody <- executeMkBalancedBody pParam  walletUtxos body consumedValue   $  skeyToAddrInEra sKey
  let utxo=UTxO marketUtxos
  let v= evaluateTransactionExecutionUnits AlonzoEraInCardanoMode systemStart eraHistory pparam utxo balancedBody
  units<- case v of
    Left tvie -> throw $ SomeError $ show tvie
    Right map -> mapM unEitherExecutionUnit (Map.elems  map)
  case units of
    [] -> throw $ SomeError " Unexpected empty list of execution units"
    [eUnit] -> do
      let modifiedIns= [(txin,BuildTxWith $ ScriptWitness ScriptWitnessForSpending $  computedScriptWitness eUnit directSale)]
          newBody=mkBody modifiedIns txOuts txInsCollateral pparam
      balanceAndSubmitBody conn sKey newBody walletUtxos consumedValue
        >>=printTxSubmited
    _ -> throw $ SomeError "Too many values in the execution unit array"


  where
    mkBody ins outs collateral pParam =
          (TxBodyContent {
            txIns=ins ,
            txInsCollateral=collateral,
            txOuts=outs,
            txFee=TxFeeExplicit TxFeesExplicitInAlonzoEra $ Lovelace 1000000,
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
    unEitherExecutionUnit e= case e of
      Left e -> throw $  SomeError  $ show e
      Right v -> pure v

    getSignKey content=deserialiseInputAnyOf [FromSomeType (AsSigningKey AsPaymentKey) id] [FromSomeType (AsSigningKey AsPaymentKey) id] content
    _witness=KeyWitness KeyWitnessForSpending

    requiredMemory = 100_000_0000
    requiredSteps  = 100_000_0000

    marketScript=PlutusScript PlutusScriptV1  $ marketScriptPlutus market
    marketHash=hashScript   marketScript
    -- marketCardanoApiAddress::AddressInEra AlonzoEra
    marketCardanoApiAddress=makeShelleyAddress (Testnet (NetworkMagic 1097911063)) scriptCredential NoStakeAddress
    scriptCredential=PaymentCredentialByScript marketHash

    computedScriptWitness unit d = PlutusScriptWitness
                            PlutusScriptV1InAlonzo
                            PlutusScriptV1
                            (marketScriptPlutus market)
                            (ScriptDatumForTxIn $ fromPlutusData $ toData d) -- script data
                            (fromPlutusData $ toData Buy) -- script redeemer
                            unit
    plutusScriptWitness ::ToData a=> a-> ScriptWitness  WitCtxTxIn AlonzoEra
    plutusScriptWitness  d = PlutusScriptWitness
                            PlutusScriptV1InAlonzo
                            PlutusScriptV1
                            (marketScriptPlutus market)
                            (ScriptDatumForTxIn $ fromPlutusData $ toData d) -- script data
                            (fromPlutusData $ toData Buy) -- script redeemer
                            (ExecutionUnits requiredSteps requiredMemory)
    protocolParamsQuery= QueryInEra AlonzoEraInCardanoMode
                    $ QueryInShelleyBasedEra ShelleyBasedEraAlonzo QueryProtocolParameters
    dummyFee = Just $ Lovelace 0
    tx_mode body =TxInMode ( ShelleyTx ShelleyBasedEraAlonzo body )  AlonzoEraInCardanoMode

    singletonTxOutValue:: AssetId -> Quantity -> TxOutValue AlonzoEra
    singletonTxOutValue  a v =TxOutValue MultiAssetInAlonzoEra (valueFromList  [ (a , v)])

    singletonTxOut :: AddressInEra  AlonzoEra -> AssetId -> Quantity -> Cardano.Api.Shelley.TxOut AlonzoEra
    singletonTxOut  addr aId q = TxOut addr (singletonTxOutValue aId q) TxOutDatumHashNone


    dsAssetId::DirectSale -> Either SomeError AssetId
    dsAssetId DirectSale{dsAsset=AssetClass (CurrencySymbol  c, TokenName t)}=
      if BS.null  $ fromBuiltin c
      then Right AdaAssetId
      else
        case  deserialiseFromRawBytes AsPolicyId  (fromBuiltin  c) of
            Just policyId -> case deserialiseFromRawBytes AsAssetName (fromBuiltin t) of
              Just assetName -> Right $ AssetId policyId assetName
              _ -> Left $ SomeError $ "TokenName couldn't be converrted to CardanoAPI type for value : 0x"++   toHexString (fromBuiltin t)
            _ -> Left $ SomeError  $ "Policy Id  couldn't converted to CardanoAPI type  for value : 0x"++ toHexString (fromBuiltin t)
    toBuiltinBs :: ByteString -> BuiltinByteString
    toBuiltinBs  = toBuiltin

    parseTxIn = case break (== '#') utxoId of
      (s1, '#':v) -> do
        txidBs <- maybeToEither "Invalid Hex value in TxId" (unHex s1)
        txid<-maybeToEither "Invalid TxId hex" (deserialiseFromRawBytes AsTxId txidBs)
        (index::Integer) <- maybeToEither "TxIndex cant be parsed as string " $ readMaybe v
        Right $  TxIn txid $ TxIx (fromInteger index)
      _           ->Left "UtxoId must be in format of TxId#Index"


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

runPayCommand::LocalNodeConnectInfo CardanoMode -> [String] -> IO()
runPayCommand conn [strAddr,lovelaceStr] =do
  receiverAddr<- unMaybe "Address Unrecognized " $ deserialiseAddress AsAddressAny $ toText strAddr
  receiverAddrAlonzo <- unMaybe "Receiver address not supported in alonzo era" $ anyAddressInEra AlonzoEra receiverAddr
  sKey <-getDefaultSignKey
  let walletAddr=  skeyToAddrInEra sKey
  paymentDigit ::Integer <-unMaybe "Invalid value for lovelace" $ readMaybe lovelaceStr
  utxos<-queryUtxos conn  $toAddressAny $ skeyToAddr sKey


  let paymentValue = lovelaceToValue $ Lovelace   paymentDigit
  let unbalancedBody=TxBodyContent {
        txIns=[],
        txInsCollateral=TxInsCollateral CollateralInAlonzoEra  [  ],
        txOuts=[TxOut receiverAddrAlonzo (TxOutValue MultiAssetInAlonzoEra paymentValue) TxOutDatumHashNone],
          -- [TxOut alonzoAddr (TxOutValue MultiAssetInAlonzoEra (lovelaceToValue $ Lovelace 5927107)) TxOutDatumHashNone ],
        txFee=TxFeeExplicit TxFeesExplicitInAlonzoEra  ( Lovelace 0),
        txValidityRange=(TxValidityNoLowerBound,TxValidityNoUpperBound ValidityNoUpperBoundInAlonzoEra),
        -- txValidityRange = (TxValidityLowerBound  ValidityLowerBoundInAlonzoEra 38698728,TxValidityUpperBound ValidityUpperBoundInAlonzoEra  38699728),
        txMetadata=TxMetadataNone ,
        txAuxScripts=TxAuxScriptsNone,
        txExtraScriptData=BuildTxWith TxExtraScriptDataNone ,
        txExtraKeyWits=TxExtraKeyWitnessesNone,
        txProtocolParams=BuildTxWith Nothing,
        txWithdrawals=TxWithdrawalsNone,
        txCertificates=TxCertificatesNone,
        txUpdateProposal=TxUpdateProposalNone,
        txMintValue=TxMintNone,
        txScriptValidity=TxScriptValidityNone
      }

  txId<-balanceAndSubmitBody conn sKey unbalancedBody utxos (lovelaceToValue $ Lovelace 0)
  putStrLn $ "Transaction Submitted " ++ show txId



  where
    _witness=KeyWitness KeyWitnessForSpending
    protocolParamsQuery= QueryInEra AlonzoEraInCardanoMode
                    $ QueryInShelleyBasedEra ShelleyBasedEraAlonzo QueryProtocolParameters
    dummyFee = Just $ Lovelace 0
    tx_mode body =TxInMode ( ShelleyTx ShelleyBasedEraAlonzo body )  AlonzoEraInCardanoMode
    handler e = putStrLn e
    catcher e = case e of
      SomeError _ ->Nothing
      _            -> Just "Node error or something"

    utxosWithWitness (txin,txout) = (txin, BuildTxWith $ KeyWitness KeyWitnessForSpending)

    utxosValue (txin, TxOut _ (TxOutValue _ value) _) = value


runPayCommand _ _ = putStrLn "Invalid no of args"

runConnectionTest :: LocalNodeConnectInfo CardanoMode -> IO ()
runConnectionTest  conn = do
  handleJust catcher handler ( getLocalChainTip conn >>=print )
  v<-queryNodeLocalState conn Nothing protocolParamsQuery

  pParam<-case v of
    Left af -> throw $ SomeError  "Acquire Failure"
    Right e -> case e of
      Left em -> throw $ SomeError "Missmatched Era"
      Right pp -> return pp

  putStrLn $ show pParam



  where
    protocolParamsQuery= QueryInEra AlonzoEraInCardanoMode
                    $ QueryInShelleyBasedEra ShelleyBasedEraAlonzo QueryProtocolParameters
    handler e = putStrLn e
    catcher e = case e of
      SomeError _  ->Nothing
      _            -> Just "Node error or something"


runBalanceCommand :: LocalNodeConnectInfo CardanoMode -> [Text] -> IO ()
runBalanceCommand conn args =do
  skey <-getDefaultSignKey
  let paymentCredential =   PaymentCredentialByKey  $ verificationKeyHash   $ getVerificationKey  skey
  addrs <- (case args of
      [] ->  do
          pure  [ toAddressAny   $ makeShelleyAddress (Testnet  (NetworkMagic 1097911063))  paymentCredential NoStakeAddress]
      as -> case mapMaybe (\x -> deserialiseAddress AsAddressAny x) ( as) of
          [] -> throw $ SomeError "Address couldn't be serialized"
          v  -> pure v)
  v2 <- queryNodeLocalState conn Nothing $ utxoQuery addrs
  balances <-case v2 of
    Left af -> throw $ SomeError $ "Acquire Failure "++ show af
    Right e -> case e of
      Left em  -> throw $ SomeError $  "Era missmatch thing just happened, has the time started running backwards!"
      Right uto -> pure $  uto
  let balance =  foldMap toValue $ toTxOut balances
  putStrLn $ "Wallet Address: "++( fromText $  serialiseToBech32 $ makeShelleyAddress (Testnet  (NetworkMagic 1097911063))  paymentCredential NoStakeAddress)
  putStrLn $ "Utxo Count :" ++(show $ length  $ toTxOut balances)
  putTextLn $ renderValuePretty balance
  where

  toTxOut (UTxO a) = Map.elems  a

  toValue ::Cardano.Api.Shelley.TxOut AlonzoEra -> Value
  toValue (TxOut _ v _) = case v of
    TxOutAdaOnly oasie lo -> lovelaceToValue lo
    TxOutValue masie va -> va

  utxoQuery qfilter= QueryInEra AlonzoEraInCardanoMode
                    $ QueryInShelleyBasedEra ShelleyBasedEraAlonzo (QueryUTxO (QueryUTxOByAddress (Set.fromList qfilter)) )

queryUtxos :: LocalNodeConnectInfo CardanoMode-> AddressAny -> IO (UTxO AlonzoEra)
queryUtxos conn addr=do
  a <-queryNodeLocalState conn Nothing $ utxoQuery [addr]
  case a of
    Left af -> throw $ SomeError $ show af
    Right e -> case e of
      Left em -> throw $ SomeError $ show em
      Right uto -> return uto

  where
  utxoQuery qfilter= QueryInEra AlonzoEraInCardanoMode
                    $ QueryInShelleyBasedEra ShelleyBasedEraAlonzo (QueryUTxO (QueryUTxOByAddress (Set.fromList qfilter)) )

querySufficientUtxo :: LocalNodeConnectInfo CardanoMode -> p -> AddressAny -> IO [(TxIn, Cardano.Api.Shelley.TxOut AlonzoEra)]
querySufficientUtxo conn value addr=queryUtxos conn addr <&>  toList
  where
    toList (UTxO m) = Map.toList m

queryPaymentUtxosAndChange :: LocalNodeConnectInfo CardanoMode-> Value -> AddressAny -> IO ([(TxIn, Cardano.Api.Shelley.TxOut AlonzoEra)], Value)
queryPaymentUtxosAndChange conn value addr=do
    allUtxos<-queryUtxos conn addr<&> toList

    pure (allUtxos  , foldMap ( toValue . snd) allUtxos <> negateValue value)

  where
    toTxOut (UTxO a) = Map.elems  a

    toValue ::Cardano.Api.Shelley.TxOut AlonzoEra -> Value
    toValue (TxOut _ v _) = case v of
      TxOutAdaOnly oasie lo -> lovelaceToValue lo
      TxOutValue masie va -> va
    toList (UTxO m) = Map.toList m

getWorkPath :: [FilePath] -> IO  FilePath
getWorkPath paths= do
  eitherHome <-try $ getEnv "HOME"
  case eitherHome of
    Left (e::IOError) -> throw $ SomeError "Can't get Home directory"
    Right home -> do
      pure $ joinPath  $  [home , ".cardano"] ++ paths

putTextLn :: Text -> IO ()
putTextLn v  = putStrLn  $ fromText v

localNodeConnInfo :: FilePath -> LocalNodeConnectInfo CardanoMode
localNodeConnInfo = LocalNodeConnectInfo (CardanoModeParams (EpochSlots 21600))  (Testnet  (NetworkMagic 1097911063))

readSignKey :: FilePath -> IO (SigningKey PaymentKey)
readSignKey file = do
  eitherSkey<-try  readSkeyFromFile
  case eitherSkey of
    Left (e::IOError )-> throw $ SomeError  $"There was error reading skey file"
    Right sk -> pure sk
  where
    readSkeyFromFile=do
      exists<-doesFileExist file
      if exists then pure () else throw (SomeError $ file ++ "  doesn't exist")
      content <-readBs file
      case getSignKey content of
        Left bde -> throw $ SomeError $ "Invalid sign key "++ show bde
        Right sk -> pure sk
    readBs:: FilePath -> IO ByteString
    readBs  = BS.readFile
    getSignKey content=deserialiseInputAnyOf [FromSomeType (AsSigningKey AsPaymentKey) id] [FromSomeType (AsSigningKey AsPaymentKey) id] content


getDefaultSignKey :: IO (SigningKey PaymentKey)
getDefaultSignKey= getWorkPath ["default.skey"] >>= readSignKey

skeyToAddr:: SigningKey PaymentKey -> Cardano.Api.Shelley.Address ShelleyAddr
skeyToAddr skey =
  makeShelleyAddress (Testnet  (NetworkMagic 1097911063))  credential NoStakeAddress
  where
    credential=PaymentCredentialByKey  $ verificationKeyHash   $ getVerificationKey  skey

skeyToAddrInEra ::  SigningKey PaymentKey -> AddressInEra AlonzoEra
skeyToAddrInEra skey=makeShelleyAddressInEra (Testnet  (NetworkMagic 1097911063))  credential NoStakeAddress
  where
    credential=PaymentCredentialByKey  $ verificationKeyHash   $ getVerificationKey  skey



sKeyToPkh:: SigningKey PaymentKey -> Plutus.PubKeyHash
sKeyToPkh skey= PubKeyHash (toBuiltin  $  serialiseToRawBytes  vkh)
  where
    vkh=verificationKeyHash   $ getVerificationKey  skey

pkhToMaybeAddr :: IsShelleyBasedEra era =>NetworkId -> Plutus.PubKeyHash -> Maybe (AddressInEra era)
pkhToMaybeAddr network (Plutus.PubKeyHash pkh) =do
    key <- vKey
    Just $ makeShelleyAddressInEra  network (PaymentCredentialByKey key)  NoStakeAddress
  where
    paymentCredential _vkey=PaymentCredentialByKey _vkey
    vKey= deserialiseFromRawBytes (AsHash AsPaymentKey) $fromBuiltin pkh

addrToMaybePkh :: Cardano.Api.Shelley.Address ShelleyAddr -> Maybe PubKeyHash
addrToMaybePkh (ShelleyAddress net cre sr) = do
  _hash <-hash
  pure $ PubKeyHash $ toBuiltin _hash
  where
    hash= case cre of
      ScriptHashObj _ ->Nothing
      KeyHashObj kh -> case kh of { KeyHash ha -> Just $ toStrict $serialise ha }

addrToMaybePkh' :: Cardano.Api.Shelley.Address ShelleyAddr -> Maybe PubKeyHash
addrToMaybePkh' (ShelleyAddress net cre sr) = do
  _hash <-hash
  pure $ PubKeyHash $ toBuiltin _hash
  where
    hash= case cre of
      ScriptHashObj _ ->Nothing
      KeyHashObj kh -> case kh of { KeyHash ha -> unHex $ init $ tail$ show  ha }

runGenerateKey :: IO ()
runGenerateKey = do
    file <- getWorkPath ["default.skey"]
    exists<-doesFileExist file
    if exists then throw (SomeError "default.skey File already exists") else pure ()
    key <- generateSigningKey AsPaymentKey
    writeResult <- writeFileTextEnvelope file (Just "market-cli operation key") key
    case writeResult of
      Left fe -> throw  $ SomeError $  "Error writing Key " ++ show fe
      Right x0 -> pure ()


runImportKey  :: [String] -> IO()
runImportKey [path] = do
  key <- readSignKey path
  file <- getWorkPath ["default.skey"]
  exists<-doesFileExist file
  if exists then throw (SomeError "default.skey File already exists") else pure ()
  writeFile file  $ fromText ( serialiseToBech32  key)
runImportKey _ = throw $ SomeError "import: To many arguments"

valueLte :: Value -> Value -> Bool
valueLte v1 v2= not $ any (\(aid,Quantity q) -> q > lookup aid) (valueToList v1) -- do we find anything that's greater than q
  where
    lookup x= case Map.lookup x v2Map of
      Nothing -> 0
      Just (Quantity v) -> v
    v2Map=Map.fromList $ valueToList v2
    -- v1Bundle= case case valueToNestedRep _v1 of { ValueNestedRep bundle -> bundle} of
    --   [ValueNestedBundleAda v , ValueNestedBundle policy assetMap] ->LovelaceToValue v
    --   [ValueNestedBundle policy assetMap]

nullValue :: Value -> Bool
nullValue v = not $ any (\(aid,Quantity q) -> q /= 0) (valueToList v)

positiveValue :: Value -> Bool
positiveValue v = not $ any (\(aid,Quantity q) -> q<0) (valueToList v)

-- parseStrToValue :: Either SomeError Value
-- parseStrToValue valueStr=  splitAt "+" valueStr 
--   where 
--   stringTokens=splitAt "+" valueStr
--   splitter valueStr=case C.split  '.' $ encodeUtf8 . T.pack $ valueStr  of
--     [cHexBytes,tBytes]     ->  case unHexBs cHexBytes of
--       Just policyBytes  -> case  deserialiseFromRawBytes AsPolicyId  policyBytes of
--         Just policyId -> case deserialiseFromRawBytes AsAssetName tBytes of
--           Just assetName -> Right  $ valueFromList  [ (AssetId policyId assetName , 1)]
--           _ -> Left $ SomeError $ "TokenName couldn't be converrted to CardanoAPI type for value "++ show tBytes
--         _ -> Left $ SomeError  $ "Policy Id  couldn't converted to CardanoAPI type  for value "++ show policyBytes
--       _       -> Left $SomeError $ "Policy id is not hex string given value is :"++ show cHexBytes
--     _         -> Left "Too many `.` character in tokenName"


dataToScriptData :: (ToData a1) => a1 -> ScriptData
dataToScriptData sData =  fromPlutusData $ toData sData


decodeJsonAsScriptData :: String -> Either SomeError ScriptData
decodeJsonAsScriptData jsonStr=do
  v<-maybeToEither "Invalid json string" decodeJson
  case scriptDataFromJson ScriptDataJsonDetailedSchema  v of
    Left sdje -> case sdje of
      ScriptDataJsonSchemaError va sdjse -> Left $ SomeError $  "Wrong schema" ++ show sdjse
      ScriptDataRangeError va sdre -> Left $ SomeError $  "Invalid data " ++ show sdre
    Right sd -> Right sd
  where
    decodeJson=JSON.decode $fromString jsonStr

mkBalancedBody :: ProtocolParameters
  -> UTxO era
  -> TxBodyContent BuildTx AlonzoEra
  -> Value
  -> AddressInEra AlonzoEra
  -> Either TxBodyError (TxBody AlonzoEra)
mkBalancedBody  pParams (UTxO utxoMap)  txbody inputSum walletAddr =
    do

      -- first iteration
      let (inputs1,change1) =minimize txouts (startingChange <> negLovelace startingFee)
          bodyContent1=modifiedBody inputs1 change1 startingFee
      txBody1 <- unEither $ case makeTransactionBody bodyContent1 of
        Left tbe -> Left $ SomeError $ show tbe
        Right tb -> Right $ tb

      let modifiedChange1=change1 <> negLovelace  fee1 <> lovelaceToValue startingFee
          fee1= evaluateTransactionFee pParams txBody1 1 0
          (inputs2,change2)= minimize txouts modifiedChange1
          bodyContent2 =modifiedBody inputs2 change2 fee1
       -- if selected utxos are  sufficient to pay transaction fees, just use the fee and make txBody
       -- otherwide, reselect txins and recalculate fee. it's very improbable that the we will need more txouts now
      if positiveValue modifiedChange1
        then makeTransactionBody (modifiedBody inputs1 modifiedChange1 fee1 )
        else do
          txbody2 <- makeTransactionBody bodyContent2
          let fee2=evaluateTransactionFee pParams txbody2 1 0
              modifiedChange2= change2 <> negLovelace fee2 <> lovelaceToValue fee1
          if fee2 == fee1
            then Right  txbody2
            else  makeTransactionBody (modifiedBody inputs2 modifiedChange2 fee2)

  where
  performBalance txbody  change fee= do
            let (inputs,change') =minimize txouts (change <> negLovelace fee)
                bodyContent=modifiedBody inputs change' fee
            txBody1<-makeTransactionBody bodyContent

            let modifiedChange1=change' <> negLovelace  fee' <> lovelaceToValue fee
                fee'= evaluateTransactionFee pParams txBody1 1 0
                (inputs2,change2)= minimize txouts modifiedChange1
                newBody =modifiedBody inputs2 change2 fee'
            if fee' == fee
              then Right (bodyContent,change,fee)
              else Right (newBody, modifiedChange1,fee')

  startingFee=Lovelace $ toInteger $ protocolParamTxFeeFixed pParams

  negLovelace v=negateValue $ lovelaceToValue v

  txouts=  Map.toList utxoMap
  utxosWithWitness (txin,txout) = (txin, BuildTxWith  $ KeyWitness KeyWitnessForSpending)


  minimize utxos remainingChange= case utxos of
    []     -> ([] ,remainingChange)
    (txIn,txOut):subUtxos -> if val`valueLte` remainingChange
            then minimize subUtxos newChange -- remove the current txOut from the list
            else (case minimize subUtxos remainingChange of { (tos, va) -> (txBodyIn :tos,va) }) -- include txOut in result
          where
            val = txOutValueToValue $ txOutValue  txOut
            newChange= remainingChange <> negateValue val
            txBodyIn =(txIn,BuildTxWith $ KeyWitness KeyWitnessForSpending)

  minimize' utxos remainingChange = (doMap,remainingChange)
    where
      doMap=map (\(txin,txout) -> (tobodyIn txin)) utxos
      tobodyIn _in=(_in,BuildTxWith $ KeyWitness KeyWitnessForSpending)
      val  out= txOutValueToValue $ txOutValue  out




  startingChange=(negateValue $ foldMap (txOutValueToValue  . txOutValue ) (txOuts txbody))  --already existing outputs
                  <> (if  null (txIns txbody) then mempty else inputSum) -- already existing inputs
                  <> (foldMap (txOutValueToValue  . txOutValue) $  Map.elems utxoMap) -- sum of all the available utxos

  utxoToTxOut (UTxO map)=Map.toList map

  txOutValueToValue :: TxOutValue era -> Value
  txOutValueToValue tv =
    case tv of
      TxOutAdaOnly _ l -> lovelaceToValue l
      TxOutValue _ v -> v

  txOutValue (TxOut _ v _) = v

  modifiedBody txins change fee= content
    where
      content=(TxBodyContent  {
            txIns=txins ++ txIns txbody,
            txInsCollateral=txInsCollateral txbody,
            txOuts=  if nullValue change
                  then txOuts txbody
                  else txOuts txbody ++ [ TxOut  walletAddr (TxOutValue MultiAssetInAlonzoEra change) TxOutDatumHashNone]  ,
              -- [TxOut alonzoAddr (TxOutValue MultiAssetInAlonzoEra (lovelaceToValue $ Lovelace 5927107)) TxOutDatumHashNone ],
            txFee=TxFeeExplicit TxFeesExplicitInAlonzoEra  fee,
            -- txValidityRange=(TxValidityNoLowerBound,TxValidityNoUpperBound ValidityNoUpperBoundInAlonzoEra),
            txValidityRange = txValidityRange txbody,
            txMetadata=txMetadata txbody ,
            txAuxScripts=txAuxScripts txbody,
            txExtraScriptData=txExtraScriptData txbody ,
            txExtraKeyWits=txExtraKeyWits txbody,
            txProtocolParams= txProtocolParams   txbody,
            txWithdrawals=txWithdrawals txbody,
            txCertificates=txCertificates txbody,
            txUpdateProposal=txUpdateProposal txbody,
            txMintValue=txMintValue txbody,
            txScriptValidity=txScriptValidity txbody
          })

executeMkBalancedBody :: ProtocolParameters
  -> UTxO era
  -> TxBodyContent BuildTx AlonzoEra
  -> Value
  -> AddressInEra AlonzoEra
  -> IO (TxBody AlonzoEra)
executeMkBalancedBody  pParams utxos  txbody inputSum walletAddr=do
  let balancedBody=mkBalancedBody pParams utxos txbody inputSum walletAddr
  case balancedBody of
    Left e -> throw $ SomeError $ show e
    Right g ->pure g


submitEitherBalancedBody conn eitherBalancedBody skey =
      --End Balance transaction body with fee
  case  eitherBalancedBody of
    Left tbe ->
      throw $ SomeError $  "Coding Error : Tx Body has error : " ++  (show tbe)
    Right txBody -> do
      let (ins,outs)=case txBody of { ShelleyTxBody sbe (LedgerBody.TxBody ins outs _ _ _ _ _ _ _ _ _ _ _ ) scs tbsd m_ad tsv -> (ins,outs) }
          tx = makeSignedTransaction [ makeShelleyKeyWitness txBody (WitnessPaymentKey skey) ] (txBody) -- witness and txBody
      res <-submitTxToNodeLocal conn $  TxInMode tx AlonzoEraInCardanoMode
      case res of
        SubmitSuccess ->  pure $ getTxId txBody
        SubmitFail reason ->
          case reason of
            TxValidationErrorInMode err _eraInMode ->  throw$ SomeError $ show  err
            TxValidationEraMismatch mismatchErr -> throw $ SomeError $ show  mismatchErr


balanceAndSubmitBody conn sKey body utxos sum  = do
  pParam <-case txProtocolParams body of {
    BuildTxWith m_pp -> case m_pp of
        Just pp -> pure pp
        Nothing -> do
          paramQueryResult<-queryNodeLocalState conn Nothing $
            QueryInEra AlonzoEraInCardanoMode
                  $ QueryInShelleyBasedEra ShelleyBasedEraAlonzo QueryProtocolParameters
          case paramQueryResult of
            Left af -> throw $ SomeError  "Acquire Failure"
            Right e -> case e of
              Left em -> throw $ SomeError "Missmatched Era"
              Right pp -> return pp
    }
  let balancedBody = mkBalancedBody pParam  utxos body sum   $  skeyToAddrInEra sKey
  submitEitherBalancedBody conn  balancedBody sKey

queryProtocolParam :: LocalNodeConnectInfo CardanoMode -> IO ProtocolParameters
queryProtocolParam conn=do
  paramQueryResult<-queryNodeLocalState conn Nothing $
            QueryInEra AlonzoEraInCardanoMode
                  $ QueryInShelleyBasedEra ShelleyBasedEraAlonzo QueryProtocolParameters
  case paramQueryResult of
    Left af -> throw $ SomeError  "Acquire Failure"
    Right e -> case e of
      Left em -> throw $ SomeError "Missmatched Era"
      Right pp -> return pp

querySystemStart conn=do
  result<-queryNodeLocalState conn Nothing QuerySystemStart
  case result of
    Left af -> throw $ SomeError "Acquire Failure"
    Right ss -> pure ss

queryEraHistory conn=do
  result <- queryNodeLocalState conn Nothing (QueryEraHistory (CardanoModeIsMultiEra ))
  case result of
    Left af -> throw $ SomeError "Acquire Failure"
    Right eh -> pure eh

printTxSubmited txid=putStrLn $ "Transaction Submitted :"++ ( init $ tail $ show txid)

marketCardanoApiAddress :: IsShelleyBasedEra era => NetworkId -> Market -> AddressInEra era
marketCardanoApiAddress network market =makeShelleyAddressInEra network scriptCredential NoStakeAddress
  where
  marketScript =PlutusScript PlutusScriptV1  $ marketScriptPlutus market
  marketHash=hashScript   marketScript
  scriptCredential=PaymentCredentialByScript marketHash

