{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE TypeFamilies #-}
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
import qualified Data.ByteString.Lazy as LBS
import Plutus.Contract.Blockchain.MarketPlace (marketValidator, Market (..), DirectSale (DirectSale), SellType (Primary), marketAddress, MarketRedeemer (Buy))
import Wallet.Emulator
import Ledger (pubKeyHash, datumHash, Datum (Datum))
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
import PlutusTx.Builtins (sha2_256, sha3_256)
import Data.ByteString.Lazy (toStrict)
import Data.Aeson (ToJSON(toJSON), encode)
import Codec.Serialise (serialise)
import Plutus.V1.Ledger.Address (Address(Address))
import qualified Data.Set as Set
import qualified Data.Map as Map
import Cardano.CLI.Shelley.Run.Transaction
import Control.Monad.Trans.Except
import Cardano.CLI.Shelley.Commands
import Cardano.CLI.Environment (readEnvSocketPath)




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
        "balance" -> putStrLn "Not implemented"

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

-- runTxBuild
--   :: AnyCardanoEra
--   -> AnyConsensusModeParams
--   -> NetworkId
--   -> Maybe ScriptValidity
--   -- ^ Mark script as expected to pass or fail validation
--   -> [(TxIn, Maybe (ScriptWitnessFiles WitCtxTxIn))]
--   -- ^ TxIn with potential script witness
--   -> [TxIn]
--   -- ^ TxIn for collateral
--   -> [TxOutAnyEra]
--   -- ^ Normal outputs
--   -> TxOutChangeAddress
--   -- ^ A change output
--   -> Maybe (Value, [ScriptWitnessFiles WitCtxMint])
--   -- ^ Multi-Asset value(s)
--   -> Maybe SlotNo
--   -- ^ Tx lower bound
--   -> Maybe SlotNo
--   -- ^ Tx upper bound
--   -> [(CertificateFile, Maybe (ScriptWitnessFiles WitCtxStake))]
--   -- ^ Certificate with potential script witness
--   -> [(StakeAddress, Lovelace, Maybe (ScriptWitnessFiles WitCtxStake))]
--   -> [WitnessSigningData]
--   -- ^ Required signers
--   -> TxMetadataJsonSchema
--   -> [ScriptFile]
--   -> [MetadataFile]
--   -> Maybe ProtocolParamsSourceSpec
--   -> Maybe UpdateProposalFile
--   -> TxBodyFile
--   -> Maybe Word
--   -> ExceptT ShelleyTxCmdError IO ()
runTxBuild (AnyCardanoEra era) (AnyConsensusModeParams cModeParams) networkId mScriptValidity txins txinsc txouts
           (TxOutChangeAddress changeAddr) mValue mLowerBound mUpperBound certFiles withdrawals reqSigners
           metadataSchema scriptFiles metadataFiles mpparams mUpdatePropFile outBody@(TxBodyFile fpath)
           mOverrideWits = do
  SocketPath sockPath <-  readEnvSocketPath

  let localNodeConnInfo = LocalNodeConnectInfo cModeParams networkId sockPath
      consensusMode = consensusModeOnly cModeParams
      dummyFee = Just $ Lovelace 0
      onlyInputs = [input | (input,_) <- txins]

  case (consensusMode, cardanoEraStyle era) of
    (CardanoMode, ShelleyBasedEra sbe) -> do
      txBodyContent <-
        TxBodyContent
          <$> validateTxIns               era txins
          <*> validateTxInsCollateral     era txinsc
          <*> validateTxOuts              era txouts
          <*> validateTxFee               era dummyFee
          <*> ((,) <$> validateTxValidityLowerBound era mLowerBound
                   <*> validateTxValidityUpperBound era mUpperBound)
          <*> validateTxMetadataInEra     era metadataSchema metadataFiles
          <*> validateTxAuxScripts        era scriptFiles
          <*> pure (BuildTxWith TxExtraScriptDataNone) --TODO alonzo: support this
          <*> validateRequiredSigners     era reqSigners
          <*> validateProtocolParameters  era mpparams
          <*> validateTxWithdrawals       era withdrawals
          <*> validateTxCertificates      era certFiles
          <*> validateTxMintValue         era mValue
          <*> validateTxScriptValidity    era mScriptValidity

      eInMode <- case toEraInMode era CardanoMode of
                   Just result -> return result
                   Nothing ->
                     left (ShelleyTxCmdEraConsensusModeMismatchTxBalance outBody
                            (AnyConsensusMode CardanoMode) (AnyCardanoEra era))

      (utxo, pparams, eraHistory, systemStart, stakePools) <-
        newExceptT . fmap (join . first ShelleyTxCmdAcquireFailure) $
          executeLocalStateQueryExpr localNodeConnInfo Nothing $ \_ntcVersion -> runExceptT $ do
            unless (null txinsc) $ do
              collateralUtxo <- firstExceptT ShelleyTxCmdTxSubmitErrorEraMismatch . newExceptT . queryExpr
                $ QueryInEra eInMode
                $ QueryInShelleyBasedEra sbe (QueryUTxO . QueryUTxOByTxIn $ Set.fromList txinsc)
              txinsExist txinsc collateralUtxo
              notScriptLockedTxIns collateralUtxo

            utxo <- firstExceptT ShelleyTxCmdTxSubmitErrorEraMismatch . newExceptT . queryExpr
              $ QueryInEra eInMode $ QueryInShelleyBasedEra sbe
              $ QueryUTxO (QueryUTxOByTxIn (Set.fromList onlyInputs))

            txinsExist onlyInputs utxo

            pparams <- firstExceptT ShelleyTxCmdTxSubmitErrorEraMismatch . newExceptT . queryExpr
              $ QueryInEra eInMode $ QueryInShelleyBasedEra sbe QueryProtocolParameters

            eraHistory <- lift . queryExpr $ QueryEraHistory CardanoModeIsMultiEra

            systemStart <- lift $ queryExpr QuerySystemStart


            stakePools <- firstExceptT ShelleyTxCmdTxSubmitErrorEraMismatch . ExceptT $
              queryExpr . QueryInEra eInMode . QueryInShelleyBasedEra sbe $ QueryStakePools

            return (utxo, pparams, eraHistory, systemStart, stakePools)

      let cAddr = case anyAddressInEra era changeAddr of
                    Just addr -> addr
                    Nothing -> error $ "runTxBuild: Byron address used: " <> show changeAddr

      (BalancedTxBody balancedTxBody _ fee) <-
        firstExceptT ShelleyTxCmdBalanceTxBody
          . hoistEither
          $ makeTransactionBodyAutoBalance eInMode systemStart eraHistory
                                           pparams stakePools utxo txBodyContent
                                           cAddr mOverrideWits

      putStrLn $ "Estimated transaction fee: " <> (show fee :: String)

      firstExceptT ShelleyTxCmdWriteFileError . newExceptT
        $ writeFileTextEnvelope fpath Nothing balancedTxBody

    (CardanoMode, LegacyByronEra) -> left ShelleyTxCmdByronEra

    (wrongMode, _) -> left (ShelleyTxCmdUnsupportedMode (AnyConsensusMode wrongMode))
  where
    txinsExist :: Monad m => [TxIn] -> UTxO era -> ExceptT ShelleyTxCmdError m ()
    txinsExist ins (UTxO utxo)
      | null utxo = left $ ShelleyTxCmdTxInsDoNotExist ins
      | otherwise = do
          let utxoIns = Map.keys utxo
              occursInUtxo = [ txin | txin <- ins, txin `elem` utxoIns ]
          if length occursInUtxo == length ins
          then return ()
          else left . ShelleyTxCmdTxInsDoNotExist $ ins \\ ins `intersect` occursInUtxo

    notScriptLockedTxIns :: Monad m => UTxO era -> ExceptT ShelleyTxCmdError m ()
    notScriptLockedTxIns (UTxO utxo) = do
      let scriptLockedTxIns =
            filter (\(_, TxOut aInEra _ _) -> not $ isKeyAddress aInEra ) $ Map.assocs utxo
      if null scriptLockedTxIns
      then return ()
      else left . ShelleyTxCmdExpectedKeyLockedTxIn $ map fst scriptLockedTxIns