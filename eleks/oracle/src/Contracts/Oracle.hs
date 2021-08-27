{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE DerivingStrategies    #-}

module Contracts.Oracle
    ( Oracle (..)
    , OracleRedeemer (..)
    , oracleTokenName
    , oracleValue
    , oracleAsset
    , typedOracleValidator
    , oracleValidator
    , oracleAddress
    , OracleSchema
    , OracleParams (..)
    , OracleData (..)
    , runOracle
    , findOracle
    , oracleTokenName
    , requestOracleForAddress
    , requestTokenSymbol
    , listenOracleRequest
    ) where

import           Control.Monad             hiding (fmap)
import           Data.Aeson                (FromJSON, ToJSON)
import qualified Data.Map                  as Map
import           Data.Monoid               (Last (..))
import           Data.Text                 (Text, pack)
import           Data.List.NonEmpty        (NonEmpty)
import           GHC.Generics              (Generic)
import           Plutus.Contract           as Contract
import qualified PlutusTx
import           PlutusTx.Prelude          hiding (Semigroup(..), unless)
import           Ledger                    hiding (singleton, MintingPolicyHash)
import qualified Ledger.Scripts            as LedgerScripts
import qualified Ledger.Tx            as LedgerScripts
import           Ledger.Constraints        as Constraints
import qualified Ledger.Contexts           as Validation
import           Ledger.Oracle             (Observation, SignedMessage, signMessage)
import qualified Ledger.Typed.Scripts      as Scripts
import           Ledger.Value              as Value
import           Ledger.Ada                as Ada
import           Plutus.Contracts.Currency as Currency
import           Plutus.Contract.Types     (Promise (..))
import           Prelude                   (Semigroup (..), Show (..), String)
import qualified Prelude                   as Haskell
import           Schema                    (ToSchema)

data Oracle = Oracle
    { oSymbol   :: !CurrencySymbol
    , oOperator :: !PubKeyHash
    , oFee      :: !Integer
    , oGame     :: !Integer
    } deriving (Show, Generic, FromJSON, ToJSON, ToSchema, Haskell.Eq, Haskell.Ord)

PlutusTx.makeLift ''Oracle

data OracleData = OracleData
    { ovGame           :: Integer
    , ovWinner         :: Integer
    , ovRequestAddress :: PubKeyHash
    , ovWinnerSigned   :: Maybe (SignedMessage Integer)
    }
    deriving (Show, Generic, FromJSON, ToJSON, Haskell.Eq)

PlutusTx.unstableMakeIsData ''OracleData
PlutusTx.makeLift ''OracleData


data OracleRedeemer = Update | Use | Sign | RequestToken
    deriving Show

PlutusTx.unstableMakeIsData ''OracleRedeemer

{-# INLINABLE oracleTokenName #-}
oracleTokenName :: TokenName
oracleTokenName = TokenName "oracleTokenName"

{-# INLINABLE oracleAsset #-}
oracleAsset :: Oracle -> AssetClass
oracleAsset oracle = AssetClass (oSymbol oracle, oracleTokenName)

{-# INLINABLE oracleValue #-}
oracleValue :: TxOut -> (DatumHash -> Maybe Datum) -> Maybe OracleData
oracleValue o f = do
    dh      <- txOutDatum o
    Datum d <- f dh
    PlutusTx.fromBuiltinData d

{-# INLINABLE mkOracleValidator #-}
mkOracleValidator :: Oracle -> OracleData -> OracleRedeemer -> ScriptContext -> Bool
mkOracleValidator oracle oracleData r ctx =
    --traceIfFalse "token missing from input"  inputHasToken  &&
    --traceIfFalse "token missing from output" outputHasToken &&
    case r of
        Update -> traceIfFalse "operator signature missing" (txSignedBy info $ oOperator oracle) &&
                  traceIfFalse "invalid output datum"       validOutputDatum
        Use    ->
                 -- TODOL fix 
                 --traceIfFalse "oracle value changed"       (outputDatum == Just oracleData)              &&
                  traceIfFalse "fees not paid"              feesPaid
        Sign   -> traceIfFalse "operator signature missing" (txSignedBy info $ oOperator oracle) &&
                  traceIfFalse "invalid output datum"       validOutputDatum

  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    forged :: Value
    forged = txInfoMint $ scriptContextTxInfo ctx

    ownInput :: TxOut
    ownInput = case findOwnInput ctx of
        Nothing -> traceError "oracle input missing"
        Just i  -> txInInfoResolved i

    inputHasToken :: Bool
    inputHasToken = assetClassValueOf (txOutValue ownInput) (oracleAsset oracle) == 1

    ownOutput :: TxOut
    ownOutput = case getContinuingOutputs ctx of
        [o] -> o
        _   -> traceError "expected exactly one oracle output"

    outputHasToken :: Bool
    outputHasToken = assetClassValueOf (txOutValue ownOutput) (oracleAsset oracle) == 1

    outputDatum :: Maybe OracleData
    outputDatum = oracleValue ownOutput (`findDatum` info)

    validOutputDatum :: Bool
    validOutputDatum = isJust outputDatum

    feesPaid :: Bool
    feesPaid =
      let
        inVal  = txOutValue ownInput
        outVal = txOutValue ownOutput
      in
        outVal `geq` (inVal <> Ada.lovelaceValueOf (oFee oracle))

data Oracling
instance Scripts.ValidatorTypes Oracling where
    type instance DatumType Oracling = OracleData
    type instance RedeemerType Oracling = OracleRedeemer

typedOracleValidator :: Oracle -> Scripts.TypedValidator Oracling
typedOracleValidator oracle = Scripts.mkTypedValidator @Oracling
    ($$(PlutusTx.compile [|| mkOracleValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode oracle)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @OracleData @OracleRedeemer

oracleValidator :: Oracle -> Validator
oracleValidator = Scripts.validatorScript . typedOracleValidator

oracleValidatorHash :: Oracle -> Ledger.ValidatorHash
oracleValidatorHash oracle = LedgerScripts.validatorHash . oracleValidator $ oracle

oracleAddress :: Oracle -> Ledger.Address
oracleAddress = scriptAddress . oracleValidator

{-# INLINABLE checkRequesTokenPolicy #-}
checkRequesTokenPolicy :: Oracle -> Ledger.Address -> () -> ScriptContext -> Bool
checkRequesTokenPolicy oracle oracleAddress _ ctx@ScriptContext{scriptContextTxInfo=TxInfo{txInfoInputs}, scriptContextPurpose=Minting _} = 
    traceIfFalse "Should forge one token" (forgedSymbolsCount == 1)
    && traceIfFalse "Is requet token and fee send to script" (isTokenAndFeeSendToAddress oracleAddress)
    where
        ownSymbol = ownCurrencySymbol ctx
        info = scriptContextTxInfo ctx
        forged = txInfoMint info
        forgedSymbolsCount = length $ symbols forged
        feeValue = Ada.toValue . Ada.lovelaceOf $ oFee oracle
        expectedOracleValue = feeValue <> forged
        isTokenAndFeeSendToAddress :: Address -> Bool
        isTokenAndFeeSendToAddress addr = isJust . find (\o ->
            txOutValue o == expectedOracleValue &&
            toPubKeyHash addr == Validation.pubKeyOutput o) $ txInfoOutputs info

requestTokenPolicy :: Oracle -> Ledger.Address -> LedgerScripts.MintingPolicy
requestTokenPolicy oracle oracleAddress = LedgerScripts.mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \oracle orAddress -> Scripts.wrapMintingPolicy (checkRequesTokenPolicy oracle orAddress) ||])
        `PlutusTx.applyCode`
            PlutusTx.liftCode oracle
        `PlutusTx.applyCode`
            PlutusTx.liftCode oracleAddress


oracleRequestMintPolicyHash :: Oracle -> Ledger.Address -> LedgerScripts.MintingPolicyHash
oracleRequestMintPolicyHash oracle address = mintingPolicyHash $ requestTokenPolicy oracle address

requestTokenSymbol :: Oracle -> Ledger.Address -> CurrencySymbol
requestTokenSymbol oracle address = Value.mpsSymbol $ oracleRequestMintPolicyHash oracle address

requestTokenClass :: Oracle -> Ledger.Address -> AssetClass
requestTokenClass oracle address = AssetClass (requestTokenSymbol oracle address, oracleTokenName)

data OracleParams = OracleParams
    { opSymbol   :: !CurrencySymbol
    , opFees   :: !Integer
    , opGame    :: !Integer
    } deriving (Haskell.Eq, Haskell.Show, Generic, FromJSON, ToJSON)

startOracle :: forall w s. OracleParams -> Contract w s Text Oracle
startOracle op = do
    pkh <- pubKeyHash <$> Contract.ownPubKey
    let oracle = Oracle
            { oSymbol   = opSymbol op
            , oOperator = pkh
            , oFee      = opFees op
            , oGame     = opGame op
            }
    logInfo @String $ "started oracle " ++ show oracle
    return oracle

updateOracle :: forall w s. Oracle -> OracleData -> Contract w s Text ()
updateOracle oracle oracleData = do
    m <- findOracle oracle
    let c = Constraints.mustPayToTheScript oracleData $ assetClassValue (oracleAsset oracle) 1
    case m of
        Nothing -> do
            ledgerTx <- submitTxConstraints (typedOracleValidator oracle) c
            awaitTxConfirmed $ txId ledgerTx
            logInfo @String $ "set initial oracle value to " ++ show oracleData
        Just (oref, o,  _) -> do
            let lookups = Constraints.unspentOutputs (Map.singleton oref o)     <>
                          Constraints.typedValidatorLookups (typedOracleValidator oracle) <>
                          Constraints.otherScript (oracleValidator oracle)
                tx      = c <> Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData Update)
            ledgerTx <- submitTxConstraintsWith @Oracling lookups tx
            awaitTxConfirmed $ txId ledgerTx
            logInfo @String $ "updated oracle value to " ++ show oracleData


oracelValueFromTxOutTx :: TxOutTx -> Maybe OracleData
oracelValueFromTxOutTx o = oracleValue (txOutTxOut o) $ \dh -> Map.lookup dh $ txData $ txOutTxTx o

findOracle :: forall w s. Oracle -> Contract w s Text (Maybe (TxOutRef, TxOutTx, OracleData))
findOracle oracle = do
    utxos <- Map.filter f <$> utxoAt (oracleAddress oracle)
    return $ case Map.toList utxos of
        [(oref, o)] -> do
            x <- oracelValueFromTxOutTx o
            return (oref, o, x)
        _           -> Nothing
  where
    f :: TxOutTx -> Bool
    f o = assetClassValueOf (txOutValue $ txOutTxOut o) (oracleAsset oracle) == 1

type OracleSchema = Endpoint "update" OracleData

requestOracleForAddress :: forall w s. Oracle -> Contract w s Text ()
requestOracleForAddress oracle = do 
    pkh <- pubKeyHash <$> ownPubKey
    let inst = typedOracleValidator oracle
        mrScript = oracleValidator oracle
        addr = oracleAddress oracle
        tokenMintingPolicy = requestTokenPolicy oracle addr
        forgedToken = Value.singleton (requestTokenSymbol oracle addr) oracleTokenName 1
        oracleFee = oFee oracle 
        feeVal = Ada.toValue . Ada.lovelaceOf $ oracleFee
        oracleData = OracleData 
            { ovRequestAddress = pkh
            , ovGame = oGame oracle
            , ovWinner = 0
            , ovWinnerSigned = Nothing
            }

    let lookups = Constraints.typedValidatorLookups inst 
                <> Constraints.otherScript mrScript
                <> Constraints.mintingPolicy tokenMintingPolicy

        tx      = Constraints.mustPayToTheScript oracleData (feeVal <> forgedToken)
                <> Constraints.mustMintValue forgedToken

    ledgerTx <- submitTxConstraintsWith lookups tx
    void $ awaitTxConfirmed $ txId ledgerTx

extract :: Maybe a -> a
extract (Just x) = x
extract Nothing  = Haskell.undefined 

listenOracleRequest :: Oracle -> PrivateKey -> Contract w s Text ()
listenOracleRequest oracle oraclePrivKey= do 
    listen
  where
    signUtxo:: Oracle -> Tx -> Contract w s Text ()
    signUtxo oracle listenTx = do
        let test = txOutRefs $ listenTx 
            oracleAddr = oracleAddress oracle
        let requetTokenRefM = find (\(o, or) -> assetClassValueOf (txOutValue o) (requestTokenClass oracle oracleAddr) == 1) . txOutRefs $ listenTx 
        logInfo ("process tx utxos " ++ (show $ txOutRefs $ listenTx))
        when (PlutusTx.Prelude.isNothing requetTokenRefM) $ throwError "no request token in utxo"
        let (o, oref) = extract requetTokenRefM 
            txOutTx = TxOutTx listenTx o
            oracleDataM = oracelValueFromTxOutTx txOutTx
        when (PlutusTx.Prelude.isNothing oracleDataM) $ throwError "no oracle data in utxo"
        let oracleData = extract oracleDataM 
            -- TODO
            winnerId = 1
            oracleData' = oracleData { ovWinner = winnerId, ovWinnerSigned = Just $ signMessage 1 oraclePrivKey }
            requestTokenVal = Value.singleton (requestTokenSymbol oracle oracleAddr) oracleTokenName 1
        let 
            lookups = Constraints.unspentOutputs (Map.singleton oref txOutTx)     
                    <> Constraints.typedValidatorLookups (typedOracleValidator oracle) 
                    <> Constraints.otherScript (oracleValidator oracle)
            tx      = Constraints.mustPayToTheScript oracleData' requestTokenVal 
                    <> Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData Sign)

        ledgerTx <- submitTxConstraintsWith lookups tx
        void $ awaitTxConfirmed $ txId ledgerTx

    listen = do
        txs <- awaitUtxoProduced $ oracleAddress oracle
        forM_ txs $ \onchainTx -> do
             case onchainTx of
                (Valid tx) -> 
                    handleError (\e -> logError e ) $ signUtxo oracle tx
                _          -> return ()
        listen
       
runOracle :: OracleParams -> Contract (Last Oracle) OracleSchema Text ()
runOracle op = do
    oracle <- startOracle op
    tell $ Last $ Just oracle
    selectList[(go oracle)]
  where
    go :: Oracle -> Promise (Last Oracle) OracleSchema Text ()
    go oracle = endpoint @"update" $ \oracleData -> do
            updateOracle oracle oracleData
