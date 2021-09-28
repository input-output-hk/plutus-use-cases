{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ViewPatterns          #-}
{-# OPTIONS_GHC -fno-specialise #-}

module Plutus.Contracts.Service.Oracle
    ( Oracle (..)
    , OracleRedeemer (..)
    , oracleTokenName
    , oracleValue
    , oracleAsset
    , oracleInst
    , oracleValidator
    , oracleAddress
    , OracleSchema
    , OracleParams (..)
    , runOracle
    , startOracle
    , updateOracle
    , findOracle
    , useOracle
    , fromTuple
    , toTuple
    , findOracleValueInTxInputs
    ) where

import           Control.Monad             hiding (fmap)
import           Data.Aeson                (FromJSON, ToJSON)
import qualified Data.Map                  as Map
import           Data.Monoid               (Last (..))
import           Data.Text                 (Text, pack)
import           GHC.Generics              (Generic)
import           Ledger                    hiding (singleton)
import           Ledger.Ada                as Ada
import           Ledger.Constraints        as Constraints
import qualified Ledger.Typed.Scripts      as Scripts
import           Ledger.Value              as Value
import qualified Plutus.Abstract.TxUtils   as TxUtils
import           Plutus.Contract           as Contract hiding (when)
import           Plutus.Contracts.Currency as Currency
import qualified PlutusTx
import           PlutusTx.Prelude          hiding (Semigroup (..), unless)
import           Prelude                   (Semigroup (..))
import qualified Prelude
import           Schema                    (ToSchema)

data Oracle = Oracle
    { oSymbol   :: CurrencySymbol
    , oOperator :: PubKeyHash
    , oFee      :: Integer
    , oAsset    :: AssetClass
    }
    deriving stock (Prelude.Eq, Prelude.Ord, Prelude.Show, Generic)
    deriving anyclass (ToJSON, FromJSON, ToSchema)

PlutusTx.makeLift ''Oracle
PlutusTx.unstableMakeIsData ''Oracle

instance Eq Oracle where
    {-# INLINABLE (==) #-}
    (Oracle oSymbol oOperator oFee oAsset) == (Oracle oSymbol' oOperator' oFee' oAsset') =
      oSymbol == oSymbol' &&
      oOperator == oOperator' && oFee == oFee' && oAsset == oAsset'

{-# INLINABLE fromTuple #-}
fromTuple :: (CurrencySymbol, PubKeyHash, Integer, AssetClass) -> Oracle
fromTuple (oSymbol, oOperator, oFee, oAsset) = Oracle {..}

{-# INLINABLE toTuple #-}
toTuple :: Oracle -> (CurrencySymbol, PubKeyHash, Integer, AssetClass)
toTuple Oracle{..} = (oSymbol, oOperator, oFee, oAsset)

data OracleRedeemer = Update | Use
    deriving stock (Prelude.Eq, Prelude.Ord, Prelude.Show, Generic)

PlutusTx.unstableMakeIsData ''OracleRedeemer

{-# INLINABLE oracleTokenName #-}
oracleTokenName :: TokenName
oracleTokenName = TokenName emptyByteString

{-# INLINABLE oracleAsset #-}
oracleAsset :: Oracle -> AssetClass
oracleAsset oracle = AssetClass (oSymbol oracle, oracleTokenName)

{-# INLINABLE oracleValue #-}
oracleValue :: TxOut -> (DatumHash -> Maybe Datum) -> Maybe Integer
oracleValue o f = do
    dh      <- txOutDatum o
    Datum d <- f dh
    PlutusTx.fromBuiltinData d

{-# INLINABLE findOracleValueInTxInputs #-}
findOracleValueInTxInputs :: TxInfo -> (CurrencySymbol, PubKeyHash, Integer, AssetClass) -> Maybe Integer
findOracleValueInTxInputs txInfo tuple = oracleTxOut >>= (`oracleValue` selector)
    where
        inputs = txInInfoResolved <$> txInfoInputs txInfo
        selector = (`findDatum` txInfo)
        oracleTxOut = find (hasOracleCoin . txOutValue) inputs
        hasOracleCoin val = assetClassValueOf val (oracleAsset . fromTuple $ tuple) == 1

{-# INLINABLE mkOracleValidator #-}
mkOracleValidator :: Oracle -> Integer -> OracleRedeemer -> ScriptContext -> Bool
mkOracleValidator oracle x r ctx =
    traceIfFalse "token missing from input"  inputHasToken  &&
    traceIfFalse "token missing from output" outputHasToken &&
    case r of
        Update -> traceIfFalse "operator signature missing" (txSignedBy info $ oOperator oracle) &&
                  traceIfFalse "invalid output datum"       validOutputDatum
        Use    -> traceIfFalse "oracle value changed"       (outputDatum == Just x)              &&
                  traceIfFalse "fees not paid"              feesPaid
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

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

    outputDatum :: Maybe Integer
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
    type instance DatumType Oracling = Integer
    type instance RedeemerType Oracling = OracleRedeemer

oracleInst :: Oracle -> Scripts.TypedValidator Oracling
oracleInst oracle = Scripts.mkTypedValidator @Oracling
    ($$(PlutusTx.compile [|| mkOracleValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode oracle)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @Integer @OracleRedeemer

oracleValidator :: Oracle -> Validator
oracleValidator = Scripts.validatorScript . oracleInst

oracleAddress :: Oracle -> Ledger.Address
oracleAddress = scriptAddress . oracleValidator

data OracleParams = OracleParams
    { opFees   :: Integer
    , opSymbol :: CurrencySymbol
    , opToken  :: TokenName
    }
    deriving stock (Prelude.Eq, Prelude.Show, Generic)
    deriving anyclass (ToJSON, FromJSON, ToSchema)

startOracle :: forall w s. OracleParams -> Contract w s Text Oracle
startOracle op = do
    pkh <- pubKeyHash <$> Contract.ownPubKey
    osc <- mapError (pack . Prelude.show) (mintContract pkh [(oracleTokenName, 1)] :: Contract w s CurrencyError OneShotCurrency)
    let cs     = Currency.currencySymbol osc
        oracle = Oracle
            { oSymbol   = cs
            , oOperator = pkh
            , oFee      = opFees op
            , oAsset    = AssetClass (opSymbol op, opToken op)
            }
    logInfo @Prelude.String $ "started oracle " ++ Prelude.show oracle
    return oracle

updateOracle :: forall w s. Oracle -> Integer -> Contract w s Text ()
updateOracle oracle x = do
    m <- findOracle oracle
    let c = Constraints.mustPayToTheScript x $ assetClassValue (oracleAsset oracle) 1
    case m of
        Nothing -> do
            ledgerTx <- submitTxConstraints (oracleInst oracle) c
            awaitTxConfirmed $ txId ledgerTx
            logInfo @Prelude.String $ "set initial oracle value to " ++ Prelude.show x
        Just (oref, o,  _) -> do
            let lookups = Constraints.unspentOutputs (Map.singleton oref o)     <>
                          Constraints.typedValidatorLookups (oracleInst oracle) <>
                          Constraints.otherScript (oracleValidator oracle)
                tx      = c <> Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData Update)
            ledgerTx <- submitTxConstraintsWith @Oracling lookups tx
            awaitTxConfirmed $ txId ledgerTx
            logInfo @Prelude.String $ "updated oracle value to " ++ Prelude.show x

findOracle :: forall w s. Oracle -> Contract w s Text (Maybe (TxOutRef, TxOutTx, Integer))
findOracle oracle = do
    utxos <- Map.filter f <$> utxoAt (oracleAddress oracle)
    return $ case Map.toList utxos of
        [(oref, o)] -> do
            x <- oracleValue (txOutTxOut o) $ \dh -> Map.lookup dh $ txData $ txOutTxTx o
            return (oref, o, x)
        _           -> Nothing
  where
    f :: TxOutTx -> Bool
    f o = assetClassValueOf (txOutValue $ txOutTxOut o) (oracleAsset oracle) == 1

useOracle ::
     forall a w s.
     ( TxUtils.IsScriptData a
     )
  => (CurrencySymbol, PubKeyHash, Integer, AssetClass)
  -> Contract w s Text (TxUtils.TxPair a)
useOracle (fromTuple -> oracle) = do
  (oracleRef, oracleOutTx, oracleDatum) <- findOracle oracle >>= maybe (throwError "useOracle: oracle not found") pure
  let unspent = Map.singleton oracleRef oracleOutTx
  let lookups =
        Constraints.otherScript (oracleValidator oracle) <>
        Constraints.unspentOutputs unspent
  let val = (assetClassValue oracleCoin 1) <> lovelaceValueOf (oFee oracle)
  let tx = Constraints.mustSpendScriptOutput oracleRef (Redeemer $ PlutusTx.toBuiltinData Use) <>
           Constraints.mustPayToOtherScript (validatorHash $ oracleValidator oracle) (Datum $ PlutusTx.toBuiltinData oracleDatum) val
  pure $ (lookups, tx)
  where
    oracleCoin = oracleAsset oracle

type OracleSchema = Endpoint "update" Integer

runOracle :: OracleParams -> Contract (Last Oracle) OracleSchema Text ()
runOracle op = do
    oracle <- startOracle op
    tell $ Last $ Just oracle
    go oracle
  where
    go :: Oracle -> Contract (Last Oracle) OracleSchema Text a
    go oracle =
        awaitPromise $ endpoint @"update" $ \x -> do
            updateOracle oracle x
            go oracle

