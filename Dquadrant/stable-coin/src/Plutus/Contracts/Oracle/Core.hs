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
{-# LANGUAGE NumericUnderscores #-}


module Plutus.Contracts.Oracle.Core
    ( Oracle (..)
    , Oracling
    , OracleRedeemer (..)
    , oracleTokenName
    , oracleValue
    , oracleNftAsset
    , oracleInst
    , oracleValidator
    , oracleAddress
    , OracleSchema
    , runOracle
    , oracleContract
    , runMockOracle
    , findOracle
    , mkOracleValidator
    ) where

import           Control.Monad             hiding (fmap)
import           Data.Aeson                (FromJSON, ToJSON)
import qualified Data.Map                  as Map
import           Data.Monoid               (Last (..))
import           Data.Text                 (Text, pack)
import           GHC.Generics              (Generic)
import           Plutus.Contract           as Contract hiding (when)
import qualified PlutusTx
import           PlutusTx.Prelude          hiding (Semigroup(..), unless)
import           Ledger                    hiding (singleton)
import           Ledger.Constraints        as Constraints
import qualified Ledger.Typed.Scripts      as Scripts
import           Ledger.Value              as Value
import           Ledger.Ada                as Ada
import           Plutus.Contracts.Currency as Currency
import           Prelude                   (Semigroup (..))
import qualified Prelude                   as Prelude

--Paramenter data of oracle to be used in oraclle validator
data Oracle = Oracle
    { oNftSymbol   :: !CurrencySymbol -- Nft symbol to identify correct utxo for getting exchange rate
    , oOperator    :: !PubKeyHash      -- Public key hash of operator of oracle like for updating the price
    , oFee         :: !Integer          -- Fee taken by oracle to use its value as an exchange rate
    } deriving (Prelude.Show, Generic, FromJSON, ToJSON)

PlutusTx.makeLift ''Oracle
PlutusTx.unstableMakeIsData ''Oracle

--Redeemer for Update and Use action provided in oracle contract
data OracleRedeemer = Update | Use
    -- deriving Prelude.Show

PlutusTx.unstableMakeIsData ''OracleRedeemer

--Construct a oracle token name as empty byte string
{-# INLINABLE oracleTokenName #-}
oracleTokenName :: TokenName
oracleTokenName = TokenName emptyByteString

--Construct a asset class of oracle symbol and oracle token name
{-# INLINABLE oracleNftAsset #-}
oracleNftAsset :: Oracle -> AssetClass
oracleNftAsset oracle = AssetClass (oNftSymbol oracle, oracleTokenName)

--Find value of oracle from tx output
{-# INLINABLE oracleValue #-}
oracleValue :: TxOut -> (DatumHash -> Maybe Datum) -> Maybe Integer
oracleValue o f = do
    dh      <- txOutDatum o
    Datum d <- f dh
    PlutusTx.fromBuiltinData d

--Oracle validator for checking update and use of oracle value
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
    inputHasToken = assetClassValueOf (txOutValue ownInput) (oracleNftAsset oracle) == 1

    ownOutput :: TxOut
    ownOutput = case getContinuingOutputs ctx of
        [o] -> o
        _   -> traceError "expected exactly one oracle output"

    outputHasToken :: Bool
    outputHasToken = assetClassValueOf (txOutValue ownOutput) (oracleNftAsset oracle) == 1

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

--Start oracle by first getting currecny symbol of nft to be used and make a oracle paramter using that currecny symbol
startOracle :: forall w s. Contract w s Text Oracle
startOracle = do
    logInfo @Prelude.String $ "Starting oracle "
    pkh <- pubKeyHash <$> Contract.ownPubKey
    osc <- mapError (pack . Prelude.show) (mintContract pkh [(oracleTokenName, 1)] :: Contract w s CurrencyError OneShotCurrency)
    let cs     = Currency.currencySymbol osc
        oracle = Oracle
            { oNftSymbol   = cs
            , oOperator = pkh
            , oFee      = 1_000_000
            }
    logInfo @Prelude.String $ "started oracle " ++ Prelude.show oracle
    return oracle

--Endpoint to update oracle which is only allowed by owner of the oracle provider
-- Find the oracle valie if nothing found then submit oracle value to oracle script
-- Otherwise spent previous ouput and construct new utxo with new oracle value at oracle address
updateOracle :: forall w s. Oracle -> Integer -> Contract w s Text ()
updateOracle oracle x = do
    m <- findOracle oracle
    let c = Constraints.mustPayToTheScript x $ assetClassValue (oracleNftAsset oracle) 1
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

-- Helper function to find oracle from list of utxos that are present at oracle address
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
    f o = assetClassValueOf (txOutValue $ txOutTxOut o) (oracleNftAsset oracle) == 1

type OracleSchema = Endpoint "update" Integer

errorHandler :: Prelude.Show a => a -> Contract w s e ()
errorHandler e = do
    Contract.logError $ Prelude.show e

oracleContract :: Contract (Last Oracle) OracleSchema Text ()
oracleContract = handleError errorHandler runOracle

--Contract for running oracle contract by first constructing a oracle paramter
runOracle :: Contract (Last Oracle) OracleSchema Text ()
runOracle = do
    oracle <- startOracle
    tell $ Last $ Just oracle
    awaitPromise $ go oracle
  where
    go :: Oracle -> Promise (Last Oracle) OracleSchema Text ()
    go oracle' = 
        (endpoint @"update") $ \x -> do
          updateOracle oracle' x
          awaitPromise $ go oracle'

--Run mock oracle to be used in test with default oracle provided form outside of contract
runMockOracle :: Oracle -> Contract () OracleSchema Text ()
runMockOracle oracle = 
    handleError errorHandler $ awaitPromise (go oracle) >> runMockOracle oracle
  where
    go :: Oracle -> Promise () OracleSchema Text ()
    go oracle' = 
        (endpoint @"update") $ \x -> do
          updateOracle oracle' x
