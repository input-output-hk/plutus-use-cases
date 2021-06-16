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

module NodeFactory.Plutus.Contracts.Oracle.Core
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
    , findOracle
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
import           Prelude                   (Semigroup (..), String, show)
import qualified Prelude                   as Prelude

-- contract param structure
data Oracle = Oracle
    { oSymbol   :: !CurrencySymbol  -- currency symbol of NFT
    , oOperator :: !PubKeyHash      -- owner of the oracle
    , oFee      :: !Integer         -- fees in lovelace
    , oAsset    :: !AssetClass      -- target of the oracle (US dollar)
    } deriving (Prelude.Show, Generic, FromJSON, ToJSON, Prelude.Eq, Prelude.Ord)

PlutusTx.makeLift ''Oracle

data OracleRedeemer = Update | Use  -- two availabe operations
    deriving Prelude.Show

PlutusTx.unstableMakeIsData ''OracleRedeemer

-- HELPER FUNCTIONS

{-# INLINABLE oracleTokenName #-}   -- use empty string for token name
oracleTokenName :: TokenName
oracleTokenName = TokenName emptyByteString

{-# INLINABLE oracleAsset #-}
oracleAsset :: Oracle -> AssetClass -- identify asset class if oracle provided                                          
oracleAsset oracle = AssetClass (oSymbol oracle, oracleTokenName)

{-# INLINABLE oracleValue #-}       -- convert transaction output datum to integer
oracleValue :: TxOut -> (DatumHash -> Maybe Datum) -> Maybe Integer
oracleValue o f = do
    dh      <- txOutDatum o 
    Datum d <- f dh
    PlutusTx.fromData d

{-# INLINABLE mkOracleValidator #-} -- validator functions
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
    info :: TxInfo                  -- extract transaction info from context
    info = scriptContextTxInfo ctx

    ownInput :: TxOut               -- get oracle input
    ownInput = case findOwnInput ctx of
        Nothing -> traceError "oracle input missing"
        Just i  -> txInInfoResolved i

    inputHasToken :: Bool           -- check if input contains nft token
    inputHasToken = assetClassValueOf (txOutValue ownInput) (oracleAsset oracle) == 1

    ownOutput :: TxOut              -- get oracle output
    ownOutput = case getContinuingOutputs ctx of
        [o] -> o
        _   -> traceError "expected exactly one oracle output"

    outputHasToken :: Bool          -- check if output contains nft token
    outputHasToken = assetClassValueOf (txOutValue ownOutput) (oracleAsset oracle) == 1

    outputDatum :: Maybe Integer    -- get data from output datum
    outputDatum = oracleValue ownOutput (`findDatum` info)

    validOutputDatum :: Bool        -- check if type of data in output datum valid
    validOutputDatum = isJust outputDatum

    feesPaid :: Bool                -- check if user paid fees
    feesPaid =
      let
        inVal  = txOutValue ownInput
        outVal = txOutValue ownOutput
      in
        outVal `geq` (inVal <> Ada.lovelaceValueOf (oFee oracle))

data Oracling
instance Scripts.ScriptType Oracling where
    type instance DatumType Oracling = Integer
    type instance RedeemerType Oracling = OracleRedeemer

oracleInst :: Oracle -> Scripts.ScriptInstance Oracling
oracleInst oracle = Scripts.validator @Oracling
    ($$(PlutusTx.compile [|| mkOracleValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode oracle)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @Integer @OracleRedeemer

oracleValidator :: Oracle -> Validator
oracleValidator = Scripts.validatorScript . oracleInst

oracleAddress :: Oracle -> Ledger.Address
oracleAddress = scriptAddress . oracleValidator

data OracleParams = OracleParams    -- parans fir starting oracle
    { opFees   :: !Integer
    , opSymbol :: !CurrencySymbol
    , opToken  :: !TokenName
    } deriving (Prelude.Show, Generic, FromJSON, ToJSON)

startOracle :: forall w s. HasBlockchainActions s => OracleParams -> Contract w s Text Oracle
startOracle op = do
    pkh <- pubKeyHash <$> Contract.ownPubKey
    -- mint NFT using forgeContract function (provide owner pkh and array of nft-s that are going to be minted) 
    -- use mapError to convert String error to Text error
    osc <- mapError (pack . show) (forgeContract pkh [(oracleTokenName, 1)] :: Contract w s CurrencyError OneShotCurrency)
    let cs     = Currency.currencySymbol osc
        oracle = Oracle
            { oSymbol   = cs
            , oOperator = pkh
            , oFee      = opFees op
            , oAsset    = AssetClass (opSymbol op, opToken op)
            }
    logInfo @String $ "started oracle " ++ show oracle
    return oracle

updateOracle :: forall w s. HasBlockchainActions s => Oracle -> Integer -> Contract w s Text ()
updateOracle oracle x = do
    m <- findOracle oracle -- check if oracle exists
    let c = Constraints.mustPayToTheScript x $ assetClassValue (oracleAsset oracle) 1
    case m of
        Nothing -> do -- oracle nft not created
            ledgerTx <- submitTxConstraints (oracleInst oracle) c
            awaitTxConfirmed $ txId ledgerTx
            logInfo @String $ "set initial oracle value to " ++ show x
        Just (oref, o,  _) -> do -- update existing oracle nft value
            let lookups = Constraints.unspentOutputs (Map.singleton oref o)     <>
                          Constraints.scriptInstanceLookups (oracleInst oracle) <>
                          Constraints.otherScript (oracleValidator oracle)
                tx      = c <> Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toData Update)
            -- fees auto send because of inbalance
            ledgerTx <- submitTxConstraintsWith @Oracling lookups tx
            awaitTxConfirmed $ txId ledgerTx
            logInfo @String $ "updated oracle value to " ++ show x

-- 
findOracle :: forall w s. HasBlockchainActions s => Oracle -> Contract w s Text (Maybe (TxOutRef, TxOutTx, Integer))
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

type OracleSchema = BlockchainActions .\/ Endpoint "update" Integer

runOracle :: OracleParams -> Contract (Last Oracle) OracleSchema Text ()
runOracle op = do
    oracle <- startOracle op
    tell $ Last $ Just oracle
    go oracle
  where
    go :: Oracle -> Contract (Last Oracle) OracleSchema Text a
    go oracle = do
        x <- endpoint @"update"
        updateOracle oracle x
        go oracle