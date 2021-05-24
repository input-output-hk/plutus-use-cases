{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE MonoLocalBinds     #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE ViewPatterns       #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
-- | Implements a custom currency with a monetary policy that allows
--   the forging of a fixed amount of units.
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Plutus.Contract.Blockchain.MarketPlace
where

import Control.Monad ( Monad((>>), (>>=)), void )
import           GHC.Generics              (Generic)
import qualified Data.Map               as Map
import           Data.Text              (Text)
import           Data.Void              (Void)
import           Plutus.Contract        as Contract hiding (when)
import           Plutus.Trace.Emulator  as Emulator
import qualified PlutusTx
import qualified PlutusTx.AssocMap
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import           Ledger                 hiding (singleton)
import           Ledger.Constraints     as Constraints
import qualified Ledger.Typed.Scripts   as Scripts
import           Ledger.Value           as Value
import           Playground.Contract    (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema)
import           Playground.TH          (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types       (KnownCurrency (..))
import qualified Prelude
import           Prelude                (Semigroup (..))
import           Text.Printf            (printf)
import Data.Aeson (FromJSON, ToJSON)
import  Ledger.Ada
import qualified Ledger.Ada as Ada
import Data.Semigroup (Last)


data Market = Market
    { operator   :: !PubKeyHash
    , fee      :: !Integer
    } deriving (Show,Generic, FromJSON, ToJSON, Prelude.Eq, Prelude.Ord)

PlutusTx.makeLift ''Market

data MarketAction = Showcase | Buy | ClaimFees
PlutusTx.makeLift ''MarketAction
PlutusTx.unstableMakeIsData ''MarketAction

{-# INLINABLE mkMarket #-}
mkMarket :: Market -> Integer -> MarketAction -> ScriptContext -> Bool
mkMarket nft@Market{operator=operator,fee=fee} x action ctx@ScriptContext{scriptContextTxInfo=info@TxInfo{txInfoInputs=inputs},scriptContextPurpose=Spending txoutRef} =
    case action of
        Showcase  -> x>100
        Buy       -> traceIfFalse "Missing Fees" feesPaid
        ClaimFees -> traceIfFalse "Missing Operator signature" signedByOperator
    where

        inputs=filter (\TxInInfo{txInInfoOutRef} -> txInInfoOutRef == txoutRef) inputs
        resolvedInputs= map txInInfoResolved inputs
        resolvedDatums= map resolvedInputs 
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
        feesPaid =let
                inVal  = txOutValue ownInput
                outVal = txOutValue ownOutput
            in
                outVal `geq` (inVal <> Ada.lovelaceValueOf (oFee oracle))

data MarketType
instance Scripts.ScriptType MarketType where
    type instance DatumType MarketType = Integer
    type instance RedeemerType MarketType =  MarketAction 

marketScript :: Market -> Scripts.ScriptInstance MarketType
marketScript market = Scripts.validator @MarketType
    ($$(PlutusTx.compile [|| mkMarket ||]) `PlutusTx.applyCode` PlutusTx.liftCode market)
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.wrapValidator @Integer @MarketAction

marketValidator :: Market -> Validator
marketValidator = Scripts.validatorScript . marketScript

marketAddress :: Market -> Ledger.Address
marketAddress = scriptAddress . marketValidator

moveToMarket :: forall w s e. (HasBlockchainActions s,HasWriteTx s,AsContractError e ) =>Market  -> AssetClass ->Integer -> Contract w s e ()
moveToMarket market@Market{operator=operator,fee=fee} asset price= do 
    ledgerTx <- submitTxConstraints inst tx
    awaitTxConfirmed ( txId ledgerTx)
    where 
        value  = assetClassValue asset 1
        inst = marketScript market
        tx = Constraints.mustPayToTheScript price value

buyFromMarket :: forall w s. HasBlockchainActions s => Market -> AssetClass -> Contract w s Text ()
buyFromMarket market asset = do
    utxoInfo <- findInMarket market asset
    case utxoInfo of
        Nothing -> do
            logError  @String $ "It was either not on sale or is not on sale anymore " ++ show asset
        Just (oref, o,  cost) -> do
            let tx = Constraints.mustPayToTheScript cost   ( Ada.lovelaceValueOf cost) <>
                     Constraints.mustSpendScriptOutput oref (Redeemer ( PlutusTx.toData Buy))
                lookups = Constraints.unspentOutputs (Map.singleton oref o)     <>
                          Constraints.scriptInstanceLookups (marketScript market) 
            ledgerTx <- submitTxConstraintsWith @MarketType lookups tx
            awaitTxConfirmed $ txId ledgerTx
            logInfo @String $ "Bought asset "++ show asset ++ "at the cost of " ++ show cost ++" Lovelace"

findInMarket :: forall w s. HasBlockchainActions s => Market -> AssetClass  -> Contract w s Text (Maybe (TxOutRef, TxOutTx, Integer))
findInMarket market asset = do
    utxos <- Map.filter hasAsset <$> utxoAt (marketAddress market)
    return $ case Map.toList utxos of
        [(oref, o)] -> do
            x <- nftCost (txOutTxOut o) $ \dh -> Map.lookup dh $ txData $ txOutTxTx o
            return (oref, o, x)
        _           -> Nothing
  where
    hasAsset :: TxOutTx -> Bool
    hasAsset o = assetClassValueOf (txOutValue $ txOutTxOut o) asset == 1

    nftCost :: TxOut -> (DatumHash -> Maybe Datum) -> Maybe Integer
    nftCost o f = do
        dh      <- txOutDatum o
        Datum d <- f dh
        PlutusTx.fromData d

type MarketSchema =
    BlockchainActions
        .\/ Endpoint "sell"     (AssetClass, Integer)
--        .\/ Endpoint "withdraw" (AssetClass)
        .\/ Endpoint "buy"      AssetClass
--        .\/ Endpoint "collect"  ()


openTheMarket :: Market -> Contract (Last Value) MarketSchema Text  ()
openTheMarket market = (sell `select` buy ) >> openTheMarket market
  where
    sell :: Contract (Last Value) MarketSchema Text ()
    sell= h $ do
        (asset,cost) <- endpoint @"sell"
        moveToMarket market asset cost

    buy ::  Contract (Last Value) MarketSchema Text ()
    buy = h $ do
        asset <-endpoint @"buy"
        buyFromMarket market asset

    h :: Contract (Last Value) MarketSchema Text () -> Contract (Last Value) MarketSchema Text ()
    h = handleError logError