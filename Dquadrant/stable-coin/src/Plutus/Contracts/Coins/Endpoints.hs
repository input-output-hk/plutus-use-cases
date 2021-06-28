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

-- {-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE DeriveAnyClass #-}
-- {-# LANGUAGE DeriveGeneric #-}
-- {-# LANGUAGE DerivingStrategies #-}
-- {-# LANGUAGE DerivingVia #-}
-- {-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE LambdaCase #-}
-- {-# LANGUAGE MonoLocalBinds #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE TemplateHaskell #-}
-- {-# LANGUAGE TypeApplications #-}
-- {-# LANGUAGE TypeFamilies     #-}
-- {-# LANGUAGE TypeOperators #-}
-- {-# LANGUAGE ViewPatterns #-}
-- {-# LANGUAGE NoImplicitPrelude #-}
-- {-# OPTIONS_GHC -Wno-name-shadowing #-}
-- {-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
-- {-# OPTIONS_GHC -fno-strictness #-}
-- {-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:debug-context #-}

module Plutus.Contracts.Coins.Endpoints
  ( 
    BankStateSchema,
    coinsContract,
  )
where

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

import Plutus.Contracts.Coins.Types
import Plutus.Contracts.Oracle.Core
import qualified Plutus.Contract.StateMachine     as SM
import           Plutus.Contracts.Coins.CoinsStateMachine
import qualified Plutus.Contracts.Utils.StateMachine as SmUtil

import           Plutus.Contract.StateMachine     (SMContractError, StateMachineClient (..))
import           Playground.TH                     (mkSchemaDefinitions)
import           Ledger.Typed.Tx              (TypedScriptTxOut (..))
import qualified Data.Aeson.Types as Types
import Data.Aeson (toJSON)
import           Ledger.AddressMap                 (UtxoMap)


forwardMPS :: StateMachineClient CoinsMachineState BankInput -> MonetaryPolicyHash
forwardMPS StateMachineClient {scInstance} = Scripts.forwardingMonetaryPolicyHash $ SM.typedValidator scInstance

initialState :: StateMachineClient CoinsMachineState BankInput -> CoinsMachineState
initialState smClient =
  CoinsMachineState
    { baseReserveAmount = 0,
      stableCoinAmount = 0,
      reserveCoinAmount = 0,
      policyScript = forwardMPS smClient
    }

mapError' :: Contract w s SMContractError a -> Contract w s Text a
mapError' = mapError $ pack . Prelude.show

start :: HasBlockchainActions s => BankParam -> Integer -> Contract w s Text ()
start bankParam _ = do
  let client = machineClient (scriptInstance bankParam) bankParam
  void $ mapError' $ SM.runInitialise client (initialState client) mempty

--TODO make transistion to use whole oracle output instead of only values obtained from it
smRunStep :: HasBlockchainActions s => BankParam -> BankInputAction -> Contract w s Text ()
smRunStep bankParam@BankParam{oracleParam} bankInputAction = do
  let client = machineClient (scriptInstance bankParam) bankParam

  oracle <- findOracle oracleParam
  
  case oracle of
    Nothing -> logInfo @Prelude.String "Oracle not found"
    Just (oref, o, x) -> do      
      let lookups = Constraints.unspentOutputs (Map.singleton oref o)
                    <> Constraints.otherScript (oracleValidator oracleParam)               
                                  
          -- tx = Constraints.mustSpendScriptOutput oref  (Redeemer $ PlutusTx.toData Use)
          --                 <>  Constraints.mustPayToOtherScript
          --                           (validatorHash $ oracleValidator oracleParam)
          --                           (Datum $ PlutusTx.toData x)
          --                           oNftValue      
          input =
            BankInput
              { bankInputAction = bankInputAction
              , oracleOutput = (oref, txOutTxOut o, x)
              }

      void $ mapError' $ SmUtil.runStepWith client input lookups
      void $ logInfo @Prelude.String $ "Endpoint call completed " ++ Prelude.show bankInputAction

--TODO check for validation in offchain
mintStableCoin :: HasBlockchainActions s => BankParam -> EndpointInput -> Contract w s Text ()
mintStableCoin bankParam@BankParam{oracleParam} EndpointInput{tokenAmount} = smRunStep bankParam $ MintStableCoin tokenAmount

redeemStableCoin :: HasBlockchainActions s => BankParam -> EndpointInput -> Contract w s Text ()
redeemStableCoin bankParam EndpointInput{tokenAmount} = smRunStep bankParam $ RedeemStableCoin tokenAmount

mintReserveCoin :: HasBlockchainActions s => BankParam -> EndpointInput -> Contract w s Text ()
mintReserveCoin bankParam EndpointInput{tokenAmount} = smRunStep bankParam $ MintReserveCoin tokenAmount

redeemReserveCoin :: HasBlockchainActions s => BankParam -> EndpointInput -> Contract w s Text ()
redeemReserveCoin bankParam EndpointInput{tokenAmount} = smRunStep bankParam $ RedeemReserveCoin tokenAmount

type BankStateSchema =
  BlockchainActions
    .\/ Endpoint "start" Integer
    .\/ Endpoint "mintStableCoin" EndpointInput
    .\/ Endpoint "redeemStableCoin" EndpointInput
    .\/ Endpoint "mintReserveCoin" EndpointInput
    .\/ Endpoint "redeemReserveCoin" EndpointInput

    .\/ Endpoint "funds" Prelude.String
    .\/ Endpoint "currentState" Prelude.String
    .\/ Endpoint "pegRate" Prelude.String
    .\/ Endpoint "stableRate" Prelude.String
    .\/ Endpoint "reserveRate" Prelude.String
    .\/ Endpoint "currentRates" Prelude.String

mkSchemaDefinitions ''BankStateSchema

--TODO writer value [Types.Value]
coinsContract :: BankParam -> Contract [Types.Value] BankStateSchema Text ()
coinsContract bankParam =
  ( 
    start'
      `select`
       mintStableCoin'
      `select` redeemStableCoin'
      `select` mintReserveCoin'
      `select` redeemReserveCoin'

      `select` ownFunds'
      `select` currentState'
      `select` pegToLovRate'
      `select` stableToLovRate'
      `select` reserveToLovRate'
      `select` currentRates'
  )
    >> coinsContract bankParam
  where
    --TODO handle state for multiple start endpoint call
    start' = endpoint @"start" >>= start bankParam
    mintStableCoin' = endpoint @"mintStableCoin" >>= mintStableCoin bankParam
    redeemStableCoin' = endpoint @"redeemStableCoin" >>= redeemStableCoin bankParam
    mintReserveCoin' = endpoint @"mintReserveCoin" >>= mintReserveCoin bankParam
    redeemReserveCoin' = endpoint @"redeemReserveCoin" >>= redeemReserveCoin bankParam
    
    ownFunds' = endpoint @"funds" >> ownFunds bankParam
    currentState' = endpoint @"currentState" >> currentCoinsMachineState bankParam
    pegToLovRate' = endpoint @"pegRate" >> currentPegToLovelaceRate bankParam
    stableToLovRate' = endpoint @"stableRate" >> currentStableToLovelaceRate bankParam
    reserveToLovRate' = endpoint @"reserveRate" >> currentReserveToLovelaceRate bankParam
    currentRates' = endpoint @"currentRates" >> currentRates bankParam


ownFunds:: HasBlockchainActions s => BankParam -> Contract [Types.Value ] s Text  ()
ownFunds _ = do
    pk    <- ownPubKey
    utxos <- utxoAt $ pubKeyAddress pk
    let v = mconcat $ Map.elems $ txOutValue . txOutTxOut Prelude.<$> utxos
    logInfo @Prelude.String $ "own funds: " ++ Prelude.show (flattenValue v)
    tell [ toJSON v]

currentState :: ( SM.AsSMContractError e
    , HasUtxoAt schema)
    => BankParam -> Contract w schema e (Maybe (SM.OnChainState CoinsMachineState BankInput, UtxoMap))
currentState bankParam = do
  let client = machineClient (scriptInstance bankParam) bankParam
  SM.getOnChainState client

currentCoinsMachineState:: HasBlockchainActions s => BankParam -> Contract [Types.Value ] s Text  ()
currentCoinsMachineState bankParam = do
  currentStateVal <- mapError' $ currentState bankParam
  case currentStateVal of
    Just ((TypedScriptTxOut{tyTxOutData=state},_),_) -> do
        logInfo @Prelude.String $ "Current state: " ++ Prelude.show state
        let stateResponse = StateResponse{
          currentCoinsState = state
        }
        tell [toJSON stateResponse]
    Nothing -> logWarn @Prelude.String $ "Current state is not present yet."

currentPegToLovelaceRate :: HasBlockchainActions s => BankParam -> Contract [Types.Value ] s Text  ()
currentPegToLovelaceRate bankParam = do
  oracle <- findOracle $ oracleParam bankParam
  case oracle of
    Nothing -> logWarn @Prelude.String "Oracle not found"
    Just (oref, o, rate) -> do
      logInfo @Prelude.String $ "Current state: " ++ Prelude.show rate
      tell [toJSON rate]

--TODO Merge duplicated code on getting stable and reserve rate functions
currentStableToLovelaceRate :: HasBlockchainActions s => BankParam -> Contract [Types.Value ] s Text  ()
currentStableToLovelaceRate bankParam = do
  oracle <- findOracle $ oracleParam bankParam
  case oracle of
    Nothing -> logWarn @Prelude.String "Oracle not found"
    Just (oref, o, rate) -> do
      currentStateVal <- mapError' $ currentState bankParam
      case currentStateVal of
          Just ((TypedScriptTxOut{tyTxOutData=state},_),_) -> do
              logInfo @Prelude.String $ "Current state: " ++ Prelude.show state
              let scRate = calcStableCoinRate state rate
              tell [toJSON scRate]
          Nothing -> logWarn @Prelude.String $ "Current state is not present yet."

currentReserveToLovelaceRate :: HasBlockchainActions s => BankParam -> Contract [Types.Value ] s Text  ()
currentReserveToLovelaceRate bankParam = do
  oracle <- findOracle $ oracleParam bankParam
  case oracle of
    Nothing -> logWarn @Prelude.String "Oracle not found"
    Just (oref, o, rate) -> do
      currentStateVal <- mapError' $ currentState bankParam
      case currentStateVal of
          Just ((TypedScriptTxOut{tyTxOutData=state},_),_) -> do
              logInfo @Prelude.String $ "Current state: " ++ Prelude.show state
              let rcRate = calcReserveCoinRate bankParam state rate
              tell [toJSON rcRate]
          Nothing -> logWarn @Prelude.String $ "Current state is not present yet."

currentRates :: HasBlockchainActions s => BankParam -> Contract [Types.Value ] s Text  ()
currentRates bankParam = do
  oracle <- findOracle $ oracleParam bankParam
  case oracle of
    Nothing -> logWarn @Prelude.String "Oracle not found"
    Just (oref, o, rate) -> do
      currentStateVal <- mapError' $ currentState bankParam
      case currentStateVal of
          Just ((TypedScriptTxOut{tyTxOutData=state},_),_) -> do
              logInfo @Prelude.String $ "Current state: " ++ Prelude.show state
              let rcRate = calcReserveCoinRate bankParam state rate
                  scRate = calcStableCoinRate state rate

                  rates = Rates 
                                  {
                                        pegRate = rate,
                                        scRate = scRate,
                                        rcRate = rcRate
                                  }
                  ratesResponse = RatesResponse{
                    currentCoinsRates = rates
                  }
              tell [toJSON ratesResponse]
          Nothing -> logWarn @Prelude.String $ "Current state is not present yet."
