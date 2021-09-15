{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:debug-context #-}

module Plutus.Contracts.Coins.CoinsStateMachine
  ( scriptInstance,
    machineClient,
    calcReserveCoinRate,
    calcStableCoinRate
  )
where

import Control.Monad (forever, guard, void)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text, pack)
import GHC.Generics (Generic)
import Ledger hiding (to)
import qualified Ledger as Interval
import qualified Ledger.Ada as Ada
import Ledger.Constraints (TxConstraints)
import qualified Ledger.Constraints as Constraints
import           Ledger.Scripts               (MintingPolicyHash)
import qualified Ledger.Typed.Scripts as Scripts
import qualified Ledger.Value as Value
import Playground.Contract (ToSchema, adaCurrency, ensureKnownCurrencies, printJson, printSchemas, stage)
import Playground.TH (mkKnownCurrencies, mkSchemaDefinitions)
import Plutus.Contract
import Plutus.Contract.StateMachine (SMContractError, State (..), StateMachine(..), StateMachineClient (..), Void)
import qualified Plutus.Contract.StateMachine as SM
import qualified PlutusTx as PlutusTx
import PlutusTx.Prelude
import PlutusTx.Ratio as Ratio
import qualified Prelude
import Prelude (show)
import Plutus.Contracts.Oracle.Core
import Plutus.Contracts.Coins.Types

--Stable coin mainly based on AGE usd protocol.
--Alogrithimic rates of different coins based on actual amount of supply of tokens and base reserve amount.

--Helper function to get validator address from current instance of state machine
address :: BankParam -> Address
address bp = Scripts.validatorAddress $ scriptInstance bp

-- Get amount of lovelaces contained in a givenn value
{-# INLINEABLE lovelaces #-}
lovelaces :: Value -> Integer
lovelaces = Ada.getLovelace . Ada.fromValue


-- Transition function for state machine to validate and get new state and constraints for state machine
{-# INLINEABLE transition #-}
transition :: BankParam -> State CoinsMachineState -> BankInput -> Maybe (TxConstraints Void Void, State CoinsMachineState)
transition bankParam@BankParam {oracleParam,oracleAddr} oldState@State {stateData = oldStateData} bankInput = 

  case bankInput of 
    --Update contract status to running or paused
    UpdateContractStatus shouldPause ->
      let constraints = Constraints.mustBeSignedBy (bankContractOwner bankParam)
      in pure (
        constraints,
        oldState {
          stateData = oldStateData
            {
              contractStatus =  if shouldPause then Paused else Running
            }
        }
      )
    --Upate bank fee case can be handled directly without hadling cases for oracle requirement like minting
    UpdateBankFee percentNumerator percentDenominator ->
      let constraints = Constraints.mustBeSignedBy (bankContractOwner bankParam)
      in pure (
        constraints,
        oldState {
          stateData = oldStateData
            {
              bankFee = percentNumerator % percentDenominator
            }
        }
      )

    BankInput bankInputAction oracleOutput -> case contractStatus oldStateData of
      Paused -> Nothing
      Running -> do
        let oValHash = toValidatorHash oracleAddr
        case oValHash of
          Nothing -> Nothing
          Just valHash-> do
            let (oref, oTxOut, rate) = oracleOutput
                oNftValue = txOutValue oTxOut 
                    <> Ada.lovelaceValueOf (oFee oracleParam)

                oracleConstraints = Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData Use) <>
                                Constraints.mustPayToOtherScript
                                  valHash
                                  (Datum $ PlutusTx.toBuiltinData rate)
                                  oNftValue
                rcRate = calcReserveCoinRate bankParam oldStateData rate
                scRate = calcStableCoinRate oldStateData rate
                (newConstraints, newStateData) = stateWithConstraints bankParam oldStateData bankInputAction scRate rcRate
                -- eitherValidState = shouldTransitToNextState bankParam newStateData rate
            --TODO Upgrade plutus dependecnies to support string in either part solve error for plc plugin
            -- guard (isRight eitherValidState)
            
            let state =
                  State
                    { stateData = newStateData,
                      stateValue = Ada.lovelaceValueOf (baseReserveAmount newStateData)
                    }

            pure
              ( newConstraints
                <> oracleConstraints
                ,
                state
              )

-- Calculate fees by getting current fee ratio convert to percent by dividing by 100
getFeesAmount :: CoinsMachineState -> Integer -> Integer
getFeesAmount bankState amount = round ( (fromInteger amount) * (bankFee bankState) * (1%100) )

-- Get state and contratins based on the input action called by the user
--TODO Refactor common function inside minting and redeeming cases
{-# INLINEABLE stateWithConstraints #-}
stateWithConstraints :: BankParam -> CoinsMachineState -> BankInputAction -> Integer -> Integer-> (TxConstraints Void Void, CoinsMachineState)
stateWithConstraints bankParam oldStateData bankInputAction scRate rcRate= case bankInputAction of
        MintReserveCoin rcAmt ->
          let constraints = Constraints.mustMintCurrency (policyScript oldStateData) (reserveCoinTokenName bankParam) rcAmt
              valueInBaseCurrency = rcAmt * rcRate
              feesValue = getFeesAmount oldStateData valueInBaseCurrency 
              newBaseReserve = baseReserveAmount oldStateData + valueInBaseCurrency + feesValue
           in ( constraints,
                oldStateData
                  { reserveCoinAmount = reserveCoinAmount oldStateData + rcAmt,
                    baseReserveAmount = newBaseReserve
                  }              
              )
        RedeemReserveCoin rcAmt ->
          let constraints = Constraints.mustMintCurrency (policyScript oldStateData) (reserveCoinTokenName bankParam) (negate rcAmt)
              valueInBaseCurrency = rcAmt * rcRate
              feesValue = getFeesAmount oldStateData valueInBaseCurrency 
              newBaseReserve = baseReserveAmount oldStateData - valueInBaseCurrency + feesValue
           in ( constraints,
                oldStateData
                  { reserveCoinAmount = reserveCoinAmount oldStateData - rcAmt,
                    baseReserveAmount = newBaseReserve
                  }              
              )
        MintStableCoin scAmt ->
          let constraints = Constraints.mustMintCurrency (policyScript oldStateData) (stableCoinTokenName bankParam) scAmt
              valueInBaseCurrency = scAmt * scRate
              feesValue = getFeesAmount oldStateData valueInBaseCurrency 
              newBaseReserve = baseReserveAmount oldStateData + valueInBaseCurrency + feesValue
           in ( constraints,
                oldStateData
                  { stableCoinAmount = stableCoinAmount oldStateData + scAmt,
                    baseReserveAmount = newBaseReserve
                  }
              )
        RedeemStableCoin scAmt ->
          let constraints = Constraints.mustMintCurrency (policyScript oldStateData) (stableCoinTokenName bankParam) (negate scAmt)
              valueInBaseCurrency = scAmt * scRate
              feesValue = getFeesAmount oldStateData valueInBaseCurrency 
              newBaseReserve = baseReserveAmount oldStateData - valueInBaseCurrency + feesValue
           in ( constraints,
                oldStateData
                  { stableCoinAmount = stableCoinAmount oldStateData - scAmt,
                    baseReserveAmount = newBaseReserve
                  }
              )


-- Get current equity i.e base reserve amount that is left after deducting liabilites
{-# INLINEABLE calcEquity #-}
calcEquity :: CoinsMachineState -> Integer -> Integer
calcEquity bs@CoinsMachineState {baseReserveAmount} rate =
  let liablities = calcLiablities bs rate
   in baseReserveAmount - liablities

--Calculate current reserve coin rate by dividing equity reserve amount to each resrve coin holders
{-# INLINEABLE calcReserveCoinRate #-}
calcReserveCoinRate :: BankParam -> CoinsMachineState -> Integer -> Integer
calcReserveCoinRate BankParam {rcDefaultRate} bs@CoinsMachineState {reserveCoinAmount} rate
  | reserveCoinAmount /= 0 = rcRate
  | otherwise = rcDefaultRate
  where
    equity = calcEquity bs rate
    rcRate = equity `divide` reserveCoinAmount

--Transistion validation to check if new state is valid and proceed accordingly
{-# INLINEABLE shouldTransitToNextState #-}
shouldTransitToNextState :: BankParam -> CoinsMachineState -> Integer -> Either Prelude.String ()
shouldTransitToNextState bankParam bankState@CoinsMachineState {baseReserveAmount, stableCoinAmount, reserveCoinAmount} rate = do
  unless (baseReserveAmount >= 0) (throwError "Invalid state : Base reserve amount is in negative.")
  unless (reserveCoinAmount >= 0) (throwError "Invalid state : Reserve coins amount is in negative.")
  unless (stableCoinAmount >= 0) (throwError "Invalid state : Stable coins amount is in negative.")
  unless (calcLiablities bankState rate >= 0) (throwError "Invalid state : Liabilities calculation is in negative.")
  unless (calcEquity bankState rate >= 0) (throwError "Invalid state : Equity calculation is in negative.")
  
  let currentReserveAmount = fromInteger baseReserveAmount
      minReserveRequired = calcMinReserveRequired bankParam bankState rate

  case minReserveRequired of 
    Just minR -> do
        unless (currentReserveAmount >= minR) (throwError "Invalid state : Base reserve amount is less than minimum required amount.")
    Nothing -> pure ()

  let maxReserveRequired = calcMaxReserveRequired bankParam bankState rate
    
  case maxReserveRequired of
    Just maxR -> do
        unless (currentReserveAmount <= maxR) (throwError "InNBLvalid state : Base reserve amount is more than maximum required amount.")
    Nothing -> pure ()

--Calculate the min reserve required for contract from current stable coin value with min reserve ratio defined in contract
{-# INLINEABLE calcMinReserveRequired #-}
calcMinReserveRequired :: BankParam -> CoinsMachineState -> Integer -> Maybe (Ratio Integer)
calcMinReserveRequired BankParam {minReserveRatio} CoinsMachineState {stableCoinAmount} rate
  | stableCoinAmount == 0 = Nothing
  | otherwise =
    let currentScValue = rate * stableCoinAmount
     in Just $ minReserveRatio * (fromInteger currentScValue)

--Calculate liabilites to stable coin holder from minimum of reserve needed for current exchange rate or base reserve available
{-# INLINEABLE calcLiablities #-}
calcLiablities :: CoinsMachineState -> Integer -> Integer
calcLiablities CoinsMachineState {baseReserveAmount, stableCoinAmount} rate =
  let reserveNeeded = rate * stableCoinAmount
   in min baseReserveAmount reserveNeeded

--Calculate stable coin rate for 1 stable coin from current rate of liable rate whichever is minimum
-- Liable rate is calculated from current liabaitles to each stable coin holders
{-# INLINEABLE calcStableCoinRate #-}
calcStableCoinRate :: CoinsMachineState -> Integer -> Integer
calcStableCoinRate bs@CoinsMachineState {stableCoinAmount} rate
  | stableCoinAmount == 0 = rate
  | otherwise = min rate liableRate
  where
    liablities = calcLiablities bs rate
    liableRate = liablities `divide` stableCoinAmount

--Calculate the max reserve required for contract from current stable coin value with max reserve ratio defined in contract
{-# INLINEABLE calcMaxReserveRequired #-}
calcMaxReserveRequired :: BankParam -> CoinsMachineState -> Integer -> Maybe (Ratio Integer)
calcMaxReserveRequired BankParam {maxReserveRatio} CoinsMachineState {stableCoinAmount} rate
  | stableCoinAmount == 0 = Nothing
  | otherwise =
    let currentScValue = rate * stableCoinAmount
     in Just $ maxReserveRatio * (fromInteger currentScValue)

--Construct a state machine from transition and check functions
{-# INLINEABLE bankMachine #-}
bankMachine :: BankParam -> StateMachine CoinsMachineState BankInput
bankMachine bankParam = SM.StateMachine{
                          smTransition = (transition bankParam),
                          smFinal = isFinal,
                          smCheck =  checkContext bankParam,
                          smThreadToken = Nothing
                        }

--Validate current context of transition for the correct oracle value is used as input and its exchange rate matches with our input
{-# INLINEABLE checkContext #-}
checkContext :: BankParam -> CoinsMachineState -> BankInput -> ScriptContext -> Bool
checkContext bankParam@BankParam{oracleAddr} oldBankState bankInput ctx = 
  case bankInput of 
    UpdateBankFee _ _-> True -- Skip check context for update bank fee where oracle is not used already checked in transistion state
    UpdateContractStatus _ -> True -- Skip check context for update contract status where oracle is not used already checked in transistion state
    BankInput _ oracleOutput -> 
      traceIfFalse "Invalid oracle use" isValidOracleUsed

      where
        (_, _, rate) = oracleOutput

        info :: TxInfo
        info = scriptContextTxInfo ctx

        oracleInput :: TxOut
        oracleInput =
          let
            inputs = [ o
                  | i <- txInfoInputs info
                  , let o = txInInfoResolved i
                  , txOutAddress o == oracleAddr
                  ]
          in
            case inputs of
                [o] -> o
                _   -> traceError "expected exactly one oracle input during transistion check"

        oracleValue' = case oracleValue oracleInput (`findDatum` info) of
          Nothing -> traceError "oracle value not found"
          Just x  -> x

        -- Is rate provided in input is same as orcale value derived from oracle input of transaction
        isValidOracleUsed :: Bool
        isValidOracleUsed =  oracleValue' == rate


-- Terminate the state machine currently the state machine is to be running forever
{-# INLINEABLE isFinal #-}
isFinal :: CoinsMachineState -> Bool
isFinal _ = False

--Construct a script instance of validator
scriptInstance :: BankParam -> Scripts.TypedValidator (StateMachine CoinsMachineState BankInput)
scriptInstance bankParam =
  let val = $$(PlutusTx.compile [||validator||]) `PlutusTx.applyCode` PlutusTx.liftCode bankParam
      validator param = SM.mkValidator (bankMachine param)
      wrap = Scripts.wrapValidator @CoinsMachineState @BankInput
   in Scripts.mkTypedValidator @(StateMachine CoinsMachineState BankInput) val $$(PlutusTx.compile [||wrap||])

--Construct state machine client of stable coin state machine
machineClient ::
  Scripts.TypedValidator (StateMachine CoinsMachineState BankInput) ->
  BankParam ->
  StateMachineClient CoinsMachineState BankInput
machineClient scriptInst bankParam =
  let machine = bankMachine bankParam
   in SM.mkStateMachineClient (SM.StateMachineInstance machine scriptInst)
