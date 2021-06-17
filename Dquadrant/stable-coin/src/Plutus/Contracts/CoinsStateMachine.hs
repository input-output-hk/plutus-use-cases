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
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:debug-context #-}

module Plutus.Contracts.CoinsStateMachine
  ( scriptInstance,
    machineClient,
    endpoints,
    BankState (..),
    BankParam (..),
    BankInput (..),
    BankInputAction (..),
    BankStateSchema,
    BankStateError,
    EndpointInput (..),
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
import Ledger.Oracle
  ( Observation (Observation, obsSlot, obsValue),
    SignedMessage,
    verifySignedMessageConstraints,
  )
import Ledger.Typed.Scripts (scriptHash)
import qualified Ledger.Typed.Scripts as Scripts
import Ledger.Typed.Scripts.Validators (forwardingMPS)
import qualified Ledger.Value as Value
import Playground.Contract (ToSchema, adaCurrency, ensureKnownCurrencies, printJson, printSchemas, stage)
import Playground.TH (mkKnownCurrencies, mkSchemaDefinitions)
import Plutus.Contract
import Plutus.Contract.StateMachine (SMContractError, State (..), StateMachine, StateMachineClient (..), StateMachineInstance (..), Void)
import qualified Plutus.Contract.StateMachine as SM
import qualified PlutusTx as PlutusTx
import PlutusTx.Prelude
import PlutusTx.Ratio as Ratio
import qualified Prelude

--
data BankState = BankState
  { baseReserveAmount :: Integer,
    stableCoinAmount :: Integer,
    reserveCoinAmount :: Integer,
    policyScript :: MonetaryPolicyHash
  }
  deriving stock (Generic, Prelude.Eq, Prelude.Show)
  deriving anyclass (ToJSON, FromJSON)

--
data BankParam = BankParam
  { stableCoinTokenName :: TokenName,
    reserveCoinTokenName :: TokenName,
    -- oracleAddress :: PubKey,
    minReserveRatio :: Ratio Integer,
    maxReserveRatio :: Ratio Integer,
    rcDefaultRate :: Ratio Integer
  }
  deriving stock (Generic, Prelude.Eq, Prelude.Show)
  deriving anyclass (ToJSON, FromJSON)

--
data BankInputAction
  = MintStableCoin Integer
  | RedeemStableCoin Integer
  | MintReserveCoin Integer
  | RedeemReserveCoin Integer
  deriving stock (Generic, Prelude.Eq, Prelude.Show)
  deriving anyclass (ToJSON, FromJSON)

type Rate = Ratio Integer

--
data BankInput = BankInput
  { --Signed message from oracle provider  for exchange rate
    rate :: Rate,
    bankInputAction :: BankInputAction
  }
  deriving stock (Generic, Prelude.Eq, Prelude.Show)
  deriving anyclass (ToJSON, FromJSON)

{-# INLINEABLE lovelaces #-}
lovelaces :: Value -> Integer
lovelaces = Ada.getLovelace . Ada.fromValue

{-# INLINEABLE calcLiablities #-}
calcLiablities :: BankState -> Rate -> Ratio Integer
calcLiablities BankState {baseReserveAmount, stableCoinAmount} rate =
  let reserveNeeded = rate * fromInteger stableCoinAmount
   in min (fromInteger baseReserveAmount) reserveNeeded

{-# INLINEABLE calcStableCoinRate #-}
calcStableCoinRate :: BankState -> Rate -> Ratio Integer
calcStableCoinRate bs@BankState {stableCoinAmount} rate
  | stableCoinAmount == 0 = rate
  | otherwise = min rate liableRate
  where
    liablities = calcLiablities bs rate
    liableRate = liablities * Ratio.recip (fromInteger stableCoinAmount)

{-# INLINEABLE calcEquity #-}
calcEquity :: BankState -> Rate -> Ratio Integer
calcEquity bs@BankState {baseReserveAmount} rate =
  let liablities = calcLiablities bs rate
   in fromInteger baseReserveAmount - liablities

{-# INLINEABLE calcReserveCoinRate #-}
calcReserveCoinRate :: BankParam -> BankState -> Rate -> Ratio Integer
calcReserveCoinRate BankParam {rcDefaultRate} bs@BankState {reserveCoinAmount} rate
  | reserveCoinAmount /= 0 = rcRate
  | otherwise = rcDefaultRate
  where
    equity = calcEquity bs rate
    rcRate = equity * Ratio.recip (fromInteger reserveCoinAmount)

--TODO refactor to functions for remove duplicate code
--TODO check for observation slot for oracle so apply date constraints must validate in
--TODO conversion of rc amount and sc amount to base currency
{-# INLINEABLE transition #-}
transition :: BankParam -> State BankState -> BankInput -> Maybe (TxConstraints Void Void, State BankState)
transition bankParam@BankParam {} State {stateData = oldStateData} BankInput {bankInputAction, rate} = do
  --   (Observation {obsValue = rate, obsSlot}, oracleConstraints) <- either (const Nothing) pure (verifySignedMessageConstraints oracleAddress rateObs)
  let rcRate = calcReserveCoinRate bankParam oldStateData rate
      scRate = calcStableCoinRate oldStateData rate
      (newConstraints, newStateData) = case bankInputAction of
        MintReserveCoin rcAmt ->
          let constraints = Constraints.mustForgeCurrency (policyScript oldStateData) (reserveCoinTokenName bankParam) rcAmt
              valueInBaseCurrency = fromInteger rcAmt * rcRate
              newBaseReserve = baseReserveAmount oldStateData + round valueInBaseCurrency
           in ( constraints,
                oldStateData
                  { reserveCoinAmount = reserveCoinAmount oldStateData + rcAmt,
                    baseReserveAmount = newBaseReserve
                  }
              )
        RedeemReserveCoin rcAmt ->
          let constraints = Constraints.mustForgeCurrency (policyScript oldStateData) (reserveCoinTokenName bankParam) (negate rcAmt)
              valueInBaseCurrency = fromInteger rcAmt * rcRate
              newBaseReserve = baseReserveAmount oldStateData - round valueInBaseCurrency
           in ( constraints,
                oldStateData
                  { reserveCoinAmount = reserveCoinAmount oldStateData - rcAmt,
                    baseReserveAmount = newBaseReserve
                  }
              )
        MintStableCoin scAmt ->
          let constraints = Constraints.mustForgeCurrency (policyScript oldStateData) (stableCoinTokenName bankParam) scAmt
              valueInBaseCurrency = fromInteger scAmt * scRate
              newBaseReserve = baseReserveAmount oldStateData + round valueInBaseCurrency -- TODO currently one calculate rate
           in ( constraints,
                oldStateData
                  { stableCoinAmount = stableCoinAmount oldStateData + scAmt,
                    baseReserveAmount = newBaseReserve
                  }
              )
        RedeemStableCoin scAmt ->
          let constraints = Constraints.mustForgeCurrency (policyScript oldStateData) (stableCoinTokenName bankParam) (negate scAmt)
              valueInBaseCurrency = fromInteger scAmt * scRate
              newBaseReserve = baseReserveAmount oldStateData - round valueInBaseCurrency
           in ( constraints,
                oldStateData
                  { stableCoinAmount = stableCoinAmount oldStateData - scAmt,
                    baseReserveAmount = newBaseReserve
                  }
              )

  guard (isNewStateValid bankParam newStateData rate)

  let state =
        State
          { stateData = newStateData,
            stateValue = Ada.lovelaceValueOf (baseReserveAmount newStateData)
          }
  --   dateConstraints = Constraints.mustValidateIn $ Interval.from obsSlot

  pure
    ( newConstraints,
      -- <> oracleConstraints,
      -- <>   dateConstraints
      state
    )

{-# INLINEABLE isNewStateValid #-}
isNewStateValid :: BankParam -> BankState -> Rate -> Bool
isNewStateValid bankParam bankState rate = isRight (checkForValidState bankParam bankState rate)

{-# INLINEABLE checkForValidState #-}
checkForValidState :: BankParam -> BankState -> Rate -> Either ErrorState ()
checkForValidState bankParam bankState@BankState {baseReserveAmount, stableCoinAmount, reserveCoinAmount} rate = do
  unless (baseReserveAmount >= 0) (Left NegativeReserves)
  unless (reserveCoinAmount >= 0) (Left NegativeReserveCoins)
  unless (stableCoinAmount >= 0) (Left NegativeStablecoins)
  unless (calcLiablities bankState rate >= zero) (Left NegativeLiabilities)
  unless (calcEquity bankState rate >= zero) (Left NegativeEquity)

  let actualReserves = fromInteger baseReserveAmount
      allowedReserves = (,) <$> minReserve bankParam bankState rate <*> maxReserve bankParam bankState rate

  case allowedReserves of
    Just (minReserves, maxReserves) -> do
      unless (actualReserves >= minReserves) (Left $ MinReserves minReserves actualReserves)
      unless (actualReserves <= maxReserves) (Left $ MaxReserves maxReserves actualReserves)
    Nothing -> pure ()

{-# INLINEABLE minReserve #-}
minReserve :: BankParam -> BankState -> Rate -> Maybe (Ratio Integer)
minReserve BankParam {minReserveRatio} BankState {stableCoinAmount} rate
  | stableCoinAmount == zero = Nothing
  | otherwise =
    let currentScValue = rate * fromInteger stableCoinAmount
     in Just $ minReserveRatio * currentScValue

{-# INLINEABLE maxReserve #-}
maxReserve :: BankParam -> BankState -> Rate -> Maybe (Ratio Integer)
maxReserve BankParam {maxReserveRatio} BankState {stableCoinAmount} rate
  | stableCoinAmount == zero = Nothing
  | otherwise =
    let currentScValue = rate * fromInteger stableCoinAmount
     in Just $ maxReserveRatio * currentScValue

data ErrorState
  = NegativeReserveCoins
  | NegativeReserves
  | NegativeStablecoins
  | MinReserves {allowed :: Ratio Integer, actual :: Ratio Integer}
  | MaxReserves {allowed :: Ratio Integer, actual :: Ratio Integer}
  | NegativeLiabilities
  | NegativeEquity
  deriving (Show)

bankMachine :: BankParam -> StateMachine BankState BankInput
bankMachine bankParam = SM.mkStateMachine Nothing (transition bankParam) isFinal
  where
    isFinal _ = False

scriptInstance :: BankParam -> Scripts.ScriptInstance (StateMachine BankState BankInput)
scriptInstance bankParam =
  let val = $$(PlutusTx.compile [||validator||]) `PlutusTx.applyCode` PlutusTx.liftCode bankParam
      validator param = SM.mkValidator (bankMachine param)
      wrap = Scripts.wrapValidator @BankState @BankInput
   in Scripts.validator @(StateMachine BankState BankInput) val $$(PlutusTx.compile [||wrap||])

machineClient ::
  Scripts.ScriptInstance (StateMachine BankState BankInput) ->
  BankParam ->
  StateMachineClient BankState BankInput
machineClient scriptInst bankParam =
  let machine = bankMachine bankParam
   in SM.mkStateMachineClient (StateMachineInstance machine scriptInst)

initialState :: StateMachineClient BankState BankInput -> BankState
initialState StateMachineClient {scInstance = StateMachineInstance {validatorInstance}} =
  BankState
    { baseReserveAmount = 0,
      stableCoinAmount = 0,
      reserveCoinAmount = 0,
      policyScript = monetaryPolicyHash $ forwardingMPS $ scriptHash validatorInstance
    }

data BankStateError
  = StartError ContractError
  | StateMachineError SMContractError
  | RunStepError ContractError
  deriving stock (Prelude.Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

stableCoinName :: TokenName
stableCoinName = "StableToken"

reserveCoinName :: TokenName
reserveCoinName = "ReserveToken"

bp :: BankParam
bp =
  BankParam
    { stableCoinTokenName = stableCoinName,
      reserveCoinTokenName = reserveCoinName,
      minReserveRatio = zero,
      maxReserveRatio = 4 % 1,
      rcDefaultRate = 1 % 1
    }

client :: StateMachineClient BankState BankInput
client = machineClient (scriptInstance bp) bp

mapError' :: Contract w s SMContractError a -> Contract w s Text a
mapError' = mapError $ pack . show

start :: HasBlockchainActions s => Integer -> Contract w s Text ()
start _ = do
  void $ mapError' $ SM.runInitialise client (initialState client) mempty

--TODO check for validation in offchain
mintStableCoin :: HasBlockchainActions s => EndpointInput -> Contract w s Text ()
mintStableCoin endpointInput@EndpointInput {tokenAmount} = do
  let input =
        BankInput
          { rate = getRatioFromInput endpointInput,
            bankInputAction = MintStableCoin tokenAmount
          }
  void $ mapError' $ SM.runStep client input

redeemStableCoin :: HasBlockchainActions s => EndpointInput -> Contract w s Text ()
redeemStableCoin endpointInput@EndpointInput {tokenAmount} = do
  let input =
        BankInput
          { rate = getRatioFromInput endpointInput,
            bankInputAction = RedeemStableCoin tokenAmount
          }
  void $ mapError' $ SM.runStep client input

mintReserveCoin :: HasBlockchainActions s => EndpointInput -> Contract w s Text ()
mintReserveCoin endpointInput@EndpointInput {tokenAmount} = do
  let input =
        BankInput
          { rate = getRatioFromInput endpointInput,
            bankInputAction = MintReserveCoin tokenAmount
          }
  void $ mapError' $ SM.runStep client input

redeemReserveCoin :: HasBlockchainActions s => EndpointInput -> Contract w s Text ()
redeemReserveCoin endpointInput@EndpointInput {tokenAmount} = do
  let input =
        BankInput
          { rate = getRatioFromInput endpointInput,
            bankInputAction = RedeemReserveCoin tokenAmount
          }
  void $ mapError' $ SM.runStep client input

getRatioFromInput :: EndpointInput -> Ratio Integer
getRatioFromInput EndpointInput {rateNume, rateDeno} = rateNume % rateDeno

data EndpointInput = EndpointInput
  { --Signed message from oracle provider  for exchange rate
    rateNume :: Integer,
    rateDeno :: Integer,
    tokenAmount :: Integer
  }
  deriving stock (Generic, Prelude.Eq, Prelude.Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type BankStateSchema =
  BlockchainActions
    .\/ Endpoint "start" Integer
    .\/ Endpoint "mintStableCoin" EndpointInput
    .\/ Endpoint "redeemStableCoin" EndpointInput
    .\/ Endpoint "mintReserveCoin" EndpointInput
    .\/ Endpoint "redeemReserveCoin" EndpointInput

mkSchemaDefinitions ''BankStateSchema

endpoints :: Contract () BankStateSchema Text ()
endpoints =
  ( start'
      `select` mintStableCoin'
      `select` redeemStableCoin'
      `select` mintReserveCoin'
      `select` redeemReserveCoin'
  )
    >> endpoints
  where
    --TODO handle state for multiple start endpoint call
    start' = endpoint @"start" >>= start
    mintStableCoin' = endpoint @"mintStableCoin" >>= mintStableCoin
    redeemStableCoin' = endpoint @"redeemStableCoin" >>= redeemStableCoin
    mintReserveCoin' = endpoint @"mintReserveCoin" >>= mintReserveCoin
    redeemReserveCoin' = endpoint @"redeemReserveCoin" >>= redeemReserveCoin

PlutusTx.makeLift ''BankState
PlutusTx.makeLift ''BankParam
PlutusTx.unstableMakeIsData ''BankState
PlutusTx.unstableMakeIsData ''BankParam
PlutusTx.unstableMakeIsData ''BankInput
PlutusTx.unstableMakeIsData ''BankInputAction