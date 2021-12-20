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


module Plutus.Contracts.Coins.Types
  ( 
    CoinsMachineState (..),
    ContractStatus (..),
    BankParam (..),
    BankInput (..),
    BankInputAction (..),
    EndpointInput (..),
    BankFeeInput (..),
    ContractStatusInput (..),
    RatesResponse (..),
    Rates (..),
    StateResponse (..),
  )
where

import           Data.Aeson                             (FromJSON, ToJSON)
import           GHC.Generics                            (Generic)
import qualified Prelude
import           PlutusTx.Ratio as Ratio
import           Ledger.Scripts               (MintingPolicyHash)
import           Ledger.Value                 (TokenName (TokenName))
import           Plutus.Contracts.Oracle.Core
import qualified PlutusTx                      as PlutusTx
import           PlutusTx.Prelude
import           Ledger                        hiding (to)
import           Playground.Contract           (ToSchema)


data ContractStatus = Paused | Running
  deriving stock (Generic,Prelude.Show)
  deriving anyclass (ToJSON, FromJSON)


--State hold by the statemachine
data CoinsMachineState = CoinsMachineState
  { baseReserveAmount :: Integer, -- Current amount of ada reserves held in contract
    stableCoinAmount :: Integer, -- Current amount of stable coins in circulation
    reserveCoinAmount :: Integer, -- Current amount of reserve coins in circulation
    policyScript :: MintingPolicyHash, -- Policy script used for minting of coins
    bankFee :: Ratio Integer, -- Fees charged by contract to contirbute some portion of forged amount to kept in reserve,
                              -- Used as state so that it can be changed by smart contract owner or starter Not hardcoded into contract param
    contractStatus :: ContractStatus
  }
  deriving stock (Generic,Prelude.Show)
  deriving anyclass (ToJSON, FromJSON)

--Parameter to parameterized stable coin contract statemachine
data BankParam = BankParam
  { stableCoinTokenName :: TokenName, -- Token name used for stable coin token
    reserveCoinTokenName :: TokenName, -- Token name used for reserve coin token
    minReserveRatio :: Ratio Integer, -- Minimum reserve ratio that must be kept in contract no tokens forging is allowded below the minimum amount 
    maxReserveRatio :: Ratio Integer, -- Maximum reserve ratio that must be kept within contract no tokens forging is allowded above maximum amount
    rcDefaultRate :: Integer, -- Default rate of reserve token if there are no reserve coins minted yet
    oracleParam :: Oracle,    -- Oracle used to getting exchange rate
    oracleAddr :: Address, -- Address of the oracle used to get oracle value to verify its integrity that value is obtained from this oracle address
    bankCurrencyAsset :: AssetClass, -- Underlying base currency which is locked by bank in which tokens exchange happens
    bankContractOwner :: PubKeyHash  -- Owner of the bank contract who can update certain contract state
  }
  deriving stock (Generic, Prelude.Show)
  deriving anyclass (ToJSON, FromJSON)

-- Actions that can be performed in stable coin contract
data BankInputAction
  = MintStableCoin Integer
  | RedeemStableCoin Integer
  | MintReserveCoin Integer
  | RedeemReserveCoin Integer
  deriving stock (Generic, Prelude.Show)
  deriving anyclass (ToJSON, FromJSON)

type OracleOutput = (TxOutRef, TxOut, Integer) 

-- Redeemer input for updating the state of the contract
data BankInput = BankInput BankInputAction OracleOutput 
                | UpdateBankFee Integer Integer
                | UpdateContractStatus Bool

  deriving stock (Generic, Prelude.Show)
  deriving anyclass (ToJSON, FromJSON)

--Data used from the endpoint to be used as input of contract definitions
data EndpointInput = EndpointInput
  { 
    tokenAmount :: Integer -- Tokens amount to be forged
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

--Data used from the endpoint to be used as input of contract definitions
--To support float percentage 
data BankFeeInput = BankFeeInput
  { 
    percentNumerator :: Integer, -- Numerator value of percent in integer eg: 1 Percent
    percentDenominator :: Integer -- Numerator value of percent in integer eg: 1 Percent
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

--Data used from the endpoint to be used as input of contract definitions
data ContractStatusInput = ContractStatusInput
  { 
    shouldPause :: Bool -- Numerator value of percent in integer eg: 1 Percent
  }
  deriving stock (Generic,Prelude.Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)


--Data used for getting current exchange rates of peg, stable coin and reserve coin
data Rates = Rates
  { 
    pegRate :: Integer, -- Current exchange rate of 1 usd to lovelace
    scRate :: Integer, -- Current stable coin exchange rate for 1 stable coin
    rcRate :: Integer -- Curretn reserve coin exchange rate for 1 reserve coin
  }
  deriving stock (Generic,Prelude.Show)
  deriving anyclass (ToJSON, FromJSON)

--Used as json key to unify response of current rates
data RatesResponse = RatesResponse
  { 
    currentCoinsRates :: Rates
  }
  deriving stock (Generic,Prelude.Show)
  deriving anyclass (ToJSON, FromJSON)

--Used as json key to unify response of current state of the statemachine
data StateResponse = StateResponse
  { 
    currentCoinsState :: CoinsMachineState
  }
  deriving stock (Generic,Prelude.Show)
  deriving anyclass (ToJSON, FromJSON)

PlutusTx.makeLift ''ContractStatus
PlutusTx.makeLift ''CoinsMachineState
PlutusTx.makeLift ''BankParam
PlutusTx.unstableMakeIsData ''ContractStatus
PlutusTx.unstableMakeIsData ''CoinsMachineState
PlutusTx.unstableMakeIsData ''BankParam
PlutusTx.unstableMakeIsData ''BankInput
PlutusTx.unstableMakeIsData ''BankInputAction