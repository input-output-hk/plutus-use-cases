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
  ( CoinsMachineState (..),
    BankParam (..),
    BankInput (..),
    BankInputAction (..),
    EndpointInput (..),
  )
where

import           Data.Aeson                             (FromJSON, ToJSON)
import           GHC.Generics                            (Generic)
import qualified Prelude
import           PlutusTx.Ratio as Ratio
import           Ledger.Scripts               (MonetaryPolicyHash)
import           Ledger.Value                 (TokenName (TokenName))
import           Plutus.Contracts.Oracle.Core
import qualified PlutusTx                      as PlutusTx
import           PlutusTx.Prelude
import           Ledger                        hiding (to)
import           Playground.Contract           (ToSchema)



data CoinsMachineState = CoinsMachineState
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
    minReserveRatio :: Ratio Integer,
    maxReserveRatio :: Ratio Integer,
    rcDefaultRate :: Integer,
    oracleParam :: Oracle
  }
  deriving stock (Generic, Prelude.Ord, Prelude.Eq, Prelude.Show)
  deriving anyclass (ToJSON, FromJSON)

--
data BankInputAction
  = MintStableCoin Integer
  | RedeemStableCoin Integer
  | MintReserveCoin Integer
  | RedeemReserveCoin Integer
  deriving stock (Generic, Prelude.Eq, Prelude.Show)
  deriving anyclass (ToJSON, FromJSON)

type OracleOutput = (TxOutRef, TxOut, Integer)
--
data BankInput = BankInput
  { 
    bankInputAction :: BankInputAction,
    oracleOutput :: OracleOutput
  }
  deriving stock (Generic, Prelude.Eq, Prelude.Show)
  deriving anyclass (ToJSON, FromJSON)

data EndpointInput = EndpointInput
  { 
    tokenAmount :: Integer
  }
  deriving stock (Generic, Prelude.Eq, Prelude.Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

PlutusTx.makeLift ''CoinsMachineState
PlutusTx.makeLift ''BankParam
PlutusTx.unstableMakeIsData ''CoinsMachineState
PlutusTx.unstableMakeIsData ''BankParam
PlutusTx.unstableMakeIsData ''BankInput
PlutusTx.unstableMakeIsData ''BankInputAction