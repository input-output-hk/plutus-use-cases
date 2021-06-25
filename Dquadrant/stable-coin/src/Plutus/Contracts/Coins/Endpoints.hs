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

module Plutus.Contracts.Coins.Endpoints
  ( 
    BankStateSchema,
    coinsContract,
  )
where

import Control.Monad (void)
import           Ledger.Scripts                   (MonetaryPolicyHash)
import           Plutus.Contract.StateMachine     (SMContractError, StateMachineClient (..))
import qualified Plutus.Contract.StateMachine     as SM
import           Plutus.Contract
import qualified Prelude
import           PlutusTx.Prelude
import           Playground.TH                     (mkKnownCurrencies, mkSchemaDefinitions)
import           Data.Text                         (Text, pack)
import qualified Ledger.Typed.Scripts              as Scripts
import Ledger hiding (to)
import Prelude (show)
import           Plutus.Contracts.Coins.Types
import           Plutus.Contracts.Coins.CoinsStateMachine
import           Plutus.Contracts.Oracle.Core

import qualified Data.Map as Map
import qualified Data.Aeson.Types as Types
import Data.Aeson (toJSON)
import Ledger.Value (flattenValue )

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
smRunStep bankParam bankInputAction = do
  let client = machineClient (scriptInstance bankParam) bankParam

  oracle <- findOracle $ oracleParam bankParam
  case oracle of
    Nothing -> logInfo @Prelude.String "Oracle not found"
    Just (oref, o, x) -> do
      logInfo @Prelude.String $ show oref
      logInfo @Prelude.String $ show o
      logInfo @Prelude.String $ show x
      logInfo @Prelude.String $ show bankInputAction
      let input =
            BankInput
              { bankInputAction = bankInputAction
              , oracleOutput = (oref, txOutTxOut o, x)
              }
      void $ mapError' $ SM.runStep client input
      void $ logInfo @Prelude.String $ "Endpoint call completed " ++ show bankInputAction


--TODO check for validation in offchain
mintStableCoin :: HasBlockchainActions s => BankParam -> EndpointInput -> Contract w s Text ()
mintStableCoin bankParam endpointInput@EndpointInput {tokenAmount} = smRunStep bankParam $ MintStableCoin tokenAmount

redeemStableCoin :: HasBlockchainActions s => BankParam -> EndpointInput -> Contract w s Text ()
redeemStableCoin bankParam endpointInput@EndpointInput {tokenAmount} = smRunStep bankParam $ RedeemStableCoin tokenAmount

mintReserveCoin :: HasBlockchainActions s => BankParam -> EndpointInput -> Contract w s Text ()
mintReserveCoin bankParam endpointInput@EndpointInput {tokenAmount} = smRunStep bankParam $ MintReserveCoin tokenAmount

redeemReserveCoin :: HasBlockchainActions s => BankParam -> EndpointInput -> Contract w s Text ()
redeemReserveCoin bankParam endpointInput@EndpointInput {tokenAmount} = smRunStep bankParam $ RedeemReserveCoin tokenAmount

type BankStateSchema =
  BlockchainActions
    .\/ Endpoint "start" Integer
    .\/ Endpoint "mintStableCoin" EndpointInput
    .\/ Endpoint "redeemStableCoin" EndpointInput
    .\/ Endpoint "mintReserveCoin" EndpointInput
    .\/ Endpoint "redeemReserveCoin" EndpointInput
    .\/ Endpoint "funds" Prelude.String

mkSchemaDefinitions ''BankStateSchema

coinsContract :: BankParam -> Contract [Types.Value ] BankStateSchema Text ()
coinsContract bankParam =
  ( start'
      `select` mintStableCoin'
      `select` redeemStableCoin'
      `select` mintReserveCoin'
      `select` redeemReserveCoin'
      `select` ownFunds'
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


ownFunds:: HasBlockchainActions s => BankParam -> Contract [Types.Value ] s Text  ()
ownFunds _ = do
    pk    <- ownPubKey
    utxos <- utxoAt $ pubKeyAddress pk
    let v = mconcat $ Map.elems $ txOutValue . txOutTxOut Prelude.<$> utxos
    logInfo @Prelude.String $ "own funds: " ++ show (flattenValue v)
    tell [ toJSON v]
