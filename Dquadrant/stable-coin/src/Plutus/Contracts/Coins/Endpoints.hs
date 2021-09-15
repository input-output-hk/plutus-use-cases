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
{-# LANGUAGE NamedFieldPuns #-}

module Plutus.Contracts.Coins.Endpoints
  ( 
    BankStateSchema,
    coinsEndpoints,
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
import           Plutus.Contract.StateMachine     (SMContractError, StateMachineClient (..))
import           Playground.TH                     (mkSchemaDefinitions)
import           Ledger.Typed.Tx              (TypedScriptTxOut (..))
import qualified Data.Aeson.Types as Types
import Data.Aeson (toJSON)
import           Ledger.AddressMap                 (UtxoMap)

-- Get forwarding monteragy policy script hash from state machine instance
forwardMPS :: StateMachineClient CoinsMachineState BankInput -> MintingPolicyHash
forwardMPS StateMachineClient {scInstance} = Scripts.forwardingMintingPolicyHash $ SM.typedValidator scInstance

--Initial state of state machine to be set to all values to 0 with mps from script instance
initialState :: StateMachineClient CoinsMachineState BankInput -> CoinsMachineState
initialState smClient =
  CoinsMachineState
    { baseReserveAmount = 0,
      stableCoinAmount = 0,
      reserveCoinAmount = 0,
      policyScript = forwardMPS smClient,
      bankFee = 1 % 1,
      contractStatus = Running
    }

--Helper to convert SM contract error to Text error
mapError' :: Contract w s SMContractError a -> Contract w s Text a
mapError' = mapError $ pack . Prelude.show

--TODO handle error
--Run step function to construct lookups and execute run step of state machine
-- Specific input to state machine i.e either minting or redeeming of coins is passed 
smRunStep :: BankParam -> BankInputAction -> Contract w s Text ()
smRunStep bankParam@BankParam{oracleParam} bankInputAction = do
  let client = machineClient (scriptInstance bankParam) bankParam

  oracle <- findOracle oracleParam
  
  case oracle of
    Nothing -> logInfo @Prelude.String "Oracle not found"
    Just (oref, o, x) -> do      
      let lookups = Constraints.unspentOutputs (Map.singleton oref o)
                    <> Constraints.otherScript (oracleValidator oracleParam)               
                                 
          input = BankInput bankInputAction (oref, txOutTxOut o, x)
              

      result <-  mapError' $ SM.runStepWith lookups mempty client input
      
      case result of
        SM.TransitionFailure e -> do
          void $ logInfo @Prelude.String $ "Transistion Faliure "
          throwError "Transistion Faliure"
        _ -> do
          void $ logInfo @Prelude.String $ "Endpoint call completed " ++ Prelude.show bankInputAction

--TODO check for handling multiple start call
--Endpoint to start the stable coint contract with  parameters supplied to banks
start :: BankParam -> Promise w BankStateSchema Text ()
start bankParam = (endpoint @"start") $ \_ -> do
    let client = machineClient (scriptInstance bankParam) bankParam
    void $ mapError' $ SM.runInitialise client (initialState client) mempty
    void $ logInfo @Prelude.String $ "Stable coin contract started. " ++ Prelude.show (initialState client)

--TODO check for validation in offchain
-- Contract endpoint for minting of stable coin
mintStableCoin :: BankParam -> Promise w BankStateSchema Text ()
mintStableCoin bankParam = 
  (endpoint @"mintStableCoin") $ \EndpointInput{tokenAmount} -> do
    smRunStep bankParam $ MintStableCoin tokenAmount

-- Contract endpoint for redeeming of stable coin to get ada back at current stable coin rate
redeemStableCoin :: BankParam -> Promise w BankStateSchema Text ()
redeemStableCoin bankParam = 
  (endpoint @"redeemStableCoin") $ \EndpointInput{tokenAmount} -> do
    smRunStep bankParam $ RedeemStableCoin tokenAmount

-- Contract endpoint for minting of reserve coin
mintReserveCoin :: BankParam -> Promise w BankStateSchema Text ()
mintReserveCoin bankParam = 
  (endpoint @"mintReserveCoin") $ \EndpointInput{tokenAmount} -> do
    smRunStep bankParam $ MintReserveCoin tokenAmount

-- Contract endpoint for redeeming of resever coin  to get ada back at current reserve rate
redeemReserveCoin :: BankParam -> Promise w BankStateSchema Text ()
redeemReserveCoin bankParam = 
  (endpoint @"redeemReserveCoin") $ \EndpointInput{tokenAmount} -> do
    smRunStep bankParam $ RedeemReserveCoin tokenAmount

updateBankFee :: BankParam -> Promise w BankStateSchema Text ()
updateBankFee bankParam = 
  (endpoint @"updateBankFee") $ \BankFeeInput{percentNumerator,percentDenominator} -> do
    let client = machineClient (scriptInstance bankParam) bankParam
        input = UpdateBankFee percentNumerator percentDenominator
        
    result <- mapError' $ SM.runStep client input
    case result of
      SM.TransitionFailure e -> do
        void $ logInfo @Prelude.String $ "Transistion Faliure "
        throwError "Transistion Faliure"
      _ -> do
        void $ logInfo @Prelude.String $ "Endpoint call completed " ++ Prelude.show input

updateContractStatus :: BankParam -> Promise w BankStateSchema Text ()
updateContractStatus bankParam = 
  (endpoint @"updateContractStatus") $ \ContractStatusInput{shouldPause} -> do
    let client = machineClient (scriptInstance bankParam) bankParam
        input = UpdateContractStatus shouldPause
                
    result <- mapError' $ SM.runStep client input

    case result of
      SM.TransitionFailure e -> do
        void $ logInfo @Prelude.String $ "Transistion Faliure Couldn't proceed further."
        throwError "Transistion Faliure"
      _ -> do
        void $ logInfo @Prelude.String $ "Endpoint call completed " ++ Prelude.show input


--Endpoint definitions availabe for the stable coin contract
type BankStateSchema =
    Endpoint "start" Integer
    .\/ Endpoint "mintStableCoin" EndpointInput
    .\/ Endpoint "redeemStableCoin" EndpointInput
    .\/ Endpoint "mintReserveCoin" EndpointInput
    .\/ Endpoint "redeemReserveCoin" EndpointInput
    .\/ Endpoint "updateBankFee" BankFeeInput
    .\/ Endpoint "updateContractStatus" ContractStatusInput

    .\/ Endpoint "funds" Prelude.String
    .\/ Endpoint "currentState" Prelude.String
    .\/ Endpoint "currentRates" Prelude.String

--TODO writer value [Types.Value]
--Starting point of the contract which combines all the endpoints to be available for call
coinsEndpoints :: BankParam -> Contract [Types.Value] BankStateSchema Text ()
coinsEndpoints bankParam = handleError handler (void selections) >> coinsEndpoints bankParam
  where 
    selections= selectList
      [ 
        start bankParam
      , mintStableCoin bankParam
      , redeemStableCoin bankParam
      , mintReserveCoin bankParam
      , redeemReserveCoin bankParam
      , updateBankFee bankParam
      , updateContractStatus bankParam
      , ownFunds
      , currentCoinsMachineState bankParam
      , currentRates bankParam
      ]
    
    handler :: Prelude.Show a => a -> Contract w s e ()
    handler e = do
        Contract.logError $ Prelude.show e

--Endpoint for getting current funds held in a users public key
ownFunds:: Promise [Types.Value ] BankStateSchema Text  ()
ownFunds = 
  (endpoint @"funds") $ \_ -> do
    pk    <- ownPubKey
    utxos <- utxoAt $ pubKeyAddress pk
    let v = mconcat $ Map.elems $ txOutValue . txOutTxOut Prelude.<$> utxos
    logInfo @Prelude.String $ "own funds: " ++ Prelude.show (flattenValue v)
    tell [ toJSON v]
                 
--Endpoint for getting current state of state machine i.e current tokesn supply, base reserves etc.
currentCoinsMachineState:: BankParam -> Promise [Types.Value ] BankStateSchema Text  ()
currentCoinsMachineState bankParam = 
  (endpoint @"currentState") $ \_ -> do
    currentState <- mapError' $ getCurrentState bankParam
    case currentState of
      Just state -> do
          logInfo @Prelude.String $ "Current state: " ++ Prelude.show state
          let stateResponse = StateResponse{
            currentCoinsState = state
          }
          tell [toJSON stateResponse]
      Nothing -> logWarn @Prelude.String $ "Current state is not present yet."

--Endpoint for getting combined rates of peg, stable coin rate and reserve coin rate
currentRates :: BankParam -> Promise [Types.Value ] BankStateSchema Text  ()
currentRates bankParam = 
  (endpoint @"currentRates") $ \_ -> do
    oracle <- findOracle $ oracleParam bankParam
    case oracle of
      Nothing -> logWarn @Prelude.String "Oracle not found"
      Just (oref, o, rate) -> do
        currentState <- mapError' $ getCurrentState bankParam
        case currentState of
          Just state -> do
              let rcRate = calcReserveCoinRate bankParam state rate
                  scRate = calcStableCoinRate state rate
                  rates = Rates {
                            pegRate = rate,
                            scRate = scRate,
                            rcRate = rcRate
                  }
                  ratesResponse = RatesResponse{
                    currentCoinsRates = rates
                  }
              logInfo @Prelude.String $ "Current rates: " ++ Prelude.show rates
              tell [toJSON ratesResponse]
          Nothing -> logWarn @Prelude.String $ "Current state is not present yet."

--Helper fucntion for getting current state of state machine
getCurrentState :: ( SM.AsSMContractError e)
    => BankParam -> Contract w schema e (Maybe CoinsMachineState)
getCurrentState bankParam = do
  let client = machineClient (scriptInstance bankParam) bankParam
  
  currentStateVal <- SM.getOnChainState client
  
  case currentStateVal of
      Just (onChainState, utxo) -> do
        let SM.OnChainState{ocsTxOut=TypedScriptTxOut{tyTxOutData=state, tyTxOutTxOut}, ocsTxOutRef} = onChainState
        pure $ Just state
      Nothing -> do
        logWarn @Prelude.String $ "Current state is not present yet."
        pure $ Nothing
