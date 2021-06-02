{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ViewPatterns          #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:debug-context #-}


module Plutus.Contracts.CoinsStateMachine(
    scriptInstance
    , machineClient
    , endpoints
    , BankState(..)
    , BankParam(..)
    , BankInput(..)
    , BankInputAction(..)
    , BankStateSchema
    , BankStateError
    ) where

import           Control.Monad                   (guard, void)
import           Data.Text                    (Text, pack)
import Data.Fixed ( Fixed(MkFixed) )
import           Data.Aeson                      (FromJSON, ToJSON)
import           GHC.Generics                    (Generic)
import           Ledger                          hiding (to)
import           Ledger.Constraints              (TxConstraints)
import qualified Ledger.Constraints              as Constraints
import           Ledger.Typed.Scripts            (scriptHash)
import qualified Ledger.Typed.Scripts            as Scripts
import           Ledger.Typed.Scripts.Validators (forwardingMPS)
import qualified Ledger.Ada                      as Ada
import qualified PlutusTx                        as PlutusTx
import           PlutusTx.Prelude

import           Plutus.Contract.StateMachine    (SMContractError, State (..), StateMachine, StateMachineClient (..), StateMachineInstance (..), Void)
import qualified Plutus.Contract.StateMachine    as SM

import           Plutus.Contract
import qualified Prelude

--
data BankState =
    BankState
        { baseReserveAmount :: Integer
        , stableCoinAmount :: Integer
        , reserveCoinAmount :: Integer
        , policyScript :: MonetaryPolicyHash
        }
    deriving stock (Generic, Prelude.Eq, Prelude.Show)
    deriving anyclass (ToJSON, FromJSON)

--
data BankParam = 
     BankParam
        { stableCoinTokenName :: TokenName
        , reserveCoinTokenName :: TokenName
        }
    deriving stock (Generic, Prelude.Eq, Prelude.Show)
    deriving anyclass (ToJSON, FromJSON)

--
data BankInputAction = 
    MintStableCoin Integer
    | MintReserveCoin Integer
    deriving stock (Generic, Prelude.Eq, Prelude.Show)
    deriving anyclass (ToJSON, FromJSON)

--
data BankInput =
    BankInput
        {
            bankInputAction :: BankInputAction
        }

{-# INLINABLE lovelaces #-}
lovelaces :: Value -> Integer
lovelaces = Ada.getLovelace . Ada.fromValue

--TODO check for observation slot for oracle so apply date constraints must validate in
--TODO conversion of rc amount and sc amount to base currency
{-# INLINEABLE transition #-}
transition :: BankParam -> State BankState -> BankInput -> Maybe (TxConstraints Void Void, State BankState)
transition bankParam State{stateData=oldState} BankInput{bankInputAction} = do
    let (newConstraints, newState) = case bankInputAction of 
            MintReserveCoin rcAmt->
                let constraints = Constraints.mustForgeCurrency (policyScript oldState) (reserveCoinTokenName bankParam) rcAmt in
                (constraints
                    , oldState
                    { reserveCoinAmount = reserveCoinAmount oldState + rcAmt
                    , baseReserveAmount = baseReserveAmount oldState + (rcAmt * 1)  
                    })
            MintStableCoin scAmt ->
                let constraints = Constraints.mustForgeCurrency (policyScript oldState) (stableCoinTokenName bankParam) scAmt in
                (constraints
                    , oldState
                    { stableCoinAmount = stableCoinAmount oldState + scAmt
                    , baseReserveAmount = baseReserveAmount oldState + (scAmt * 1)
                    })

            -- ReedemStableCoin scAmt ->
            --     let constraints = Constraints.mustForgeCurrency (policyScript bankState) (stableCoinTokenName bankParam) (negate scAmt) in
            --     (constraints
            --         , bankState
            --         { stableCoinAmount = stableCoinAmount bankState - scAmt
            --         , baseReserveAmount = baseReserveAmount bankState - (scAmt * 1)
            --         })

            -- ReedemReserveCoin rcAmt ->
            --     let constraints = Constraints.mustForgeCurrency (policyScript bankState) (reserveCoinTokenName bankParam) (negate rcAmt) in
            --     (constraints
            --         , bankState
            --         { reserveCoinAmount = reserveCoinAmount bankState - scAmt
            --         , baseReserveAmount = baseReserveAmount bankState - (scAmt * 1)
            --         })

                      
    guard $ isNewStateValid bankParam newState
    let state = State{ stateData=newState, stateValue = Ada.lovelaceValueOf (baseReserveAmount newState)}
    pure (newConstraints, state)


{-# INLINEABLE isNewStateValid #-}
isNewStateValid :: BankParam -> BankState -> Bool
isNewStateValid bankParam bankState = isRight (checkForValidState bankParam bankState)

{-# INLINEABLE checkForValidState #-}
checkForValidState :: BankParam -> BankState -> Either ErrorState ()
checkForValidState bankParam bankState@BankState{baseReserveAmount, stableCoinAmount, reserveCoinAmount} = do
    unless (baseReserveAmount >= 0) (Left NegativeReserves)
    unless (reserveCoinAmount >= 0) (Left NegativeReserveCoins)
    unless (stableCoinAmount >= 0) (Left NegativeStablecoins)
    -- unless (liabilities bankState >= zero) (Left NegativeLiabilities)
    -- unless (equity bankState >= zero) (Left NegativeEquity)

    -- let allowedReserves = (,) <$> minReserve bankParam bankState <*> maxReserve bankParam bankState

    -- case allowedReserves of
    --     Just (minReserves, maxReserves) -> do
    --         unless (baseReserveAmount >= minReserves) (Left $ MinReserves minReserves actualReserves)
    --         unless (baseReserveAmount <= maxReserves) (Left $ MaxReserves maxReserves actualReserves)
    --     Nothing -> pure ()

-- {-# INLINEABLE liabilities #-}
-- liabilities ::    BankState    -> ConversionRate    -> BC (Ratio Integer)
-- liabilities BankState{baseReserveAmount, stableCoinAmount, reserveCoinAmount} cr =
--     let stableCoinLiabilities = convert cr (PC $ fromInteger stablecoins) stableCoinAmount * 1
--     in BC (min (fromInteger reserves) stableCoinLiabilities)

-- {-# INLINEABLE equity #-}
-- equity ::    BankState    -> ConversionRate    -> BC (Ratio Integer)
-- equity r@BankState{bsReserves=BC reserves} cr =
--     let BC l = liabilities r cr
--     in BC (fromInteger reserves - l)


data ErrorState
    = NegativeReserveCoins
    | NegativeReserves
    | NegativeStablecoins
    | MinReserves { allowed :: Integer, actual :: Integer  }
    | MaxReserves { allowed :: Integer, actual :: Integer  }
    | NegativeLiabilities
    | NegativeEquity
    deriving (Show)

bankMachine :: BankParam -> StateMachine BankState BankInput
bankMachine bankParam = SM.mkStateMachine Nothing (transition bankParam) isFinal where
    isFinal _ = False

scriptInstance :: BankParam -> Scripts.ScriptInstance (StateMachine BankState BankInput)
scriptInstance bankParam =
    let val = $$(PlutusTx.compile [|| validator ||]) `PlutusTx.applyCode` PlutusTx.liftCode bankParam
        validator param = SM.mkValidator (bankMachine param)
        wrap = Scripts.wrapValidator @BankState @BankInput
    in Scripts.validator @(StateMachine BankState BankInput) val $$(PlutusTx.compile [|| wrap ||])

machineClient ::
    Scripts.ScriptInstance (StateMachine BankState BankInput)
    -> BankParam
    -> StateMachineClient BankState BankInput
machineClient scriptInst bankParam =
    let machine = bankMachine bankParam
    in SM.mkStateMachineClient (StateMachineInstance machine scriptInst)

initialState :: StateMachineClient BankState BankInput -> BankState
initialState StateMachineClient{scInstance=StateMachineInstance{validatorInstance}} =
    BankState
        { baseReserveAmount = 0
        , stableCoinAmount = 0
        , reserveCoinAmount = 0
        , policyScript = monetaryPolicyHash $ forwardingMPS $ scriptHash validatorInstance
        }

data BankStateError =
    StartError ContractError
    | StateMachineError SMContractError
    | RunStepError ContractError
    deriving stock (Prelude.Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- makeClassyPrisms ''BankStateError

-- instance AsContractError BankStateError where
--     _ContractError = _MSContractError

-- instance AsSMContractError BankStateError where
--     _SMContractError = _MSStateMachineError

stableCoinName :: TokenName
stableCoinName = "StableToken"

reserveCoinName :: TokenName
reserveCoinName = "ReserveToken"

bp :: BankParam
bp = BankParam
            {   stableCoinTokenName = stableCoinName
            ,   reserveCoinTokenName = reserveCoinName
            }

client :: StateMachineClient BankState BankInput
client = machineClient (scriptInstance bp) bp

mapError' :: Contract w s SMContractError a -> Contract w s Text a
mapError' = mapError $ pack . show

start :: HasBlockchainActions s => Integer -> Contract w s Text ()
start i = do
    void $ mapError' $ SM.runInitialise client (initialState client) mempty


mintStableCoin :: HasBlockchainActions s => Integer -> Contract w s Text ()
mintStableCoin noOfStableCoins= do
        let input = BankInput{
                    bankInputAction = MintStableCoin noOfStableCoins
                }
        void $ mapError' $ SM.runStep client input 
   

type BankStateSchema =
    BlockchainActions
        .\/ Endpoint "start" Integer
        .\/ Endpoint "mintStableCoin" Integer

endpoints :: Contract () BankStateSchema Text ()
endpoints = (start'  `select` mintStableCoin')>> endpoints
  where
    start'  = endpoint @"start"  >>= start
    mintStableCoin' = endpoint @"mintStableCoin" >>= mintStableCoin

-- contract ::
--     ( AsContractError e
--     , AsSMContractError e
--     )
--     => Contract () BankStateSchema e ()
-- contract = forever endpoints  where
--     bankParam = BankParam
--                 {   stableCoinTokenName = stableCoinName
--                 ,   reserveCoinTokenName = reserveCoinName
--                 }

--     client = machineClient (scriptInstance bankParam) bankParam
--     endpoints = (TransitionSuccess <$> start) `select` mintStableCoin `select` mintReserveCoin
--     start = do
--         mapError StateMachineError $ SM.runInitialise client (initialState client) mempty
   
--     mintStableCoin  = do
--         noOfStableCoins <- mapError RunStepError (endpoint @"mintStableCoin")
--         let input = BankInput{
--                     bankInputAction = MintStableCoin noOfStableCoins
--                 }
--         mapError StateMachineError $ SM.runStep client input 
   
--     mintReserveCoin = do
--         noOfReserveCoins <- mapError RunStepError (endpoint @"mintReserveCoin")
--         let input = BankInput{
--                     bankInputAction = MintReserveCoin noOfReserveCoins
--                 }
--         mapError StateMachineError $ SM.runStep client input 




PlutusTx.makeLift ''BankState
PlutusTx.makeLift ''BankParam
PlutusTx.unstableMakeIsData ''BankState
PlutusTx.unstableMakeIsData ''BankParam
PlutusTx.unstableMakeIsData ''BankInput
PlutusTx.unstableMakeIsData ''BankInputAction