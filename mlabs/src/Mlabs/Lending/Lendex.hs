{-# OPTIONS_GHC -fno-specialize #-}
{-# OPTIONS_GHC -fno-strictness #-}
module Mlabs.Lending.Lendex(
    userEndpoints
  , mkValidator
  , scriptInstance
) where

import Control.Monad (forever)
import Control.Monad.State.Strict (runStateT)

import Data.Functor (void)

import Plutus.V1.Ledger.Contexts (pubKeyHash)
import           Plutus.Contract
import qualified Plutus.Contract.StateMachine as SM
import qualified Ledger.Typed.Scripts         as Scripts
import qualified PlutusTx                     as PlutusTx
import           PlutusTx.Prelude             hiding (Applicative (..), check)

import Mlabs.Lending.Logic.React
import Mlabs.Lending.Logic.Types

type Lendex = SM.StateMachine LendingPool Act

{-# INLINABLE machine #-}
machine :: Lendex
machine = SM.mkStateMachine Nothing transition isFinal
  where
    isFinal = const False

{-# INLINABLE mkValidator #-}
mkValidator :: Scripts.ValidatorType Lendex
mkValidator = SM.mkValidator machine

client :: SM.StateMachineClient LendingPool Act
client = SM.mkStateMachineClient $ SM.StateMachineInstance machine scriptInstance

scriptInstance :: Scripts.ScriptInstance Lendex
scriptInstance = Scripts.validator @Lendex
  $$(PlutusTx.compile [|| mkValidator ||])
  $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator

{-# INLINABLE transition #-}
transition ::
     SM.State LendingPool
  -> Act
  -> Maybe (SM.TxConstraints SM.Void SM.Void, SM.State LendingPool)
transition SM.State{stateData=oldData, stateValue=oldValue} input = case runStateT (react input) oldData of
  Left _             -> Nothing
  Right (_, newData) -> Just (mempty, SM.State { stateData=newData, stateValue=oldValue })

type LendexError = SM.SMContractError

type UserLendexSchema =
  BlockchainActions
    .\/ Endpoint "user-action" UserAct

type UserApp a = Contract () UserLendexSchema LendexError a

userAction :: UserAct -> UserApp ()
userAction act = do
  pkh <- fmap pubKeyHash ownPubKey
  void $ SM.runStep client (UserAct (UserId pkh) act)

-- | Endpoints for user
userEndpoints :: UserApp ()
userEndpoints = forever userAction'
  where
    userAction' = endpoint @"user-action" >>= userAction

type PriceOracleLendexSchema =
  BlockchainActions
    .\/ Endpoint "price-oracle-action" PriceAct

type PriceOracleApp a = Contract () PriceOracleLendexSchema LendexError a

priceOracleAction :: PriceAct -> PriceOracleApp ()
priceOracleAction act = do
  void $ SM.runStep client (PriceAct act)

-- | Endpoints for price oracle
priceOracleEndpoints :: PriceOracleApp ()
priceOracleEndpoints = forever priceOracleAction'
  where
    priceOracleAction' = endpoint @"price-oracle-action" >>= priceOracleAction

type GovernLendexSchema =
  BlockchainActions
    .\/ Endpoint "govern-action" GovernAct

type GovernApp a = Contract () GovernLendexSchema LendexError a

governAction :: GovernAct -> GovernApp ()
governAction act = do
  void $ SM.runStep client (GovernAct act)

-- | Endpoints for admin user
governEndpoints :: GovernApp ()
governEndpoints = forever governAction'
  where
    governAction' = endpoint @"govern-action" >>= governAction

