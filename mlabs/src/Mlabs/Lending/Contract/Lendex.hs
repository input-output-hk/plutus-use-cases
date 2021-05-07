{-# OPTIONS_GHC -fno-specialize #-}
{-# OPTIONS_GHC -fno-strictness #-}
module Mlabs.Lending.Contract.Lendex(
    mkValidator
  , scriptInstance
  -- * Endpoints
  , UserLendexSchema, UserApp
  , userEndpoints
  , PriceOracleLendexSchema, PriceOracleApp
  , priceOracleEndpoints
  , GovernLendexSchema, GovernApp
  , governEndpoints
  , StartParams(..)
  -- * Test endpoints
  , callUserAct
  , callPriceOracleAct
  , callGovernAct
  , callStartLendex
) where

import Control.Monad (forever)
import Control.Monad.State.Strict (runStateT)

import Data.Aeson (FromJSON, ToJSON)
import Data.Functor (void)

import GHC.Generics

import Plutus.V1.Ledger.Contexts (pubKeyHash)
import           Plutus.Contract
import qualified Plutus.Contract.StateMachine as SM
import qualified Ledger.Typed.Scripts         as Scripts
import qualified PlutusTx                     as PlutusTx
import           PlutusTx.Prelude             hiding (Applicative (..), check)

import Mlabs.Lending.Logic.React
import Mlabs.Lending.Logic.Types
import qualified Mlabs.Lending.Contract.Forge as Forge

import Plutus.Trace.Emulator (EmulatorTrace, callEndpoint, activateContractWallet)
import qualified Wallet.Emulator as Emulator

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
  Left _err          -> Nothing
  Right (_, newData) -> Just (mempty, SM.State { stateData=newData, stateValue=oldValue })

-----------------------------------------------------------------------
-- endpoints and schemas

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
    .\/ Endpoint "start-lendex"  StartParams

data StartParams = StartParams
  { sp'coins  :: [(Coin, Rational)]  -- ^ supported coins with ratios to ADA
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

type GovernApp a = Contract () GovernLendexSchema LendexError a

governAction :: GovernAct -> GovernApp ()
governAction act = do
  void $ SM.runStep client (GovernAct act)

startLendex :: StartParams -> GovernApp ()
startLendex StartParams{..} = do
  void $ SM.runInitialise client (initLendingPool Forge.currencySymbol sp'coins) initValue
  where
    initValue = mempty

-- | Endpoints for admin user
governEndpoints :: GovernApp ()
governEndpoints = startLendex' >> forever governAction'
  where
    governAction' = endpoint @"govern-action" >>= governAction
    startLendex'  = endpoint @"start-lendex"  >>= startLendex

---------------------------------------------------------
-- call endpoints (for debug and testing)

-- | Calls user act
callUserAct :: Emulator.Wallet -> UserAct -> EmulatorTrace ()
callUserAct wal act = do
  hdl <- activateContractWallet wal userEndpoints
  void $ callEndpoint @"user-action" hdl act

-- | Calls price oracle act
callPriceOracleAct :: Emulator.Wallet -> PriceAct -> EmulatorTrace ()
callPriceOracleAct wal act = do
  hdl <- activateContractWallet wal priceOracleEndpoints
  void $ callEndpoint @"price-oracle-action" hdl act

-- | Calls govern act
callGovernAct :: Emulator.Wallet -> GovernAct -> EmulatorTrace ()
callGovernAct wal act = do
  hdl <- activateContractWallet wal governEndpoints
  void $ callEndpoint @"govern-action" hdl act

-- | Calls initialisation of state for Lending pool
callStartLendex :: Emulator.Wallet -> StartParams -> EmulatorTrace ()
callStartLendex wal sp = do
  hdl <- activateContractWallet wal governEndpoints
  void $ callEndpoint @"start-lendex" hdl sp

