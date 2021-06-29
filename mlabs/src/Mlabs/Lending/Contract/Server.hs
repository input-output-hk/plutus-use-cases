-- | Server for lendex application
module Mlabs.Lending.Contract.Server(
  -- * Contract monads
    UserContract
  , OracleContract
  , AdminContract
  -- * Endpoints
  , userEndpoints
  , oracleEndpoints
  , adminEndpoints
  -- * Errors
  , LendexError
  , LendexStatus(..)
) where

import Prelude
import Control.Monad

import qualified Data.Map as M
import Data.List.Extra (firstJust)

import Playground.Contract
import Plutus.V1.Ledger.Crypto
import Plutus.V1.Ledger.Api
import Plutus.Contract
import Ledger.Constraints

import Mlabs.Emulator.Types
import Mlabs.Lending.Logic.Types

import Mlabs.Plutus.Contract
import Mlabs.Lending.Contract.Api
import Mlabs.Lending.Contract.StateMachine
import qualified Mlabs.Lending.Contract.Forge as Forge

import Data.Monoid (Last)

type LendexStatus = Last (LendexId, LendingPool)

-- | User contract monad
type UserContract a = Contract () UserSchema LendexError a

-- | Oracle contract monad
type OracleContract a = Contract () OracleSchema LendexError a

-- | Admin contract monad
type AdminContract a = Contract LendexStatus AdminSchema LendexError a

----------------------------------------------------------
-- endpoints

-- | Endpoints for user
userEndpoints :: LendexId -> UserContract  ()
userEndpoints lid = forever $ selects
  [ act $ getEndpoint @Deposit
  , act $ getEndpoint @Borrow
  , act $ getEndpoint @Repay
  , act $ getEndpoint @SwapBorrowRateModel
  , act $ getEndpoint @SetUserReserveAsCollateral
  , act $ getEndpoint @Withdraw
  , act $ getEndpoint @LiquidationCall
  ]
  where
    act :: IsUserAct a => UserContract a -> UserContract ()
    act readInput = readInput >>= userAction lid


-- | Endpoints for price oracle
oracleEndpoints :: LendexId -> OracleContract ()
oracleEndpoints lid = forever $ selects
  [ act $ getEndpoint @SetAssetPrice
  ]
  where
    act :: IsPriceAct a => OracleContract a -> OracleContract ()
    act readInput = readInput >>= priceOracleAction lid

-- | Endpoints for admin
adminEndpoints :: LendexId -> AdminContract ()
adminEndpoints lid = do
  getEndpoint @StartParams >>= (startLendex lid)
  forever $ selects
    [ act $ getEndpoint @AddReserve
    ]
  where
    act :: IsGovernAct a => AdminContract a -> AdminContract ()
    act readInput = readInput >>= adminAction lid

-- actions

userAction :: IsUserAct a => LendexId -> a -> UserContract ()
userAction lid input = do
  pkh <- pubKeyHash <$> ownPubKey
  act <- getUserAct input
  inputDatum <- findInputStateDatum lid
  let lookups = monetaryPolicy (Forge.currencyPolicy lid) <>
                ownPubKeyHash  pkh
      constraints = mustIncludeDatum inputDatum
  runStepWith lid act lookups constraints

priceOracleAction :: IsPriceAct a => LendexId -> a -> OracleContract ()
priceOracleAction lid input = runStep lid =<< getPriceAct input

adminAction :: IsGovernAct a => LendexId -> a -> AdminContract ()
adminAction lid input = runStep lid =<< getGovernAct input

startLendex :: LendexId -> StartParams -> AdminContract ()
startLendex lid StartParams{..} =
  runInitialise lid  (initLendingPool (Forge.currencySymbol lid) sp'coins (fmap UserId sp'admins) (fmap UserId sp'oracles)) sp'initValue

----------------------------------------------------------
-- to act conversion

-- | Converts endpoint inputs to logic actions
getUserAct :: IsUserAct a => a -> UserContract Act
getUserAct act = do
  uid <- ownUserId
  t   <- getCurrentTime
  pure $ UserAct t uid $ toUserAct act

-- | Converts endpoint inputs to logic actions
getPriceAct :: IsPriceAct a => a -> OracleContract Act
getPriceAct act = do
  uid <- ownUserId
  t   <- getCurrentTime
  pure $ PriceAct t uid $ toPriceAct act

getGovernAct :: IsGovernAct a => a -> AdminContract Act
getGovernAct act = do
  uid <- ownUserId
  pure $ GovernAct uid $ toGovernAct act

getCurrentTime :: (HasBlockchainActions s, AsContractError e) => Contract w s e Integer
getCurrentTime = getSlot <$> currentSlot

----------------------------------------------------------

findInputStateDatum :: LendexId -> UserContract Datum
findInputStateDatum lid = do
  utxos <- utxoAt (lendexAddress lid)
  maybe err pure $ firstJust (readDatum . snd) $ M.toList utxos
  where
    err = throwError $ toLendexError "Can not find Lending app instance"


