{-# LANGUAGE NamedFieldPuns #-}

-- | Server for lendex application
module Mlabs.Lending.Contract.Server (
  -- * Contract monads
  UserContract,
  OracleContract,
  AdminContract,

  -- * Endpoints
  userEndpoints,
  oracleEndpoints,
  adminEndpoints,
  queryEndpoints,

  -- * Errors
  StateMachine.LendexError,
) where

import Control.Lens ((^.))
import Control.Monad (guard)
import Control.Monad.State.Strict (runStateT)

import Data.Bifunctor (second)
import Data.List.Extra (firstJust)
import Data.Map qualified as Map
import Data.Semigroup (Last (..))

import Ledger (PaymentPubKeyHash (PaymentPubKeyHash))
import Ledger.Constraints (mintingPolicy, mustIncludeDatum, ownPaymentPubKeyHash)
import Ledger.Tx (ChainIndexTxOut, ciTxOutAddress)

import Plutus.Contract ()
import Plutus.Contract qualified as Contract
import Plutus.V1.Ledger.Api (Datum (..))
import Plutus.V1.Ledger.Slot (getSlot)
import Plutus.V1.Ledger.Tx

import PlutusTx (FromData)
import PlutusTx.AssocMap qualified as M

import Mlabs.Emulator.Types (UserId (..), ownUserId)
import Mlabs.Lending.Contract.Api qualified as Api
import Mlabs.Lending.Contract.Forge (currencyPolicy, currencySymbol)
import Mlabs.Lending.Contract.StateMachine qualified as StateMachine
import Mlabs.Lending.Logic.React qualified as React
import Mlabs.Lending.Logic.Types qualified as Types
import Mlabs.Plutus.Contract (getEndpoint, readChainIndexTxDatum, readDatum', selectForever)
import Plutus.Contract.Request qualified as Request

import PlutusTx.Prelude
import Prelude qualified as Hask

-- | User contract monad
type UserContract a = Contract.Contract () Api.UserSchema StateMachine.LendexError a

-- | Oracle contract monad
type OracleContract a = Contract.Contract () Api.OracleSchema StateMachine.LendexError a

-- | Admin contract monad
type AdminContract a = Contract.Contract () Api.AdminSchema StateMachine.LendexError a

-- | Query contract monad
type QueryContract a = Contract.Contract QueryResult Api.QuerySchema StateMachine.LendexError a

type QueryResult = Maybe (Last Types.QueryRes)

----------------------------------------------------------
-- endpoints

-- | Endpoints for user
userEndpoints :: Types.LendexId -> UserContract ()
userEndpoints lid =
  selectForever
    [ getEndpoint @Api.Deposit $ userAction lid
    , getEndpoint @Api.Borrow $ userAction lid
    , getEndpoint @Api.Repay $ userAction lid
    , getEndpoint @Api.SwapBorrowRateModel $ userAction lid
    , getEndpoint @Api.AddCollateral $ userAction lid
    , getEndpoint @Api.RemoveCollateral $ userAction lid
    , getEndpoint @Api.Withdraw $ userAction lid
    , getEndpoint @Api.LiquidationCall $ userAction lid
    ]

-- TODO fix repetition
-- where
-- act :: Api.IsUserAct a => UserContract a -> UserContract ()
-- act readInput = readInput >>= userAction lid

-- | Endpoints for price oracle
oracleEndpoints :: Types.LendexId -> OracleContract ()
oracleEndpoints lid =
  selectForever
    [ getEndpoint @Api.SetAssetPrice $ priceOracleAction lid
    ]

-- | Endpoints for admin
adminEndpoints :: Types.LendexId -> AdminContract ()
adminEndpoints lid = do
  Contract.toContract $ getEndpoint @Api.StartLendex $ startLendex lid
  selectForever
    [ getEndpoint @Api.AddReserve $ adminAction lid
    ]

{- | Endpoints for querrying Lendex state:
   * `QueryAllLendexes` - returns a list of `LendingPool` data associated with each available lendes
   * `QuerySupportedCurrencies` - returns the list of supported currencies (see `SupportedCurrency`) for current `LendingPool`
   * `QuerryCurrentBalance` - returns a list of all the users, together with their current balances.
-}
queryEndpoints :: Types.LendexId -> QueryContract ()
queryEndpoints lid =
  selectForever
    [ getEndpoint @Api.QueryAllLendexes $ queryAllLendexes lid
    , getEndpoint @Api.QuerySupportedCurrencies $ \_ -> querySupportedCurrencies lid
    , getEndpoint @Api.QueryCurrentBalance $ queryAction lid
    , getEndpoint @Api.QueryInsolventAccounts $ queryAction lid
    ]

-- user actions

userAction :: Api.IsUserAct a => Types.LendexId -> a -> UserContract ()
userAction lid input = do
  pkh <- Contract.ownPaymentPubKeyHash
  act <- getUserAct input
  inputDatum <- findInputStateDatum lid
  let lookups =
        mintingPolicy (currencyPolicy lid)
          Hask.<> ownPaymentPubKeyHash pkh
      constraints = mustIncludeDatum inputDatum
  StateMachine.runStepWith lid act lookups constraints

-- Oracle actions

priceOracleAction :: Api.IsPriceAct a => Types.LendexId -> a -> OracleContract ()
priceOracleAction lid input = StateMachine.runStep lid =<< getPriceAct input

-- Admin actions

adminAction :: Api.IsGovernAct a => Types.LendexId -> a -> AdminContract ()
adminAction lid input = StateMachine.runStep lid =<< getGovernAct input

startLendex :: Types.LendexId -> Api.StartLendex -> AdminContract ()
startLendex lid (Api.StartLendex Types.StartParams {..}) =
  StateMachine.runInitialise lid (Types.initLendingPool (currencySymbol lid) sp'coins (fmap (Types.UserId . PaymentPubKeyHash) sp'admins) (fmap (Types.UserId . PaymentPubKeyHash) sp'oracles)) sp'initValue

-- Query actions

queryAction :: Api.IsQueryAct a => Types.LendexId -> a -> QueryContract ()
queryAction lid input = do
  (_, pool) <- findDatum lid :: QueryContract (Types.LendexId, Types.LendingPool)
  qAction pool =<< getQueryAct input
  where
    qAction :: Types.LendingPool -> Types.Act -> QueryContract ()
    qAction pool act = Contract.tell $ buildLog pool act

    -- Builds the Log by running a State Machine
    buildLog :: Types.LendingPool -> Types.Act -> QueryResult
    buildLog pool action = either (const Nothing) fst $ runStateT (React.qReact action) pool

queryAllLendexes :: Types.LendexId -> Api.QueryAllLendexes -> QueryContract ()
queryAllLendexes lid (Api.QueryAllLendexes spm) = do
  utxos <- Contract.utxosAt $ StateMachine.lendexAddress lid
  Contract.tell . Just . Last . Types.QueryResAllLendexes . mapMaybe f . Map.elems $ utxos
  pure ()
  where
    startedWith :: Types.LendingPool -> Types.StartParams -> Maybe Types.LendingPool
    startedWith lp@Types.LendingPool {..} Types.StartParams {..} = do
      guard (map (UserId . PaymentPubKeyHash) sp'admins == lp'admins)
      guard (map (UserId . PaymentPubKeyHash) sp'oracles == lp'trustedOracles)
      -- unsure if we can check that the tokens in StartParams are still being dealt in
      -- there is no 100% certainty since AddReserve can add new Coin types
      -- todo: we could check that the Coins is SartParams are a subset of the ones being dealt in now?
      pure lp

    f :: ChainIndexTxOut -> Maybe (Address, Types.LendingPool)
    f o = do
      let add = o ^. ciTxOutAddress
      (dat :: (Types.LendexId, Types.LendingPool)) <- readDatum' o
      lp <- startedWith (snd dat) spm
      pure (add, lp)

querySupportedCurrencies :: Types.LendexId -> QueryContract ()
querySupportedCurrencies lid = do
  (_, pool) <- findInputStateData lid :: QueryContract (Types.LendexId, Types.LendingPool)
  tellResult . getSupportedCurrencies $ pool
  where
    getSupportedCurrencies :: Types.LendingPool -> [Types.SupportedCurrency]
    getSupportedCurrencies Types.LendingPool {lp'reserves} =
      fmap toSupportedCurrency (M.toList lp'reserves)

    toSupportedCurrency (coin, Types.Reserve {reserve'aToken, reserve'rate}) =
      Types.SupportedCurrency coin reserve'aToken reserve'rate

    tellResult = Contract.tell . Just . Last . Types.QueryResSupportedCurrencies

----------------------------------------------------------
-- to act conversion

-- | Converts endpoint inputs to logic actions
getUserAct :: Api.IsUserAct a => a -> UserContract Types.Act
getUserAct act = do
  uid <- ownUserId
  t <- getCurrentTime
  pure $ Types.UserAct t uid $ Api.toUserAct act

-- | Converts endpoint inputs to logic actions
getPriceAct :: Api.IsPriceAct a => a -> OracleContract Types.Act
getPriceAct act = do
  uid <- ownUserId
  t <- getCurrentTime
  pure $ Types.PriceAct t uid $ Api.toPriceAct act

getGovernAct :: Api.IsGovernAct a => a -> AdminContract Types.Act
getGovernAct act = do
  uid <- ownUserId
  pure $ Types.GovernAct uid $ Api.toGovernAct act

getQueryAct :: Api.IsQueryAct a => a -> QueryContract Types.Act
getQueryAct act = do
  uid <- ownUserId
  t <- getCurrentTime
  pure $ Types.QueryAct uid t $ Api.toQueryAct act

getCurrentTime :: Contract.AsContractError e => Contract.Contract w s e Integer
getCurrentTime = getSlot <$> Contract.currentSlot

----------------------------------------------------------

findInputStateDatum :: Types.LendexId -> UserContract Datum
findInputStateDatum = findInputStateData

findInputStateData :: FromData d => Types.LendexId -> Contract.Contract w s StateMachine.LendexError d
findInputStateData lid = do
  txOuts <- Map.elems <$> Contract.utxosAt (StateMachine.lendexAddress lid)
  maybe err pure $ firstJust readDatum' txOuts
  where
    err = Contract.throwError $ StateMachine.toLendexError "Can not find Lending app instance"

-- | todo: add a unique NFT to distinguish between utxos / review logic.
findDatum :: FromData d => Types.LendexId -> Contract.Contract w s StateMachine.LendexError (Types.LendexId, d)
findDatum lid = do
  txOuts <- filterTxOuts . Map.toList <$> Request.utxosTxOutTxAt (StateMachine.lendexAddress lid)
  case txOuts of
    [(_, [x])] -> maybe err return $ (lid,) <$> x -- only passes if there is only 1 datum instance.
    _ -> err
  where
    err = Contract.throwError . StateMachine.toLendexError $ "Cannot establish correct Lending app instance."
    filterTxOuts = filter ((== 1) . length . filter isNotNothing . snd) . fmap (second $ readChainIndexTxDatum . snd)
    isNotNothing = \case
      Nothing -> False
      Just _ -> True
