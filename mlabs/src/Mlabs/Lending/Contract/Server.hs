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

import Control.Monad (forever, guard)
import Control.Monad.State.Strict (runStateT)
import Data.List.Extra (firstJust)
import Data.Map qualified as Map (elems)
import Data.Semigroup (Last (..))
import Ledger.Constraints (mintingPolicy, mustIncludeDatum, ownPubKeyHash)
import Mlabs.Emulator.Types (UserId (..), ownUserId)
import Mlabs.Lending.Contract.Api qualified as Api
import Mlabs.Lending.Contract.Forge (currencyPolicy, currencySymbol)
import Mlabs.Lending.Contract.StateMachine qualified as StateMachine
import Mlabs.Lending.Logic.React qualified as React
import Mlabs.Lending.Logic.Types qualified as Types
import Mlabs.Plutus.Contract (getEndpoint, readDatum, selects)
import Plutus.Contract qualified as Contract
import Plutus.V1.Ledger.Api (Datum (..))
import Plutus.V1.Ledger.Crypto (pubKeyHash)
import Plutus.V1.Ledger.Slot (getSlot)
import Plutus.V1.Ledger.Tx
import PlutusTx (IsData)
import PlutusTx.AssocMap qualified as M
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
  forever $
    selects
      [ act $ getEndpoint @Api.Deposit
      , act $ getEndpoint @Api.Borrow
      , act $ getEndpoint @Api.Repay
      , act $ getEndpoint @Api.SwapBorrowRateModel
      , act $ getEndpoint @Api.AddCollateral
      , act $ getEndpoint @Api.RemoveCollateral
      , act $ getEndpoint @Api.Withdraw
      , act $ getEndpoint @Api.LiquidationCall
      ]
  where
    act :: Api.IsUserAct a => UserContract a -> UserContract ()
    act readInput = readInput >>= userAction lid

-- | Endpoints for price oracle
oracleEndpoints :: Types.LendexId -> OracleContract ()
oracleEndpoints lid =
  forever $
    selects
      [ act $ getEndpoint @Api.SetAssetPrice
      ]
  where
    act :: Api.IsPriceAct a => OracleContract a -> OracleContract ()
    act readInput = readInput >>= priceOracleAction lid

-- | Endpoints for admin
adminEndpoints :: Types.LendexId -> AdminContract ()
adminEndpoints lid = do
  getEndpoint @Api.StartLendex >>= startLendex lid
  forever $
    selects
      [ act $ getEndpoint @Api.AddReserve
      ]
  where
    act :: Api.IsGovernAct a => AdminContract a -> AdminContract ()
    act readInput = readInput >>= adminAction lid

{- | Endpoints for querrying Lendex state:
   * `QueryAllLendexes` - returns a list of `LendingPool` data associated with each available lendes
   * `QuerySupportedCurrencies` - returns the list of supported currencies (see `SupportedCurrency`) for current `LendingPool`
   * `QuerryCurrentBalance` - returns a list of all the users, together with their current balances.
-}
queryEndpoints :: Types.LendexId -> QueryContract ()
queryEndpoints lid =
  forever $
    selects
      [ getEndpoint @Api.QueryAllLendexes >>= queryAllLendexes lid
      , getEndpoint @Api.QuerySupportedCurrencies >> querySupportedCurrencies lid
      , act $ getEndpoint @Api.QueryCurrentBalance
      ]
  where
    act :: Api.IsQueryAct a => QueryContract a -> QueryContract ()
    act readInput = readInput >>= queryAction lid

-- user actions

userAction :: Api.IsUserAct a => Types.LendexId -> a -> UserContract ()
userAction lid input = do
  pkh <- pubKeyHash <$> Contract.ownPubKey
  act <- getUserAct input
  inputDatum <- findInputStateDatum lid
  let lookups =
        mintingPolicy (currencyPolicy lid)
          Hask.<> ownPubKeyHash pkh
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
  StateMachine.runInitialise lid (Types.initLendingPool (currencySymbol lid) sp'coins (fmap Types.UserId sp'admins) (fmap Types.UserId sp'oracles)) sp'initValue

-- Query actions

queryAction :: Api.IsQueryAct a => Types.LendexId -> a -> QueryContract ()
queryAction lid input = do
  (_, pool) <- findInputStateData lid :: QueryContract (Types.LendexId, Types.LendingPool)
  qAction pool =<< getQueryAct input
  where
    qAction :: Types.LendingPool -> Types.Act -> QueryContract ()
    qAction pool act = Contract.tell $ buildLog pool act

    -- Builds the Log by running a State Machine
    buildLog :: Types.LendingPool -> Types.Act -> QueryResult
    buildLog pool action = either (const Nothing) fst $ runStateT (React.qReact action) pool

queryAllLendexes :: Types.LendexId -> Api.QueryAllLendexes -> QueryContract ()
queryAllLendexes lid (Api.QueryAllLendexes spm) = do
  utxos <- Contract.utxoAt $ StateMachine.lendexAddress lid
  Contract.tell . Just . Last . Types.QueryResAllLendexes . mapMaybe f . Map.elems $ utxos
  pure ()
  where
    startedWith :: Types.LendingPool -> Types.StartParams -> Maybe Types.LendingPool
    startedWith lp@Types.LendingPool {..} Types.StartParams {..} = do
      guard (map UserId sp'admins == lp'admins)
      guard (map UserId sp'oracles == lp'trustedOracles)
      -- unsure if we can check that the tokens in StartParams are still being dealt in
      -- there is no 100% certainty since AddReserve can add new Coin types
      -- todo: we could check that the Coins is SartParams are a subset of the ones being dealt in now?
      pure lp

    f :: TxOutTx -> Maybe (Address, Types.LendingPool)
    f o = do
      let add = txOutAddress $ txOutTxOut o
      (dat :: (Types.LendexId, Types.LendingPool)) <- readDatum o
      lp <- startedWith (snd dat) spm
      pure (add, lp)

querySupportedCurrencies :: Types.LendexId -> QueryContract ()
querySupportedCurrencies lid = do
  (_, pool) <- findInputStateData lid :: QueryContract (Types.LendexId, Types.LendingPool)
  tellResult . getSupportedCurrencies $ pool
  where
    getSupportedCurrencies :: Types.LendingPool -> [Types.SupportedCurrency]
    getSupportedCurrencies lp =
      fmap
        (\(coin, rsrv) -> Types.SupportedCurrency coin rsrv.reserve'aToken rsrv.reserve'rate)
        (M.toList lp.lp'reserves)
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

findInputStateData :: IsData d => Types.LendexId -> Contract.Contract w s StateMachine.LendexError d
findInputStateData lid = do
  txOuts <- Map.elems <$> Contract.utxoAt (StateMachine.lendexAddress lid)
  maybe err pure $ firstJust readDatum txOuts
  where
    err = Contract.throwError $ StateMachine.toLendexError "Can not find Lending app instance"