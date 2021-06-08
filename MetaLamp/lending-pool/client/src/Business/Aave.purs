module Business.Aave where

import Prelude

import Capability.Contract (class Contract, APIError(..), ContractId, ContractUnit(..), Endpoint(..), callEndpoint, getContractStatus, getContracts)
import Capability.Delay (class Delay, delay)
import Control.Monad.Except (runExcept, runExceptT, throwError)
import Control.Monad.Trans.Class (lift)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), either)
import Data.Json.JsonTuple (JsonTuple)
import Data.Lens (Prism', preview)
import Data.Maybe (Maybe(..))
import Data.RawJson (RawJson(..))
import Data.Time.Duration (Milliseconds(..))
import Foreign.Generic (class Encode, decodeJSON)
import Plutus.Contracts.Core (Reserve, UserConfig)
import Plutus.Contracts.Endpoints (BorrowParams, DepositParams, RepayParams, UserContractState, WithdrawParams, _Borrowed, _Deposited, _FundsAt, _GetPubKey, _Pending, _PoolFunds, _Repaid, _Reserves, _Users, _Withdrawn)
import Plutus.PAB.Events.ContractInstanceState (PartiallyDecodedResponse(..))
import Plutus.PAB.Simulation (AaveContracts)
import Plutus.PAB.Webserver.Types (ContractInstanceClientState(..))
import Plutus.V1.Ledger.Crypto (PubKeyHash)
import Plutus.V1.Ledger.Value (AssetClass, Value)
import PlutusTx.AssocMap (Map)

getAaveContracts :: forall m. Contract m => m (Either APIError (Array (ContractInstanceClientState AaveContracts)))
getAaveContracts = getContracts

getAaveContractStatus :: forall m. Contract m => ContractId -> m (Either APIError (ContractInstanceClientState AaveContracts))
getAaveContractStatus = getContractStatus

getAaveContractResponse :: forall m. Contract m => ContractId -> m (Either APIError UserContractState)
getAaveContractResponse = map (_ >>= getAaveResponse) <<< getAaveContractStatus

getAaveResponse :: ContractInstanceClientState AaveContracts -> Either APIError UserContractState
getAaveResponse (ContractInstanceClientState { cicCurrentState: PartiallyDecodedResponse { observableState: RawJson s } }) = do
  (res:: Either String UserContractState) <- lmap (AjaxCallError <<< show) <<< runExcept <<< decodeJSON $ s
  case res of
    Left e -> Left <<< AjaxCallError $ e
    Right r -> Right r

getAaveResponseWith :: forall m a p.
  Contract m =>
  Delay m =>
  Encode p =>
  Endpoint ->
  Prism' UserContractState a ->
  ContractId ->
  p ->
  m (Either APIError a)
getAaveResponseWith endpoint pick cid param =
  callEndpoint endpoint cid param >>=
    either (pure <<< Left) (const $ pollStatus endpoint pick cid)

pollStatus :: forall m a.
  Contract m =>
  Delay m =>
  Endpoint ->
  Prism' UserContractState a ->
  ContractId ->
  m (Either APIError a)
pollStatus endpoint pick cid = runExceptT $ do
  _ <- lift <<< delay <<< Milliseconds $ 300.0
  res <- lift (getAaveContractResponse cid) >>= either throwError pure
  case (preview _Pending res) of
    Just _ -> lift (pollStatus endpoint pick cid) >>= either throwError pure
    Nothing ->
      case (preview pick res) of
        Just v -> pure v
        Nothing -> throwError $ AjaxCallError $ "Invalid state: " <> (show res)

deposit :: forall m. Contract m => Delay m => ContractId -> DepositParams -> m (Either APIError Unit)
deposit = getAaveResponseWith (Endpoint "deposit") _Deposited

withdraw :: forall m. Contract m => Delay m => ContractId -> WithdrawParams -> m (Either APIError Unit)
withdraw = getAaveResponseWith (Endpoint "withdraw") _Withdrawn

borrow :: forall m. Contract m => Delay m => ContractId -> BorrowParams -> m (Either APIError Unit)
borrow = getAaveResponseWith (Endpoint "borrow") _Borrowed

repay :: forall m. Contract m => Delay m => ContractId -> RepayParams -> m (Either APIError Unit)
repay = getAaveResponseWith (Endpoint "repay") _Repaid

fundsAt :: forall m. Contract m => Delay m => ContractId -> PubKeyHash -> m (Either APIError Value)
fundsAt = getAaveResponseWith (Endpoint "fundsAt") _FundsAt

poolFunds :: forall m. Contract m => Delay m => ContractId -> m (Either APIError Value)
poolFunds cid = getAaveResponseWith (Endpoint "poolFunds") _PoolFunds cid ContractUnit

reserves :: forall m. Contract m => Delay m => ContractId -> m (Either APIError (Map AssetClass Reserve))
reserves cid = getAaveResponseWith (Endpoint "reserves") _Reserves cid ContractUnit

users :: forall m. Contract m => Delay m => ContractId -> m (Either APIError (Map (JsonTuple AssetClass PubKeyHash) UserConfig))
users cid = getAaveResponseWith (Endpoint "users") _Users cid ContractUnit

ownPubKey :: forall m. Contract m => Delay m => ContractId -> m (Either APIError PubKeyHash)
ownPubKey cid = getAaveResponseWith (Endpoint "ownPubKey") _GetPubKey cid ContractUnit
