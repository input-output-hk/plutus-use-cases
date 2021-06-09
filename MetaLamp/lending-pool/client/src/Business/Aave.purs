module Business.Aave where

import Prelude
import Capability.Contract (class Contract, ContractId, APIError, ContractUnit(..), Endpoint(..), getContracts)
import Capability.PollContract (class PollContract, LeftPoll(..), PollError, PollResponse, pollEndpoint)
import Control.Monad.Except (runExcept, throwError, withExcept)
import Data.Either (Either, either)
import Data.Json.JsonTuple (JsonTuple)
import Data.Lens (Prism', preview)
import Data.Maybe (Maybe(..), maybe)
import Data.RawJson (RawJson(..))
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

getAaveResponseWith ::
  forall m a p.
  PollContract m =>
  Encode p =>
  Endpoint ->
  Prism' UserContractState a ->
  ContractId ->
  p ->
  m (Either PollError a)
getAaveResponseWith endpoint pick cid param = pollEndpoint getNext endpoint param cid
  where
  getNext :: ContractInstanceClientState AaveContracts -> PollResponse a
  getNext (ContractInstanceClientState { cicCurrentState: PartiallyDecodedResponse { observableState: RawJson s } }) =
    runExcept
      $ do
          (response :: Either String UserContractState) <- withExcept (ResponseError <<< show) (decodeJSON s)
          state <- either (throwError <<< ResponseError <<< show) pure response
          case (preview _Pending state) of
            Just _ -> throwError Continue
            Nothing ->
              maybe
                (throwError <<< ResponseError $ "Invalid state: " <> (show state))
                pure
                (preview pick state)

deposit :: forall m. PollContract m => ContractId -> DepositParams -> m (Either PollError Unit)
deposit = getAaveResponseWith (Endpoint "deposit") _Deposited

withdraw :: forall m. PollContract m => ContractId -> WithdrawParams -> m (Either PollError Unit)
withdraw = getAaveResponseWith (Endpoint "withdraw") _Withdrawn

borrow :: forall m. PollContract m => ContractId -> BorrowParams -> m (Either PollError Unit)
borrow = getAaveResponseWith (Endpoint "borrow") _Borrowed

repay :: forall m. PollContract m => ContractId -> RepayParams -> m (Either PollError Unit)
repay = getAaveResponseWith (Endpoint "repay") _Repaid

fundsAt :: forall m. PollContract m => ContractId -> PubKeyHash -> m (Either PollError Value)
fundsAt = getAaveResponseWith (Endpoint "fundsAt") _FundsAt

poolFunds :: forall m. PollContract m => ContractId -> m (Either PollError Value)
poolFunds cid = getAaveResponseWith (Endpoint "poolFunds") _PoolFunds cid ContractUnit

reserves :: forall m. PollContract m => ContractId -> m (Either PollError (Map AssetClass Reserve))
reserves cid = getAaveResponseWith (Endpoint "reserves") _Reserves cid ContractUnit

users :: forall m. PollContract m => ContractId -> m (Either PollError (Map (JsonTuple AssetClass PubKeyHash) UserConfig))
users cid = getAaveResponseWith (Endpoint "users") _Users cid ContractUnit

ownPubKey :: forall m. PollContract m => ContractId -> m (Either PollError PubKeyHash)
ownPubKey cid = getAaveResponseWith (Endpoint "ownPubKey") _GetPubKey cid ContractUnit
