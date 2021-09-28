module Business.AaveInfo where

import Prelude
import Business.Aave (getAaveContractId, getAaveResponseWith)
import Capability.Contract (ContractId, ContractUnit(..), Endpoint(..))
import Capability.PollContract (class PollContract, PollError)
import Data.Either (Either)
import Data.Json.JsonTuple (JsonTuple)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap)
import Plutus.Contracts.LendingPool.OnChain.Core.Script (Reserve, UserConfig)
import Plutus.Contracts.LendingPool.OffChain.Info (_FundsAt, _PoolFunds, _Reserves, _Users)
import Plutus.PAB.Simulation (AaveContracts, _AaveInfo)
import Plutus.PAB.Webserver.Types (ContractInstanceClientState)
import Plutus.V1.Ledger.Crypto (PubKeyHash)
import Plutus.V1.Ledger.Value (AssetClass, Value)
import PlutusTx.AssocMap (Map)

newtype InfoContractId
  = InfoContractId ContractId

derive instance newtypeInfoContractId :: Newtype InfoContractId _

getInfoContractId :: ContractInstanceClientState AaveContracts -> Maybe InfoContractId
getInfoContractId = map InfoContractId <<< getAaveContractId _AaveInfo

fundsAt :: forall m. PollContract m => InfoContractId -> PubKeyHash -> m (Either PollError Value)
fundsAt = getAaveResponseWith (Endpoint "fundsAt") _FundsAt <<< unwrap

poolFunds :: forall m. PollContract m => InfoContractId -> m (Either PollError Value)
poolFunds cid = getAaveResponseWith (Endpoint "poolFunds") _PoolFunds (unwrap cid) ContractUnit

reserves :: forall m. PollContract m => InfoContractId -> m (Either PollError (Map AssetClass Reserve))
reserves cid = getAaveResponseWith (Endpoint "reserves") _Reserves (unwrap cid) ContractUnit

users :: forall m. PollContract m => InfoContractId -> m (Either PollError (Map (JsonTuple AssetClass PubKeyHash) UserConfig))
users cid = getAaveResponseWith (Endpoint "users") _Users (unwrap cid) ContractUnit
