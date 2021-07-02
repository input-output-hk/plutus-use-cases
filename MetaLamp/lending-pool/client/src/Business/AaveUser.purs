module Business.AaveUser where

import Prelude
import Business.Aave (getAaveContractId, getAaveResponseWith)
import Capability.Contract (ContractId, ContractUnit(..), Endpoint(..))
import Capability.PollContract (class PollContract, PollError)
import Data.Either (Either)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap)
import Plutus.Contracts.LendingPool.OffChain.User (BorrowParams, ProvideCollateralParams, RevokeCollateralParams, DepositParams, RepayParams, WithdrawParams, _Borrowed, _Deposited, _GetPubKey, _GetPubKeyBalance, _Repaid, _Withdrawn, _CollateralProvided, _CollateralRevoked)
import Plutus.PAB.Simulation (AaveContracts, _AaveUser)
import Plutus.PAB.Webserver.Types (ContractInstanceClientState)
import Plutus.V1.Ledger.Crypto (PubKeyHash)
import Plutus.V1.Ledger.Value (Value)

newtype UserContractId
  = UserContractId ContractId

derive instance newtypeUserContractId :: Newtype UserContractId _

getUserContractId :: ContractInstanceClientState AaveContracts -> Maybe UserContractId
getUserContractId = map UserContractId <<< getAaveContractId _AaveUser

deposit :: forall m. PollContract m => UserContractId -> DepositParams -> m (Either PollError Unit)
deposit = getAaveResponseWith (Endpoint "deposit") _Deposited <<< unwrap

withdraw :: forall m. PollContract m => UserContractId -> WithdrawParams -> m (Either PollError Unit)
withdraw = getAaveResponseWith (Endpoint "withdraw") _Withdrawn <<< unwrap

borrow :: forall m. PollContract m => UserContractId -> BorrowParams -> m (Either PollError Unit)
borrow = getAaveResponseWith (Endpoint "borrow") _Borrowed <<< unwrap

repay :: forall m. PollContract m => UserContractId -> RepayParams -> m (Either PollError Unit)
repay = getAaveResponseWith (Endpoint "repay") _Repaid <<< unwrap

provideCollateral :: forall m. PollContract m => UserContractId -> ProvideCollateralParams -> m (Either PollError Unit)
provideCollateral = getAaveResponseWith (Endpoint "provideCollateral") _CollateralProvided <<< unwrap

revokeCollateral :: forall m. PollContract m => UserContractId -> RevokeCollateralParams -> m (Either PollError Unit)
revokeCollateral = getAaveResponseWith (Endpoint "revokeCollateral") _CollateralRevoked <<< unwrap

ownPubKey :: forall m. PollContract m => UserContractId -> m (Either PollError PubKeyHash)
ownPubKey cid = getAaveResponseWith (Endpoint "ownPubKey") _GetPubKey (unwrap cid) ContractUnit

ownPubKeyBalance :: forall m. PollContract m => UserContractId -> m (Either PollError Value)
ownPubKeyBalance cid = getAaveResponseWith (Endpoint "ownPubKeyBalance") _GetPubKeyBalance (unwrap cid) ContractUnit
