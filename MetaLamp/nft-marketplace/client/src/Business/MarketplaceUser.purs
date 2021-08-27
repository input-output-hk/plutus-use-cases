module Business.MarketplaceUser where

import Prelude
import Business.Marketplace (getMarketplaceContractId, getMarketplaceResponseWith)
import Capability.Contract (ContractId, ContractUnit(..), Endpoint(..))
import Capability.PollContract (class PollContract, PollError)
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap)
import Plutus.Contracts.NftMarketplace.OffChain.User (_GetPubKey, _GetPubKeyBalance)
import Plutus.PAB.Simulation (MarketplaceContracts, _MarketplaceUser)
import Plutus.PAB.Webserver.Types (ContractInstanceClientState)
import Plutus.V1.Ledger.Crypto (PubKeyHash)
import Plutus.V1.Ledger.Value (Value)

newtype UserContractId
  = UserContractId ContractId

derive instance newtypeUserContractId :: Newtype UserContractId _

derive instance genericUserContractId :: Generic UserContractId _

instance showUserContractId :: Show UserContractId where
  show = genericShow

getUserContractId :: ContractInstanceClientState MarketplaceContracts -> Maybe UserContractId
getUserContractId = map UserContractId <<< getMarketplaceContractId _MarketplaceUser

ownPubKey :: forall m. PollContract m => UserContractId -> m (Either PollError PubKeyHash)
ownPubKey cid = getMarketplaceResponseWith (Endpoint "ownPubKey") _GetPubKey (unwrap cid) ContractUnit

ownPubKeyBalance :: forall m. PollContract m => UserContractId -> m (Either PollError Value)
ownPubKeyBalance cid = getMarketplaceResponseWith (Endpoint "ownPubKeyBalance") _GetPubKeyBalance (unwrap cid) ContractUnit
