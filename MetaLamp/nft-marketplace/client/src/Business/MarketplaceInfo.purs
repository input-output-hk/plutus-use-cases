module Business.MarketplaceInfo where

import Ext.Plutus.Contracts.Auction
import Plutus.Contracts.NftMarketplace.OffChain.ID
import Plutus.Contracts.NftMarketplace.OffChain.Info
import Plutus.Contracts.NftMarketplace.OnChain.Core.StateMachine
import Prelude
import Business.Marketplace (getMarketplaceContractId, getMarketplaceResponseWith)
import Capability.Contract (ContractId, ContractUnit(..), Endpoint(..))
import Capability.PollContract (class PollContract, PollError)
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Json.JsonTuple (JsonTuple)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap)
import Plutus.PAB.Simulation (MarketplaceContracts, _MarketplaceInfo)
import Plutus.PAB.Webserver.Types (ContractInstanceClientState)
import Plutus.V1.Ledger.Crypto (PubKeyHash)
import Plutus.V1.Ledger.Value (AssetClass, Value)
import PlutusTx.AssocMap (Map)

newtype InfoContractId
  = InfoContractId ContractId

derive instance newtypeInfoContractId :: Newtype InfoContractId _

derive instance genericInfoContractId :: Generic InfoContractId _

instance showInfoContractId :: Show InfoContractId where
  show = genericShow

getInfoContractId :: ContractInstanceClientState MarketplaceContracts -> Maybe InfoContractId
getInfoContractId = map InfoContractId <<< getMarketplaceContractId _MarketplaceInfo

fundsAt :: forall m. PollContract m => InfoContractId -> PubKeyHash -> m (Either PollError Value)
fundsAt = getMarketplaceResponseWith (Endpoint "fundsAt") _FundsAt <<< unwrap

marketplaceFunds :: forall m. PollContract m => InfoContractId -> m (Either PollError Value)
marketplaceFunds cid = getMarketplaceResponseWith (Endpoint "marketplaceFunds") _MarketplaceFunds (unwrap cid) ContractUnit

marketplaceStore :: forall m. PollContract m => InfoContractId -> m (Either PollError MarketplaceDatum)
marketplaceStore cid = getMarketplaceResponseWith (Endpoint "marketplaceStore") _MarketplaceStore (unwrap cid) ContractUnit

auctionState :: forall m. PollContract m => InfoContractId -> UserItemId -> m (Either PollError AuctionState)
auctionState = getMarketplaceResponseWith (Endpoint "getAuctionState") _AuctionState <<< unwrap
