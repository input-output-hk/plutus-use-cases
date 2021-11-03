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
import Plutus.Contracts.NftMarketplace.OffChain.User
import Plutus.PAB.MarketplaceContracts (MarketplaceContracts, _MarketplaceUser)
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

createNft :: forall m. PollContract m => UserContractId -> CreateNftParams -> m (Either PollError Unit)
createNft = getMarketplaceResponseWith (Endpoint "createNft") _NftCreated <<< unwrap

openSale :: forall m. PollContract m => UserContractId -> OpenSaleParams -> m (Either PollError Unit)
openSale = getMarketplaceResponseWith (Endpoint "openSale") _OpenedSale <<< unwrap

buyItem :: forall m. PollContract m => UserContractId -> CloseLotParams -> m (Either PollError Unit)
buyItem = getMarketplaceResponseWith (Endpoint "buyItem") _NftBought <<< unwrap

closeSale :: forall m. PollContract m => UserContractId -> CloseLotParams -> m (Either PollError Unit)
closeSale = getMarketplaceResponseWith (Endpoint "closeSale") _ClosedSale <<< unwrap

startAnAuction :: forall m. PollContract m => UserContractId -> StartAnAuctionParams -> m (Either PollError Unit)
startAnAuction = getMarketplaceResponseWith (Endpoint "startAnAuction") _AuctionStarted <<< unwrap

completeAnAuction :: forall m. PollContract m => UserContractId -> CloseLotParams -> m (Either PollError Unit)
completeAnAuction = getMarketplaceResponseWith (Endpoint "completeAnAuction") _AuctionComplete <<< unwrap

bidOnAuction :: forall m. PollContract m => UserContractId -> BidOnAuctionParams -> m (Either PollError Unit)
bidOnAuction = getMarketplaceResponseWith (Endpoint "bidOnAuction") _BidSubmitted <<< unwrap

bundleUp :: forall m. PollContract m => UserContractId -> BundleUpParams -> m (Either PollError Unit)
bundleUp = getMarketplaceResponseWith (Endpoint "bundleUp") _Bundled <<< unwrap

unbundle :: forall m. PollContract m => UserContractId -> UnbundleParams -> m (Either PollError Unit)
unbundle = getMarketplaceResponseWith (Endpoint "unbundle") _Unbundled <<< unwrap
