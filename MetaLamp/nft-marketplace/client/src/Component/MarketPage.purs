module Component.MarketPage where

import Prelude
import Business.Datum as Datum
import Business.MarketplaceInfo (InfoContractId)
import Business.MarketplaceInfo as MarketplaceInfo
import Business.MarketplaceUser (bidOnAuction, buyItem, closeSale, completeAnAuction) as MarketplaceUser
import Capability.IPFS as IPFS
import Capability.LogMessages (class LogMessages, logInfo)
import Capability.PollContract (class PollContract)
import Component.BidOnAuctionForm as BidOnAuctionForm
import Component.Utils (PageInput, runRD)
import Component.Utils as Utils
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Lens (Lens')
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.UserInstance (UserInstance)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Network.RemoteData (RemoteData(..))
import Plutus.Contracts.NftMarketplace.OffChain.ID (UserItemId(..))
import Plutus.Contracts.NftMarketplace.OffChain.User (BidOnAuctionParams(..), CloseLotParams(..)) as MarketplaceUser
import Plutus.Contracts.NftMarketplace.OnChain.Core.StateMachine (MarketplaceDatum)
import View.NftSingletons (renderAuction, renderNftSingletonLots, renderSale)

type Slot id
  = forall query. H.Slot query Void id

_marketPage :: SProxy "marketPage"
_marketPage = SProxy

type State
  = { userInstance :: UserInstance
    , infoInstance :: InfoContractId
    , marketplaceState :: RemoteData String MarketplaceDatum
    }

_marketplaceState :: Lens' State (RemoteData String MarketplaceDatum)
_marketplaceState = prop (SProxy :: SProxy "marketplaceState")

data Action
  = Initialize
  | Reinitialize PageInput
  | GetMarketplaceState
  | CloseSale Datum.NftSingletonLot
  | BuyNft Datum.NftSingletonLot
  | CompleteAuction Datum.NftSingletonLot
  | BidOnAuction Datum.NftSingletonLot BidOnAuctionForm.BidOutput

type Slots
  = ( bidOnAuctionForm :: BidOnAuctionForm.Slot Datum.NftSingleton
    )

component ::
  forall query output m.
  LogMessages m =>
  IPFS.IPFS m =>
  PollContract m =>
  MonadEffect m =>
  MonadAff m =>
  H.Component HH.HTML query PageInput output m
component =
  H.mkComponent
    { initialState: initialState
    , render
    , eval:
        H.mkEval
          $ H.defaultEval
              { handleAction = handleAction
              , initialize = Just Initialize
              , receive = Just <<< Reinitialize
              }
    }
  where
  initialState :: PageInput -> State
  initialState i =
    { userInstance: i.userInstance
    , infoInstance: i.infoInstance
    , marketplaceState: NotAsked
    }

  render :: State -> H.ComponentHTML Action Slots m
  render st =
    HH.div_
      [ HH.h3_ [ HH.text "Market NFT singletons: " ]
      , renderNftSingletonLots st.marketplaceState renderLot
      ]

  handleAction :: Action -> H.HalogenM State Action Slots output m Unit
  handleAction = case _ of
    Initialize -> do
      handleAction GetMarketplaceState
      state <- H.get
      logInfo $ show state
    Reinitialize i -> do
      H.put $ initialState i
      handleAction Initialize
    GetMarketplaceState -> do
      state <- H.get
      runRD _marketplaceState $ map (lmap show)
        $ MarketplaceInfo.marketplaceStore state.infoInstance
    CloseSale r -> do
      contractId <- H.gets _.userInstance.userContract
      resp <-
        MarketplaceUser.closeSale contractId
          $ MarketplaceUser.CloseLotParams
              { clpItemId: UserNftId r.nft.ipfsCid
              }
      logInfo $ "Marketplace sale closed: " <> show resp
      handleAction Initialize
    BuyNft r -> do
      contractId <- H.gets _.userInstance.userContract
      resp <-
        MarketplaceUser.buyItem contractId
          $ MarketplaceUser.CloseLotParams
              { clpItemId: UserNftId r.nft.ipfsCid
              }
      logInfo $ "Marketplace nft bought: " <> show resp
      handleAction Initialize
    CompleteAuction r -> do
      contractId <- H.gets _.userInstance.userContract
      resp <-
        MarketplaceUser.completeAnAuction contractId
          $ MarketplaceUser.CloseLotParams
              { clpItemId: UserNftId r.nft.ipfsCid
              }
      logInfo $ "Marketplace auction complete: " <> show resp
      handleAction Initialize
    BidOnAuction r o -> do
      contractId <- H.gets _.userInstance.userContract
      resp <-
        MarketplaceUser.bidOnAuction contractId
          $ MarketplaceUser.BidOnAuctionParams
              { boapItemId: UserNftId r.nft.ipfsCid
              , boapBid: Utils.mkAdaFromInt o.bid
              }
      logInfo $ "Marketplace auction complete: " <> show resp
      handleAction Initialize

renderLot ::
  forall m.
  MonadAff m =>
  Datum.NftSingletonLot -> H.ComponentHTML Action Slots m
renderLot r = case r.lot of
  Right auction ->
    HH.div_
      [ renderAuction auction
      , HH.button
          [ HE.onClick \_ -> Just (CompleteAuction r) ]
          [ HH.text "Complete Auction" ]
      , HH.slot (SProxy :: _ "bidOnAuctionForm") r.nft BidOnAuctionForm.component unit (Just <<< BidOnAuction r)
      ]
  Left sale ->
    HH.div_
      [ renderSale sale
      , HH.button
          [ HE.onClick \_ -> Just (CloseSale r) ]
          [ HH.text "Close Sale" ]
      , HH.button
          [ HE.onClick \_ -> Just (BuyNft r) ]
          [ HH.text "Buy NFT" ]
      ]
