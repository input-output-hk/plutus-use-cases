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
import Data.Newtype (unwrap)
import Data.Symbol (SProxy(..))
import Data.UserInstance (UserInstance)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Network.RemoteData (RemoteData(..))
import Plutus.Contracts.NftMarketplace.OffChain.User (BidOnAuctionParams(..), CloseLotParams(..)) as MarketplaceUser
import Plutus.Contracts.NftMarketplace.OnChain.Core.StateMachine (MarketplaceDatum)
import View.NFT (renderAuction, renderNftSingletonLots, renderSale, renderNftBundleLots)

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
  | CloseSale Datum.ItemLot
  | BuyItem Datum.ItemLot
  | CompleteAuction Datum.ItemLot
  | BidOnAuction Datum.ItemLot BidOnAuctionForm.BidOutput

type Slots
  = ( bidOnAuctionForm :: BidOnAuctionForm.Slot Datum.Item
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
      , renderNftSingletonLots st.marketplaceState (renderLot st <<< Left)
      , HH.h3_ [ HH.text "Market NFT bundles: " ]
      , renderNftBundleLots st.marketplaceState (renderLot st <<< Right)
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
              { clpItemId: Datum.getItemId $ Datum.getItem r
              }
      logInfo $ "Marketplace sale closed: " <> show resp
      handleAction Initialize
    BuyItem r -> do
      contractId <- H.gets _.userInstance.userContract
      resp <-
        MarketplaceUser.buyItem contractId
          $ MarketplaceUser.CloseLotParams
              { clpItemId: Datum.getItemId $ Datum.getItem r
              }
      logInfo $ "Marketplace item bought: " <> show resp
      handleAction Initialize
    CompleteAuction r -> do
      contractId <- H.gets _.userInstance.userContract
      resp <-
        MarketplaceUser.completeAnAuction contractId
          $ MarketplaceUser.CloseLotParams
              { clpItemId: Datum.getItemId $ Datum.getItem r
              }
      logInfo $ "Marketplace auction complete: " <> show resp
      handleAction Initialize
    BidOnAuction r o -> do
      contractId <- H.gets _.userInstance.userContract
      resp <-
        MarketplaceUser.bidOnAuction contractId
          $ MarketplaceUser.BidOnAuctionParams
              { boapItemId: Datum.getItemId $ Datum.getItem r
              , boapBid: Utils.mkAdaFromInt o.bid
              }
      logInfo $ "Marketplace auction bid submitted: " <> show resp
      handleAction Initialize

renderLot ::
  forall m.
  MonadAff m =>
  State -> Datum.ItemLot -> H.ComponentHTML Action Slots m
renderLot st r = case Datum.getLot r of
  Right auction ->
    HH.div_
      [ renderAuction auction
      , HH.button
          [ HE.onClick \_ -> Just (CompleteAuction r) ]
          [ HH.text "Complete Auction" ]
      , HH.slot (SProxy :: _ "bidOnAuctionForm") (Datum.getItem r) BidOnAuctionForm.component unit (Just <<< BidOnAuction r)
      ]
  Left sale ->
    HH.div_
      [ renderSale sale
      , if (unwrap sale).saleOwner == st.userInstance.userPubKey then
          HH.button
            [ HE.onClick \_ -> Just (CloseSale r) ]
            [ HH.text "Close Sale" ]
        else
          HH.button
            [ HE.onClick \_ -> Just (BuyItem r) ]
            [ HH.text "Buy Item" ]
      ]
