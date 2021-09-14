module Component.MarketPage where

import Prelude
import Business.Datum as Datum
import Business.MarketplaceInfo (InfoContractId)
import Business.MarketplaceInfo as MarketplaceInfo
import Business.MarketplaceUser (buyItem, closeSale) as MarketplaceUser
import Capability.IPFS as IPFS
import Capability.LogMessages (class LogMessages, logInfo)
import Capability.PollContract (class PollContract)
import Component.Utils (PageInput, runRD)
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
import Plutus.Contracts.NftMarketplace.OffChain.User (CloseLotParams(..)) as MarketplaceUser
import Plutus.Contracts.NftMarketplace.OnChain.Core.StateMachine (MarketplaceDatum)
import View.NftSingletons (renderNftSingletonLots, renderSale)

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

  render :: State -> H.ComponentHTML Action () m
  render st =
    HH.div_
      [ HH.h3_ [ HH.text "Market NFT singletons: " ]
      , renderNftSingletonLots st.marketplaceState renderLot
      ]

  handleAction :: Action -> H.HalogenM State Action () output m Unit
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

renderLot ::
  forall props.
  Datum.NftSingletonLot -> HH.HTML props Action
renderLot r = case r.lot of
  Right auction -> HH.div_ []
  Left sale ->
    HH.div_
      [ renderSale sale
      , HH.button
          [ HE.onClick \_ -> Just (CloseSale r) ]
          [ HH.text "Close Sale" ]
      , HH.button
          [ HE.onClick \_ -> Just (BuyNft r) ]
          [ HH.text "Buy Nft" ]
      ]
