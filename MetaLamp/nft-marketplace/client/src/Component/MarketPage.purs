module Component.MarketPage where

import Prelude
import Business.MarketplaceInfo (InfoContractId)
import Business.MarketplaceInfo as MarketplaceInfo
import Capability.IPFS as IPFS
import Capability.LogMessages (class LogMessages, logInfo)
import Capability.PollContract (class PollContract)
import Component.Utils (PageInput, runRD)
import Data.Bifunctor (lmap)
import Data.Lens (Lens')
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.UserInstance (UserInstance)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Network.RemoteData (RemoteData(..))
import Plutus.Contracts.NftMarketplace.OnChain.Core.StateMachine (MarketplaceDatum)

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
  render _ = HH.h1_ [ HH.text "MarketPage" ]

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
