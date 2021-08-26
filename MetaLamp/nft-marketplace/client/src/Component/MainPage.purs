module Component.MainPage where

import Data.Route
import Data.Unit
import Prelude
import Business.Marketplace as Marketplace
import Business.MarketplaceInfo as MarketplaceInfo
import Capability.LogMessages (class LogMessages, logInfo)
import Capability.Navigate (class Navigate, navigate)
import Capability.PollContract (class PollContract)
import Component.MarketPage as Market
import Component.UserPage as User
import Component.Utils (runRD)
import Component.WalletSelector as WalletSelector
import Control.Monad.Except (lift, runExceptT, throwError)
import Control.Parallel (parTraverse)
import Data.Array (catMaybes, findMap, groupBy, mapWithIndex, take)
import Data.Array.NonEmpty as NEA
import Data.BigInteger (BigInteger)
import Data.Either (either, hush)
import Data.Json.JsonTuple (JsonTuple(..))
import Data.Lens (Lens')
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties (classes)
import Network.RemoteData (RemoteData(..))
import Network.RemoteData as RD
import Network.RemoteData as RemoteData
import Plutus.PAB.Simulation (MarketplaceContracts)
import Plutus.PAB.Webserver.Types (ContractInstanceClientState)
import Plutus.V1.Ledger.Crypto (PubKeyHash)
import Plutus.V1.Ledger.Value (AssetClass, Value)
import PlutusTx.AssocMap as Map
import Routing.Duplex as Routing
import Routing.Hash as Routing
import Utils.BEM as BEM
import View.RemoteDataState (remoteDataState)
import Web.Event.Event (preventDefault)
import Web.UIEvent.MouseEvent (MouseEvent, toEvent)
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE

type State
  = { route :: Maybe Route
    }

data Query a
  = Navigate Route a

type Slots
  = ( walletSelector :: WalletSelector.WalletSelectorSlot Unit
    , userPage :: User.Slot Unit
    , marketPage :: Market.Slot Unit
    )

data Action
  = Initialize
  | GoTo Route MouseEvent
  | ChooseWallet WalletSelector.Output

component ::
  forall m input output.
  MonadEffect m =>
  Navigate m =>
  LogMessages m =>
  PollContract m =>
  H.Component HH.HTML Query input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval:
        H.mkEval
          $ H.defaultEval
              { handleAction = handleAction
              , handleQuery = handleQuery
              , initialize = Just Initialize
              }
    }
  where
  initialState :: input -> State
  initialState _ = { route: Nothing }

  render :: State -> H.ComponentHTML Action Slots m
  render st =
    HH.div_
      [ HH.text "Choose wallet: "
      , HH.slot WalletSelector._walletSelector unit WalletSelector.component unit (Just <<< ChooseWallet)
      , pages st
      ]

  handleAction :: Action -> H.HalogenM State Action Slots output m Unit
  handleAction = case _ of
    Initialize -> do
      initialRoute <- hush <<< (Routing.parse routeCodec) <$> H.liftEffect Routing.getHash
      navigate $ fromMaybe UserPage initialRoute
    GoTo route e -> do
      H.liftEffect $ preventDefault (toEvent e)
      oldRoute <- H.gets _.route
      when (oldRoute /= Just route) $ navigate route
    ChooseWallet (WalletSelector.Submit w) -> do
      logInfo $ show w
      pure unit

  handleQuery :: forall a. Query a -> H.HalogenM State Action Slots output m (Maybe a)
  handleQuery = case _ of
    Navigate route a -> do
      oldRoute <- H.gets _.route
      when (oldRoute /= Just route)
        $ H.modify_ _ { route = Just route }
      pure (Just a)

pages :: forall m. State -> H.ComponentHTML Action Slots m
pages st =
  navbar
    $ case st.route of
        Nothing -> HH.h1_ [ HH.text "Page wasn't found" ]
        Just route -> case route of
          UserPage -> HH.slot User._userPage unit User.component unit absurd
          MarketPage -> HH.slot Market._marketPage unit Market.component unit absurd

navbar :: forall w. HH.HTML w Action -> HH.HTML w Action
navbar html =
  HH.div_
    [ HH.ul_
        [ HH.li_
            [ HH.a
                [ HP.href "#"
                , HE.onClick (Just <<< GoTo UserPage)
                ]
                [ HH.text "Personal" ]
            ]
        , HH.li_
            [ HH.a
                [ HP.href "#"
                , HE.onClick (Just <<< GoTo MarketPage)
                ]
                [ HH.text "Market" ]
            ]
        ]
    , html
    ]
