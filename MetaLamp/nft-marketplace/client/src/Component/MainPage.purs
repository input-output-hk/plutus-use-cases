module Component.MainPage where

import Data.Route
import Data.Unit
import Prelude
import Business.Marketplace (getMarketplaceContracts)
import Business.Marketplace as Marketplace
import Business.MarketplaceInfo (InfoContractId, getInfoContractId)
import Business.MarketplaceInfo as MarketplaceInfo
import Business.MarketplaceUser (UserContractId, getUserContractId, ownPubKey)
import Capability.IPFS as IPFS
import Capability.LogMessages (class LogMessages, logInfo)
import Capability.Navigate (class Navigate, navigate)
import Capability.PollContract (class PollContract)
import Clipboard (handleAction)
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
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (classes)
import Halogen.HTML.Properties as HP
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
import Web.File.File as File
import Web.UIEvent.MouseEvent (MouseEvent, toEvent)

type State
  = { route :: Maybe Route
    , contracts :: RemoteData String (Array (ContractInstanceClientState MarketplaceContracts))
    , userInstances :: RemoteData String (Array ({ pubKey :: PubKeyHash, contractId :: UserContractId }))
    , currentInstance :: WalletSelector.UserWallet
    , infoInstance :: Maybe InfoContractId
    }

_contracts :: Lens' State (RemoteData String (Array (ContractInstanceClientState MarketplaceContracts)))
_contracts = prop (SProxy :: SProxy "contracts")

_userInstances :: Lens' State (RemoteData String (Array ({ pubKey :: PubKeyHash, contractId :: UserContractId })))
_userInstances = prop (SProxy :: SProxy "userInstances")

data Query a
  = NavigateTo Route a

type Slots
  = ( walletSelector :: WalletSelector.WalletSelectorSlot Unit
    , userPage :: User.Slot Unit
    , marketPage :: Market.Slot Unit
    )

data Action
  = Initialize
  | GoTo Route MouseEvent
  | ChooseWallet WalletSelector.Output
  | GetContracts
  | GetInstances
  | HandleFile File.File

component ::
  forall m input output.
  MonadEffect m =>
  Navigate m =>
  LogMessages m =>
  PollContract m =>
  IPFS.IPFS m =>
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
  initialState _ =
    { route: Nothing
    , contracts: NotAsked
    , userInstances: NotAsked
    , currentInstance: WalletSelector.WalletA
    , infoInstance: Nothing
    }

  render :: State -> H.ComponentHTML Action Slots m
  render st =
    HH.div_
      [ HH.text "Choose wallet: "
      , HH.slot WalletSelector._walletSelector unit WalletSelector.component st.currentInstance (Just <<< ChooseWallet)
      , HH.input [ HP.type_ HP.InputFile, HE.onFileUpload f ]
      , pages st
      ]

  f = case _ of
    [ file ] -> Just $ HandleFile file
    _ -> Nothing

  handleAction :: Action -> H.HalogenM State Action Slots output m Unit
  handleAction = case _ of
    Initialize -> do
      initialRoute <- hush <<< (Routing.parse routeCodec) <$> H.liftEffect Routing.getHash
      navigate $ fromMaybe UserPage initialRoute
      res <- IPFS.catFile "QmTxda1tn2ourkdKYhTwMfLScLCLxJUXMX3q4ScLyVFy8n"
      logInfo $ show res
      handleAction GetContracts
      handleAction GetInstances
    GoTo route e -> do
      H.liftEffect $ preventDefault (toEvent e)
      oldRoute <- H.gets _.route
      when (oldRoute /= Just route) $ navigate route
    ChooseWallet (WalletSelector.Submit w) -> do
      logInfo $ "Choose new wallet: " <> show w
      H.modify_ _ { currentInstance = w }
    GetContracts ->
      runRD _contracts <<< runExceptT
        $ lift getMarketplaceContracts
        >>= either (throwError <<< show) pure
    GetInstances ->
      runRD _userInstances <<< runExceptT
        $ do
            state <- lift H.get
            contracts <- RemoteData.maybe (throwError "No contracts found") pure state.contracts
            case catMaybes (getInfoContractId <$> contracts) of
              [ cid ] -> do
                lift $ logInfo $ "Found info instance: " <> show cid
                lift $ H.modify_ _ { infoInstance = Just cid }
              _ -> throwError "Info contract not found"
            parTraverse
              ( \contractId -> do
                  lift $ logInfo $ "Found user instance: " <> show contractId
                  pubKey <- lift (ownPubKey contractId) >>= either (throwError <<< show) pure
                  pure $ { pubKey, contractId }
              )
              (catMaybes <<< map getUserContractId $ contracts)
    HandleFile file -> do
      res <- IPFS.pinFile file
      logInfo $ "uploaded file : " <> show res

  handleQuery :: forall a. Query a -> H.HalogenM State Action Slots output m (Maybe a)
  handleQuery = case _ of
    NavigateTo route a -> do
      oldRoute <- H.gets _.route
      when (oldRoute /= Just route)
        $ H.modify_ _ { route = Just route }
      pure (Just a)

pages :: forall m. State -> H.ComponentHTML Action Slots m
pages st =
  navbar
    $ case st.route of
        Nothing -> HH.h1_ [ HH.text "Loading page..." ]
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
