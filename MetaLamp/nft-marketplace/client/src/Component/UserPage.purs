module Component.UserPage where

import Prelude

import Business.MarketplaceInfo (InfoContractId)
import Business.MarketplaceUser (UserContractId)
import Capability.LogMessages (class LogMessages, logInfo)
import Chain.State (handleAction)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.UserInstance (UserInstance)
import Halogen (Component, lift)
import Halogen as H
import Halogen.HTML as HH
import Plutus.V1.Ledger.Crypto (PubKeyHash)

type Slot id
  = forall query. H.Slot query Void id

_userPage :: SProxy "userPage"
_userPage = SProxy

type Input
  = { userInstance :: UserInstance
    , infoInstance :: InfoContractId
    }

type State
  = { userInstance :: UserInstance
    , infoInstance :: InfoContractId
    }

data Action
  = Initialize
  | Reinitialize Input

component :: forall query output m. LogMessages m => H.Component HH.HTML query Input output m
component =
  H.mkComponent
    { initialState: identity
    , render
    , eval: H.mkEval
          $ H.defaultEval
              { handleAction = handleAction
              , initialize = Just Initialize
              , receive = Just <<< Reinitialize
              }
    }
    where
    render :: State -> H.ComponentHTML Action () m
    render _ = HH.h1_ [ HH.text "UserPage" ]

    handleAction :: forall slots. Action -> H.HalogenM State Action slots output m Unit
    handleAction = case _ of
      Initialize -> do
        state <- H.get
        logInfo $ show state
      Reinitialize st -> do
        H.put st
        handleAction Initialize

