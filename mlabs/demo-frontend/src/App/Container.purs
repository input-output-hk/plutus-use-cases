module App.Container where

import Prelude

import Affjax as AX
import Affjax.ResponseFormat as AXRF
import App.ActionForm (actionForm)
import App.Types (Action(..), State(..))
import Bootstrap as BS
import Config (pabConfig)
import Data.Argonaut as A
import Data.Argonaut.Encode (encodeJson)
import Data.Array ((..))
import Data.Either (Either(..))
import Data.Json.JsonTuple (JsonTuple(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (class MonadAff)
import Effect.Console (log, logShow)
import Halogen (HalogenQ(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (classes, style)
import PAB.Api as Api
import PAB.Types (ContractCall(CallEndpoint), ContractSignatureResponse, FormArgumentF(..), Fix(..), FormArgument, PabConfig, Wallet)
import Web.HTML.Event.EventTypes (offline)

--------------------------------------------------------------------------------

initialState :: forall input. input -> State
initialState _ = 
  { contractDefinitions: []
  , walletIds: 1..10
  , selectedWalletIdx: 0
  , selectedContractIdx: 0
  , selectedEndpointIdx: 0
  , argument: Fix $ FormObjectF [ JsonTuple ("Test Field" /\ (Fix (FormIntF $ Just 5))) ]
  }

component :: forall q i o m. MonadAff m => H.Component q i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize'
        }
    }

render :: forall cs m. State -> H.ComponentHTML Action cs m
render state =
  HH.div
    [ classes [ BS.container, BS.h100 ] ]
    [ HH.div
        [ classes 
            [ BS.row
            , BS.displayFlex
            , BS.flexColumn
            , BS.alignItemsCenter
            , BS.justifyContentCenter 
            ] 
        ]
        [ HH.h1 
            [ classes [ BS.textCenter, BS.my4 ] ]
            [ HH.text "Plutus Use Cases Demo" ]
        , HH.div 
            [ classes [ BS.colMd6 ] ]
            [ actionForm state
            ]
        ]
    ]

handleAction :: 
  forall output m. 
  MonadAff m => 
  Action -> 
  H.HalogenM State Action () output m Unit
handleAction = case _ of 
  Initialize' -> do
    contractDefs <- H.liftAff $ Api.getContractDefinitions pabConfig
    H.liftEffect $ logShow $ A.stringify $ encodeJson contractDefs
    H.modify_ _ { contractDefinitions = contractDefs }
  SetSelectedWalletIdx i -> do
    H.modify_ _ { selectedWalletIdx = i }
  SetSelectedContractIdx i-> do
    H.modify_ _ { selectedContractIdx = i, selectedEndpointIdx = 0 }
  SetSelectedEndpointIdx i -> do
    H.modify_ _ { selectedEndpointIdx = i }
  SetFormField _ -> do
    pure unit
  SetFormSubField _ _ -> do
    pure unit


