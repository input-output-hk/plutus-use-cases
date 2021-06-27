module App.Container where

import Prelude

import Affjax as AX
import Affjax.ResponseFormat as AXRF
import Data.Argonaut as A
import Data.Argonaut.Encode (encodeJson)
import App.ActionForm (actionForm)
import Bootstrap as BS
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Console (log, logShow)
import Effect.Aff.Class (class MonadAff)
import Halogen (HalogenQ(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (classes, style)
import Web.HTML.Event.EventTypes (offline)

import Config (pabConfig)
import PAB.Api as Api
import PAB.Types (ContractCall(CallEndpoint), ContractSignatureResponse, FormArgument(FormArgUnit, FormArgInt), FormArgument, PabConfig)

--------------------------------------------------------------------------------

type State = 
  { contractCall :: ContractCall 
  , contractDefinitions :: Array ContractSignatureResponse
  }

data Action
  = Initialize'

initialState :: forall input. input -> State
initialState _ = 
  { contractCall: 
      CallEndpoint
        { caller: { getWallet: 1 }
        , argumentValues: 
          { endpointDescription: { getEndpointDescription: "borrow" }
          , argument: FormArgInt $ Just 5
          } 
        }
  , contractDefinitions: []
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
            [ actionForm $ state.contractCall ]
        ]
    ]

handleAction :: 
  forall output m. 
  MonadAff m => 
  Action -> 
  H.HalogenM State Action () output m Unit
handleAction = case _ of 
  Initialize' -> do
    res <- H.liftAff $ Api.getContractDefinitions pabConfig
    H.liftEffect $ logShow $ A.stringify $ encodeJson res
    H.modify_ _ { contractDefinitions = res }


