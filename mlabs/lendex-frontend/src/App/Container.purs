module App.Container where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (classes, style)

import App.Form as Form
import Bootstrap as BS
import PAB.Types (ContractCall(CallEndpoint), FormArgument(FormUnit), FormArgument)

type State = { contractCall :: ContractCall FormArgument }

data Action = Unit

initialState :: forall input. input -> State
initialState _ = 
  { contractCall: 
      CallEndpoint
        { caller: { getWallet: 1 }
        , argumentValues: 
          { endpointDescription: { getEndpointDescription: "Test" }
          , argument: FormUnit
          } 
        }
  }

component :: forall q i o m. H.Component q i o m
component =
  H.mkComponent
    { initialState: initialState
    , render
    , eval: H.mkEval H.defaultEval
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
            [ classes [ BS.textCenter ] ]
            [ HH.text "Plutus Use Cases Demo" ]
        , HH.div 
            [ classes [ BS.colMd6 ] ]
            [ Form.actionPane $ state.contractCall ]
        ]
    ]


