module App.ActionForm (actionForm) where

import Prelude

import Bootstrap as BS
import Data.Array as Array
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Debug.Trace (trace)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Elements.Keyed as Keyed
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.DOM.Document (getElementsByTagName)
import Web.Event.Event (Event(..))
import Web.Event.Event as Event
import Web.HTML.Event.EventTypes (offline)
import Web.HTML.HTMLSelectElement as HTMLSelectElement

import App.ActionFormInputs (actionFormInputs)
import App.Types (Action(..), State(..), getSelectedContractSig)
import PAB.Types (ContractCall(..), ContractDefinition(..), ContractSignatureResponse, EndpointDescription, Wallet)

--------------------------------------------------------------------------------

actionForm :: forall w. State -> HH.HTML w Action
actionForm state =
  HH.div
    [ HP.classes [ BS.card, HH.ClassName ("action-pane") ] ]
    [ actionFormBody state ]

actionFormBody :: forall w. State -> HH.HTML w Action
actionFormBody state =
  HH.div
    [ HP.classes [ BS.my2, BS.cardBody ] ]
    [ dropdownField "Wallet" (show <$> state.walletIds) SetSelectedWalletIdx
    , dropdownField "Contract" (getContractSigNames state) SetSelectedContractIdx
    , dropdownField "Endpoint" (getEndpointNames state) SetSelectedEndpointIdx
    , actionFormInputs state.argument
    , submitBtn
    ]

getContractSigNames :: State -> Array String
getContractSigNames state = _.csrDefinition <$> state.contractDefinitions

getEndpointNames :: State -> Array String
getEndpointNames state = 
  let 
    maybeContractSig = getSelectedContractSig state
  in 
    case maybeContractSig of
      Nothing   -> []
      Just csr   -> _.endpointDescription.getEndpointDescription <$> csr.csrSchemas

submitBtn :: forall w. HH.HTML w Action
submitBtn =
  HH.div
    [ HP.classes [ BS.displayGrid, BS.gap2 ],
      HE.onClick \e -> Submit
    ]
    [ HH.button 
        [ HP.classes [ BS.btn, BS.btnPrimary ] ]
        [ HH.text "Submit" ]
    ]

dropdownField :: 
  forall w. 
  String -> 
  Array String -> 
  (Int -> Action) -> 
  HH.HTML w Action
dropdownField label items onSelectedIndexChange =
  HH.div
    [ HP.classes [ BS.mb3 ] ]
    [ HH.label
      [ HP.for $ inputId
      , HP.class_ BS.formLabel
      ]
      [ HH.text label ]
    , HH.select
        [ HP.id inputId
        , HP.classes [ BS.formSelect ] 
        , HE.onSelectedIndexChange onSelectedIndexChange
        ] $ 
        mkOption <$> items
    ]
 where
  mkOption :: String -> HH.HTML w Action
  mkOption item =
    HH.option
      [ HP.value $ item ]
      [ HH.text $ item ]
  
  inputId :: String
  inputId = label <> "Input"
