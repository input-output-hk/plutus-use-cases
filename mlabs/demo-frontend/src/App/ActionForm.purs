module App.ActionForm (actionForm) where

import Prelude

import App.ActionFormInputs (actionFormInputs)
import App.Types (Action(..), Action(..), State(..), getSelectedContractSig)
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
import PAB.Types (ContractCall(..), ContractDefinition(..), ContractSignatureResponse, EndpointDescription, Wallet)
import Web.DOM.Document (getElementsByTagName)
import Web.Event.Event (Event(..))
import Web.Event.Event as Event
import Web.HTML.Event.EventTypes (offline)
import Web.HTML.HTMLSelectElement as HTMLSelectElement


--------------------------------------------------------------------------------
-- type State
--   = { count :: Int }
-- data Action
--   = Increment
-- data Query a
--   = Unit
-- type Slot = H.Slot Query Void Unit
-- proxy = Proxy :: Proxy "slot"
-- component :: forall q i o m. H.Component q i o m
-- component =
--   H.mkComponent
--     { initialState: \_ -> { count: 0 }
--     , render
--     , eval: H.mkEval H.defaultEval { handleAction = handleAction }
--     }
-- render :: forall cs m. State -> H.ComponentHTML Action cs m
-- render state = 
--   actionPane $
--     CallEndpoint
--       { caller: { getWallet: 1 }
--       , argumentValues: 
--         { endpointDescription: { getEndpointDescription: "Test" }
--         , argument: FormUnit
--         } 
--       }
-- handleAction :: forall cs o m. Action → H.HalogenM State Action cs o m Unit
-- handleAction = case _ of
--   Increment -> H.modify_ \st -> st { count = st.count + 1 }
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
    , submitBtn
    ]

-- actionFormBody (PayToWallet _) =
--   HH.div_
--     [ HH.h3_
--         [ HH.text $ "Not Data"
--         ]
--     ]

getContractSigNames :: State -> Array String
getContractSigNames state = _.csrDefinition <$> state.contractDefinitions

getEndpointNames :: State -> Array String
getEndpointNames state = 
  let 
    maybeContractSig = getSelectedContractSig state state.selectedContractIdx
  in 
    case maybeContractSig of
      Nothing   -> []
      Just csr   -> _.endpointDescription.getEndpointDescription <$> csr.csrSchemas

submitBtn :: forall w i. HH.HTML w i
submitBtn =
  HH.div
    [ HP.classes [ BS.displayGrid, BS.gap2 ] ]
    [ HH.button 
        [ HP.classes [ BS.btn, BS.btnPrimary ] ]
        [ HH.text "Submit" ]
    ]

-- actionPaneBody index (PayToWallet { sender, recipient, amount }) =
--   div_
--     [ h3_ [ walletIdPane sender, text ": Pay To Wallet" ]
--     , formGroup_
--         [ label [ for "recipient" ] [ text "Recipient" ]
--         , input
--             [ type_ InputNumber
--             , classes [ formControl ]
--             , value $ show $ view _walletId recipient
--             , required true
--             , min 1.0
--             , placeholder "Wallet ID"
--             , onBigIntegerInput $ ModifyActions <<< SetPayToWalletRecipient index <<< review _walletId
--             ]
--         ]
--     , formGroup_
--         [ label [ for "amount" ] [ text "Amount" ]
--         , valueForm (ModifyActions <<< SetPayToWalletValue index) amount
--         ]
--     ]
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
        mkOption false <$> items
    ]
 where
  mkOption :: Boolean -> String -> HH.HTML w Action
  mkOption isSelected item =
    HH.option
      [ HP.value $ item ]
      [ HH.text $ item ]
  
  inputId :: String
  inputId = label <> "Input"
