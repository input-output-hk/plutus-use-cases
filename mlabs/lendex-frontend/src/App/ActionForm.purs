module App.ActionForm (actionForm) where

import Prelude

import App.ActionFormInputs (actionFormInputs)
import Bootstrap as BS
import Data.Maybe (Maybe(..), fromMaybe)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Elements.Keyed as Keyed
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import PAB.Types (ContractCall(..), ContractDefinition(..), EndpointDescription, FormArgument(..), Wallet)
import Web.DOM.Document (getElementsByTagName)

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
actionForm :: forall w i. ContractCall FormArgument -> HH.HTML w i
actionForm contractCall =
  HH.div
    [ HP.classes [ BS.card, HH.ClassName ("action-pane") ] ]
    [ actionFormBody contractCall ]

actionFormBody :: forall w i. ContractCall FormArgument -> HH.HTML w i
actionFormBody (CallEndpoint { caller, argumentValues }) =
  HH.div
    [ HP.classes [ BS.my2, BS.cardBody ] ]
    [ contractField [ ContractDefinition "User" ]
    , walletField [ caller ]
    , endpointField [ argumentValues.endpointDescription ]
    , actionFormInputs argumentValues.argument
    , submitBtn
    ]

actionFormBody (PayToWallet _) =
  HH.div_
    [ HH.h3_
        [ HH.text $ "Not Data"
        ]
    ]

contractField :: forall w i. Array ContractDefinition -> HH.HTML w i
contractField cds = dropdownField "Contract" $ getName <$> cds
 where 
  getName (ContractDefinition cd) = cd

walletField :: forall w i. Array Wallet -> HH.HTML w i
walletField wallets = dropdownField "Wallet" $ getName <$> wallets
 where 
  getName w = "Wallet " <> show w.getWallet

endpointField :: forall w i. Array EndpointDescription -> HH.HTML w i
endpointField endpoints = dropdownField "Endpoint" $ _.getEndpointDescription <$> endpoints

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
dropdownField :: forall w i. String -> Array String -> HH.HTML w i
dropdownField label items =
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
        ] $ 
        mkOption false <$> items
    ]
 where
  mkOption :: Boolean -> String -> HH.HTML w i
  mkOption isSelected item =
    HH.option
      [ HP.value $ item ]
      [ HH.text $ item ]
  
  inputId :: String
  inputId = label <> "Input"
