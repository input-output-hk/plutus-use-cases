module App.Form (actionPane) where

import Prelude
-- import Action.Validation (actionIsValid)
-- import Action.Lenses (_InSlot)
import Bootstrap as BS
-- import Data.Array (mapWithIndex)
-- import Data.Array as Array
-- import Data.BigInteger (BigInteger)
-- import Data.BigInteger as BigInteger
-- import Data.Either (Either(..))
-- import Data.Lens (review, view)
import Data.Maybe (Maybe(..), fromMaybe)
-- import Data.Tuple (Tuple(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Elements.Keyed as Keyed
import Halogen.HTML.Properties (InputType(..), checked, class_, classes, draggable, for, id_, min, name, placeholder, required, type_, value)
-- import Icons (Icon(..), icon)
-- import MainFrame.Types (DragAndDropEventType(..), HAction(..), SimulatorAction)
-- import Playground.Lenses (_endpointDescription, _getEndpointDescription)
-- import Playground.Types (ContractCall(..), SimulatorWallet, _FunctionSchema)
-- import Plutus.V1.Ledger.Slot (Slot)
-- import Prelude (const, one, show, ($), (+), (<$>), (<<<), (<>), (==))
-- import Schema.Types (ActionEvent(..), FormArgument, SimulationAction(..))
-- import Schema.View (actionArgumentForm)
-- import Validation (_argument)
-- import ValueEditor (valueForm)
-- import Wallet.Lenses (_walletId)
-- import Wallet.View (walletIdPane)
-- import Web.Event.Event (Event)
-- import Web.HTML.Event.DragEvent (DragEvent)
-- import Type.Proxy (Proxy(..))

import App.ActionArgumentForm (actionArgumentForm)
import PAB.Types (ContractCall(..), FormArgument(..))

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

actionPane :: forall w i. ContractCall FormArgument -> HH.HTML w i
actionPane contractCall =
  HH.div
    [ classes [ BS.card, HH.ClassName ("action-pane") ] ]
    -- [ actionPaneBody action ]
    [ HH.div 
        [ classes [ BS.cardBody ] ]
        [ actionPaneBody contractCall ]
    ]

actionPaneBody :: forall p i. ContractCall FormArgument -> HH.HTML p i
actionPaneBody (CallEndpoint { caller, argumentValues }) =
  HH.div_
    [ HH.h3_
      [ HH.text $ "Wallet " <> show caller.getWallet
      -- , text $ view (_FunctionSchema <<< _endpointDescription <<< _getEndpointDescription) argumentValues
      ]
    -- , actionArgumentForm index (PopulateAction index) $ view (_FunctionSchema <<< _argument) argumentValues
    ]
actionPaneBody (PayToWallet _) = 
  HH.div_
    [ HH.h3_
      [ HH.text $ "Not Data"
      ]
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
