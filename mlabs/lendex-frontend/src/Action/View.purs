module Action.View (actionPane) where

-- import Action.Validation (actionIsValid)
-- import Action.Lenses (_InSlot)
import Bootstrap (btn, card, cardBody_, col, colFormLabel, col_, formCheck, formCheckInline, formCheckInput, formCheckLabel, formControl, formGroup_, formRow_, floatRight)
-- import Data.Array (mapWithIndex)
-- import Data.Array as Array
-- import Data.BigInteger (BigInteger)
-- import Data.BigInteger as BigInteger
-- import Data.Either (Either(..))
-- import Data.Lens (review, view)
-- import Data.Maybe (Maybe(..), fromMaybe)
-- import Data.Tuple (Tuple(..))
import Halogen.HTML (ClassName(ClassName), HTML, IProp, button, div, div_, h2_, h3_, input, label, p_, text)
import Halogen.HTML.Elements.Keyed as Keyed
import Halogen.HTML.Events (onChange, onClick, onDragEnd, onDragEnter, onDragLeave, onDragOver, onDragStart, onDrop, onValueInput)
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

import PAB.Types (ContractCall, FormArgument)

--------------------------------------------------------------------------------

actionPane :: forall w i. ContractCall FormArgument -> HTML w i
actionPane action =
  div
    [ classes [ card, ClassName ("action-pane") ] ]
    -- [ actionPaneBody action ]
    [ text "Hello World"]

-- actionPaneBody :: forall p. Int -> SimulatorAction -> HTML p SimulationAction
-- actionPaneBody index (CallEndpoint { caller, argumentValues }) =
--   div_
--     [ h3_
--         [ walletIdPane caller
--         , text ": "
--         , text $ view (_FunctionSchema <<< _endpointDescription <<< _getEndpointDescription) argumentValues
--         ]
--     , actionArgumentForm index (PopulateAction index) $ view (_FunctionSchema <<< _argument) argumentValues
--     ]

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

-- actionPaneBody index (AddBlocks { blocks }) =
--   div_
--     [ h3_ [ text "Wait" ]
--     , waitTypeRadioInputs index (Right blocks)
--     , formGroup_
--         [ formRow_
--             [ label [ classes [ col, colFormLabel ] ]
--                 [ text "Blocks" ]
--             , col_
--                 [ input
--                     [ type_ InputNumber
--                     , classes [ formControl, ClassName $ "action-argument-0-blocks" ]
--                     , value $ show blocks
--                     , required true
--                     , min 1.0
--                     , placeholder "Block Number"
--                     , onBigIntegerInput $ ModifyActions <<< SetWaitTime index
--                     ]
--                 ]
--             ]
--         ]
--     ]

-- actionPaneBody index (AddBlocksUntil { slot }) =
--   div_
--     [ h3_ [ text "Wait" ]
--     , waitTypeRadioInputs index (Left slot)
--     , formGroup_
--         [ formRow_
--             [ label [ classes [ col, colFormLabel ] ]
--                 [ text "Slot" ]
--             , col_
--                 [ input
--                     [ type_ InputNumber
--                     , classes [ formControl, ClassName $ "action-argument-0-until-slot" ]
--                     , value $ show $ view _InSlot slot
--                     , required true
--                     , min 1.0
--                     , placeholder "Slot Number"
--                     , onBigIntegerInput $ ModifyActions <<< SetWaitUntilTime index <<< review _InSlot
--                     ]
--                 ]
--             ]
--         ]
--     ]