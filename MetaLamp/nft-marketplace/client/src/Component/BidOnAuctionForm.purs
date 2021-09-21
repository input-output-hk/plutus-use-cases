module Component.BidOnAuctionForm where

import Prelude
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Effect.Aff.Class (class MonadAff)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Utils.FormValidation as V
import View.FormElement as UI

type Slot
  = H.Slot (F.Query Form (Const Void) ()) BidOutput

type BidOutput
  = { | FormRow F.OutputType }

newtype Form r f
  = Form (r (FormRow f))

derive instance newtypeForm :: Newtype (Form r f) _

type FormRow f
  = ( bid :: f V.FieldError String Int
    )

component ::
  forall m.
  MonadAff m =>
  F.Component Form (Const Void) () Unit BidOutput m
component =
  F.component (const formInput)
    $ F.defaultSpec
        { render = renderForm
        , handleEvent = F.raiseResult
        }
  where
  formInput :: F.Input' Form m
  formInput =
    { validators:
        Form
          { bid: V.strIsInt
          }
    , initialInputs: Nothing
    }

  renderForm { form } =
    UI.formContent_
      [ UI.input
          { label: "Bid value"
          , help:
              F.getResult prx.bid form
                # UI.resultToHelp
                    "How much lovelace would bid?"
          , placeholder: "10000"
          }
          [ HP.value $ F.getInput prx.bid form
          , HE.onValueInput
              $ Just
              <<< F.setValidate prx.bid
          ]
      , UI.buttonPrimary
          [ HE.onClick \_ -> Just F.submit ]
          [ HH.text "Submit Bid" ]
      ]
    where
    prx = F.mkSProxies (F.FormProxy :: _ Form)
