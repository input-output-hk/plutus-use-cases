module Component.StartAnAuctionForm where

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
  = H.Slot (F.Query Form (Const Void) ()) DurationOutput

type DurationOutput
  = { | FormRow F.OutputType }

newtype Form r f
  = Form (r (FormRow f))

derive instance newtypeForm :: Newtype (Form r f) _

type FormRow f
  = ( duration :: f V.FieldError String Int -- Seconds
    )

component ::
  forall m.
  MonadAff m =>
  F.Component Form (Const Void) () Unit DurationOutput m
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
          { duration: V.strIsInt
          }
    , initialInputs: Nothing
    }

  renderForm { form } =
    UI.formContent_
      [ UI.input
          { label: "Duration in seconds"
          , help:
              F.getResult prx.duration form
                # UI.resultToHelp
                    "How long will the auction continue?"
          , placeholder: "360"
          }
          [ HP.value $ F.getInput prx.duration form
          , HE.onValueInput
              $ Just
              <<< F.setValidate prx.duration
          ]
      , UI.buttonPrimary
          [ HE.onClick \_ -> Just F.submit ]
          [ HH.text "Put On Auction" ]
      ]
    where
    prx = F.mkSProxies (F.FormProxy :: _ Form)