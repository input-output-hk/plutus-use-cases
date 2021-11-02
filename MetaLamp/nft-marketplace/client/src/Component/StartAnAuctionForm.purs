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
import Capability.GetDate (class GetDate, now, parseDate)
import Data.DateTime.Instant (Instant, unInstant)
import Control.Monad.Except (runExcept, throwError)
import Formless.Validation (Validation(..), hoistFnME_)
import Data.Maybe (Maybe(..), maybe)
import Data.JSDate as JSD
import Data.Int (fromNumber)
import Data.Newtype (unwrap)

type Slot
  = H.Slot (F.Query Form (Const Void) ()) AuctionOutput

type AuctionOutput
  = { | FormRow F.OutputType }

newtype Form r f
  = Form (r (FormRow f))

derive instance newtypeForm :: Newtype (Form r f) _

type FormRow f
  = ( endDate :: f String String Int
    , initialPrice :: f V.FieldError String Int
    )

isValidEndDate :: ∀ form m. GetDate m => Validation form m String String Int
isValidEndDate = hoistFnME_ $ \str -> do
  min <- now
  parsed <- parseDate str
  pure <<< runExcept $ do
    input <- maybe (throwError "Invalid date") pure (JSD.toInstant parsed)
    when (input < min) (throwError "End date should be in the future")
    maybe (throwError "Invalid timestamp") pure <<< fromNumber <<< unwrap <<< unInstant $ input 
    -- TODO: figure out a way to convert Number to BigInteger, this doesn't work - seemingly because Number is out of bounds

component ::
  forall m.
  MonadAff m =>
  GetDate m =>
  F.Component Form (Const Void) () Unit AuctionOutput m
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
          { endDate: isValidEndDate
          , initialPrice: V.strIsInt
          }
    , initialInputs: Nothing
    }

  renderForm { form } =
    UI.formContent_
      [ UI.input
          { label: "Initial price:"
          , help:
              F.getResult prx.initialPrice form
                # UI.resultToHelp
                    "What is a minimal price you want to sell for?"
          , placeholder: "10000"
          }
          [ HP.value $ F.getInput prx.initialPrice form
          , HE.onValueInput
              $ Just
              <<< F.setValidate prx.initialPrice
          ]
      , UI.dateInput
          { label: "Auction end date"
          , help:
              F.getResult prx.endDate form
                # UI.resultToHelp
                    "How long will the auction continue?"
          , placeholder: ""
          }
          [ HP.value $ F.getInput prx.endDate form
          , HE.onValueInput
              $ Just
              <<< F.setValidate prx.endDate
          ]
      , UI.buttonPrimary
          [ HE.onClick \_ -> Just F.submit ]
          [ HH.text "Put On Auction" ]
      ]
    where
    prx = F.mkSProxies (F.FormProxy :: _ Form)
