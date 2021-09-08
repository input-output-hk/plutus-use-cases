module Component.CreateNftForm where

import Prelude

import Data.Array (filter, snoc, catMaybes)
import Data.Const (Const)
import Data.Either (Either(..))
import Data.List (toUnfoldable)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Effect.Aff.Class (class MonadAff)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Utils.FormValidation as V
import View.FormElement (class_)
import View.FormElement as UI

-----
-- Event form

-- Form types

type Event = { | EventRow F.OutputType }

newtype EventForm r f = EventForm (r (EventRow f))
derive instance newtypeEventForm :: Newtype (EventForm r f) _

type EventRow f =
  ( name :: f Void String String
  , description :: f Void String String
  , subcategories :: f V.FieldError (Maybe (Array CategoryInfo)) (Array CategoryInfo)
  )

-- Form component types

type Slot =
  H.Slot (F.Query EventForm (Const Void) ChildSlots) Event

type State =
  ( formIds :: Array Int
  , nextId :: Int
  )

data Action
  = AddCategoryForm
  | SubmitAll
  | HandleCategoryForm Int CategoryFormMessage

type ChildSlots =
  ( categoryForm :: CategoryFormSlot Int )

-- Form spec

eventComponent :: forall m.
  MonadAff m =>
   F.Component EventForm (Const Void) ChildSlots Unit Event m
eventComponent = F.component (const eventFormInput) $ F.defaultSpec
  { render = render
  , handleAction = handleAction
  , handleEvent = handleEvent
  }
  where
  eventFormInput :: F.Input EventForm State m
  eventFormInput =
    { validators: EventForm
        { name: F.noValidation
        , description: F.noValidation
        , subcategories: F.hoistFn_ (fromMaybe [])
        }
    , initialInputs: Nothing
    , formIds: []
    , nextId: 0
    }

  handleAction = case _ of
    HandleCategoryForm ix Destroy -> do
      H.modify_ \st -> st { formIds = filter (_ /= ix) st.formIds }
      eval $ F.set _subcategories Nothing

    AddCategoryForm ->
      H.modify_ \st -> st
        { nextId = st.nextId + 1, formIds = st.formIds `snoc` st.nextId }

    SubmitAll -> do
      st <- H.get
      res <- H.queryAll _categoryForm $ H.request F.submitReply
      case map F.unwrapOutputFields $ catMaybes $ toUnfoldable $ M.values res of
        [] -> eval F.submit
        subcategories -> eval (F.set _subcategories (Just subcategories)) *> eval F.submit

    where
    eval act = F.handleAction handleAction handleEvent act
    _subcategories = SProxy :: _ "subcategories"
    _categoryForm = SProxy :: _ "categoryForm"

  handleEvent = case _ of
    F.Submitted outputs ->
      H.raise (F.unwrapOutputFields outputs)
    _ -> pure unit

  render st =
    HH.div_
      [ HH.div
        [ class_ "field is-grouped" ]
        [ HH.div
            [ class_ "control" ]
            [ UI.button
                [ HE.onClick \_ -> Just $ F.injAction AddCategoryForm ]
                [ HH.text "Add Category Form" ]
            ]
        , HH.div
            [ class_ "control" ]
            [ UI.buttonPrimary
                [ HE.onClick \_ -> Just $ F.injAction SubmitAll ]
                [ HH.text "Submit" ]
            ]
        ]
      , UI.input
          { label: "NFT Name"
          , help: Right
              "Provide an NFT name"
          , placeholder: "My NFT"
          }
          [ HP.value $ F.getInput _name st.form
          , HE.onValueInput $ Just <<< F.setValidate _name
          ]
      , UI.input
          { label: "NFT Description"
          , help: Right
              "Provide an NFT description"
          , placeholder: "Beautiful artwork"
          }
          [ HP.value $ F.getInput _description st.form
          , HE.onValueInput $ Just <<< F.setValidate _description
          ]
      , HH.div_
          (mkCategoryForm <$> st.formIds)
      ]
    where
    mkCategoryForm i = do
      let handler = Just <<< F.injAction <<< HandleCategoryForm i
      HH.slot _categoryForm i categoryFormComponent unit handler

    _name = SProxy :: SProxy "name"
    _description = SProxy :: SProxy "description"
    _categoryForm = SProxy :: SProxy "categoryForm"


-----
-- Category form, nested inside
-----

-- Form types

type CategoryInfo = { | CategoryRow F.OutputType }

newtype CategoryForm r f = CategoryForm (r (CategoryRow f))
derive instance newtypeCategoryForm :: Newtype (CategoryForm r f) _

type CategoryRow f =
  ( category :: f Void String String
  )

-- Form component types

type CategoryFormSlot =
  H.Slot (F.Query' CategoryForm) CategoryFormMessage

data CategoryFormAction = RemoveMe
data CategoryFormMessage = Destroy

-- Form spec

categoryFormComponent :: forall m.
  MonadAff m => F.Component CategoryForm (Const Void) () Unit CategoryFormMessage m
categoryFormComponent = F.component (const categoryFormInput) $ F.defaultSpec
  { render = render
  , handleAction = handleAction
  }
  where
  categoryFormInput :: F.Input' CategoryForm m
  categoryFormInput =
    { validators: CategoryForm
        { category: F.noValidation
        }
    , initialInputs: Nothing
    }

  handleAction = case _ of
    RemoveMe -> H.raise Destroy

  render st =
   UI.formContent_
     [ HH.div
         [ class_ "field" ]
         [ UI.buttonPrimary
             [ HE.onClick \_ -> Just $ F.injAction RemoveMe ]
             [ HH.text "Remove Me" ]
         ]

     , UI.input
         { label: "Inner category"
         , help: Right "Provide any additional category you'd like."
         , placeholder: "photos"
         }
         [ HP.value $ F.getInput _category st.form
         , HE.onValueInput $ Just <<< F.set _category
         ]
    ]
    where
    _category = SProxy :: SProxy "category"
