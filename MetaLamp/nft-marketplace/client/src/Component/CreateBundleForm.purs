module Component.CreateBundleForm where

import Prelude
import Data.Array (filter, snoc, catMaybes)
import Data.Const (Const)
import Data.Either (Either(..))
import Data.List (toUnfoldable)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
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
-- PutNft form
-- Form types
type SubmittedBundle
  = { name :: String
    , description :: String
    , subcategories :: Array String
    , tokenIpfsCids :: Array String
    }

getSubmittedBundle :: { | PutNftRow F.OutputType } -> SubmittedBundle
getSubmittedBundle form =
  { name: form.name
  , description: form.description
  , subcategories: _.category <$> form.subcategories
  , tokenIpfsCids: _.ipfsCid <$> form.tokenIpfsCids
  }

newtype PutBundleForm r f
  = PutBundleForm (r (PutNftRow f))

derive instance newtypePutBundleForm :: Newtype (PutBundleForm r f) _

type PutNftRow f
  = ( name :: f Void String String
    , description :: f Void String String
    , subcategories :: f V.FieldError (Maybe (Array CategoryInfo)) (Array CategoryInfo)
    , tokenIpfsCids :: f V.FieldError (Maybe (Array IpfsCidInfo)) (Array IpfsCidInfo)
    )

-- Form component types
type Slot
  = H.Slot (F.Query PutBundleForm (Const Void) ChildSlots) SubmittedBundle

type State
  = ( categoryFormIds :: Array Int
    , categoryNextId :: Int
    , ipfsCidFormIds :: Array Int
    , ipfsCidNextId :: Int
    )

data Action
  = AddCategoryForm
  | AddIpfsCidForm
  | SubmitAll
  | HandleCategoryForm Int CategoryFormMessage
  | HandleIpfsCidForm Int IpfsCidFormMessage

type ChildSlots
  = ( categoryForm :: CategoryFormSlot Int, ipfsCidForm :: IpfsCidFormSlot Int )

-- Form spec
component ::
  forall m.
  MonadAff m =>
  F.Component PutBundleForm (Const Void) ChildSlots Unit SubmittedBundle m
component =
  F.component (const putNftFormInput)
    $ F.defaultSpec
        { render = render
        , handleAction = handleAction
        , handleEvent = handleEvent
        }
  where
  putNftFormInput :: F.Input PutBundleForm State m
  putNftFormInput =
    { validators:
        PutBundleForm
          { name: F.noValidation
          , description: F.noValidation
          , tokenIpfsCids: F.hoistFn_ (fromMaybe [])
          , subcategories: F.hoistFn_ (fromMaybe [])
          }
    , initialInputs: Nothing
    , categoryFormIds: []
    , categoryNextId: 0
    , ipfsCidFormIds: []
    , ipfsCidNextId: 0
    }

  handleAction = case _ of
    HandleCategoryForm ix Destroy1 -> do
      H.modify_ \st -> st { categoryFormIds = filter (_ /= ix) st.categoryFormIds }
      eval $ F.set _subcategories Nothing
    HandleIpfsCidForm ix Destroy2 -> do
      H.modify_ \st -> st { ipfsCidFormIds = filter (_ /= ix) st.ipfsCidFormIds }
      eval $ F.set _tokenIpfsCids Nothing
    AddCategoryForm ->
      H.modify_ \st ->
        st
          { categoryNextId = st.categoryNextId + 1, categoryFormIds = st.categoryFormIds `snoc` st.categoryNextId }
    AddIpfsCidForm ->
      H.modify_ \st ->
        st
          { ipfsCidNextId = st.ipfsCidNextId + 1, ipfsCidFormIds = st.ipfsCidFormIds `snoc` st.ipfsCidNextId }
    SubmitAll -> do
      resCategories <- H.queryAll _categoryForm $ H.request F.submitReply
      resTokens <- H.queryAll _ipfsCidForm $ H.request F.submitReply
      let
        categoriesList = map F.unwrapOutputFields $ catMaybes $ toUnfoldable $ M.values resCategories
      let
        tokensList = map F.unwrapOutputFields $ catMaybes $ toUnfoldable $ M.values resTokens
      case Tuple categoriesList tokensList of
        Tuple subcategories [] -> pure unit
        Tuple [] tokens -> eval (F.set _tokenIpfsCids (Just tokens)) *> eval F.submit *> eval F.resetAll
        Tuple subcategories tokens -> eval (F.set _tokenIpfsCids (Just tokens)) *> eval (F.set _subcategories (Just subcategories)) *> eval F.submit *> eval F.resetAll
    where
    eval act = F.handleAction handleAction handleEvent act

    _subcategories = SProxy :: _ "subcategories"

    _tokenIpfsCids = SProxy :: _ "tokenIpfsCids"

    _categoryForm = SProxy :: _ "categoryForm"

    _ipfsCidForm = SProxy :: _ "ipfsCidForm"

  handleEvent = case _ of
    F.Submitted outputs -> do
      H.raise (getSubmittedBundle $ F.unwrapOutputFields outputs)
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
          ]
      , HH.div
          [ class_ "field is-grouped" ]
          [ HH.div
              [ class_ "control" ]
              [ UI.button
                  [ HE.onClick \_ -> Just $ F.injAction AddIpfsCidForm ]
                  [ HH.text "Add Token Form" ]
              ]
          ]
      , UI.input
          { label: "Bundle Name"
          , help:
              Right
                "Provide a Bundle name"
          , placeholder: "My Bundle"
          }
          [ HP.value $ F.getInput _name st.form
          , HE.onValueInput $ Just <<< F.setValidate _name
          ]
      , UI.input
          { label: "Bundle Description"
          , help:
              Right
                "Provide a Bundle description"
          , placeholder: "Collection of artwork"
          }
          [ HP.value $ F.getInput _description st.form
          , HE.onValueInput $ Just <<< F.setValidate _description
          ]
      , HH.div_
          (mkCategoryForm <$> st.categoryFormIds)
      , HH.div_
          (mkIpfsCidForm <$> st.ipfsCidFormIds)
      , HH.div
          [ class_ "control" ]
          [ UI.buttonPrimary
              [ HE.onClick \_ -> Just $ F.injAction SubmitAll ]
              [ HH.text "Submit" ]
          ]
      ]
    where
    mkCategoryForm i = do
      let
        handler = Just <<< F.injAction <<< HandleCategoryForm i
      HH.slot _categoryForm i categoryFormComponent unit handler

    mkIpfsCidForm i = do
      let
        handler = Just <<< F.injAction <<< HandleIpfsCidForm i
      HH.slot _ipfsCidForm i ipfsCidFormComponent unit handler

    _name = SProxy :: SProxy "name"

    _description = SProxy :: SProxy "description"

    _categoryForm = SProxy :: SProxy "categoryForm"

    _ipfsCidForm = SProxy :: SProxy "ipfsCidForm"

    _revealIssuer = SProxy :: SProxy "revealIssuer"

-----
-- Category form, nested inside
-----
-- Form types
type CategoryInfo
  = { | CategoryRow F.OutputType }

newtype CategoryForm r f
  = CategoryForm (r (CategoryRow f))

derive instance newtypeCategoryForm :: Newtype (CategoryForm r f) _

type CategoryRow f
  = ( category :: f Void String String
    )

-- Form component types
type CategoryFormSlot
  = H.Slot (F.Query' CategoryForm) CategoryFormMessage

data CategoryFormAction
  = RemoveMe1

data CategoryFormMessage
  = Destroy1

-- Form spec
categoryFormComponent ::
  forall m.
  MonadAff m => F.Component CategoryForm (Const Void) () Unit CategoryFormMessage m
categoryFormComponent =
  F.component (const categoryFormInput)
    $ F.defaultSpec
        { render = render
        , handleAction = handleAction
        }
  where
  categoryFormInput :: F.Input' CategoryForm m
  categoryFormInput =
    { validators:
        CategoryForm
          { category: F.noValidation
          }
    , initialInputs: Nothing
    }

  handleAction = case _ of
    RemoveMe1 -> H.raise Destroy1

  render st =
    UI.formContent_
      [ HH.div
          [ class_ "field" ]
          [ UI.buttonPrimary
              [ HE.onClick \_ -> Just $ F.injAction RemoveMe1 ]
              [ HH.text "Remove Category" ]
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

-----
-- IpfsCid form, nested inside
-----
-- Form types
type IpfsCidInfo
  = { | IpfsCidRow F.OutputType }

newtype IpfsCidForm r f
  = IpfsCidForm (r (IpfsCidRow f))

derive instance newtypeIpfsCidForm :: Newtype (IpfsCidForm r f) _

type IpfsCidRow f
  = ( ipfsCid :: f Void String String
    )

-- Form component types
type IpfsCidFormSlot
  = H.Slot (F.Query' IpfsCidForm) IpfsCidFormMessage

data IpfsCidFormAction
  = RemoveMe2

data IpfsCidFormMessage
  = Destroy2

-- Form spec
ipfsCidFormComponent ::
  forall m.
  MonadAff m => F.Component IpfsCidForm (Const Void) () Unit IpfsCidFormMessage m
ipfsCidFormComponent =
  F.component (const ipfsCidFormInput)
    $ F.defaultSpec
        { render = render
        , handleAction = handleAction
        }
  where
  ipfsCidFormInput :: F.Input' IpfsCidForm m
  ipfsCidFormInput =
    { validators:
        IpfsCidForm
          { ipfsCid: F.noValidation
          }
    , initialInputs: Nothing
    }

  handleAction = case _ of
    RemoveMe2 -> H.raise Destroy2

  render st =
    UI.formContent_
      [ HH.div
          [ class_ "field" ]
          [ UI.buttonPrimary
              [ HE.onClick \_ -> Just $ F.injAction RemoveMe2 ]
              [ HH.text "Remove Token" ]
          ]
      , UI.input
          { label: "Token to Add"
          , help: Right "Enter token IPFS content ID."
          , placeholder: "QmPeoJnaDttpFrSySYBY3reRFCzL3qv4Uiqz376EBv9W16"
          }
          [ HP.value $ F.getInput _ipfsCid st.form
          , HE.onValueInput $ Just <<< F.set _ipfsCid
          ]
      ]
    where
    _ipfsCid = SProxy :: SProxy "ipfsCid"
