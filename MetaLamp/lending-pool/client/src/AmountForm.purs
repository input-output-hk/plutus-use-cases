module AmountForm where

import Prelude

import Data.BigInteger (BigInteger, fromString)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type AmountInfo = { name :: String, amount :: BigInteger }

data Output = Submit AmountInfo

data Action = SubmitClick | EnterName String | EnterAmount String | Receive Input

type State = { amounts :: Array AmountInfo, name :: Maybe String, amount :: Maybe BigInteger }

type Input = Array AmountInfo

initialState :: Input -> State
initialState amounts = { amounts, name: Nothing, amount: Nothing }

amountForm :: forall query m. H.Component HH.HTML query Input Output m
amountForm =
  H.mkComponent
    { initialState: initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction, receive = Just <<< Receive }
    }
  where
  render state =
    HH.div_
      [ HH.select
          [HP.value (fromMaybe "" state.name), HE.onValueChange (Just <<< EnterName)]
          (map (\({ name }) ->
            HH.option [HP.value name, HP.selected (name == (fromMaybe "" state.name))] [HH.text name])
            state.amounts
          ),
        HH.input [HP.value $ maybe "" show state.amount, HE.onValueInput (Just <<< EnterAmount)],
        HH.button [HE.onClick \_ -> Just SubmitClick] [HH.text "Submit"]
      ]

  handleAction :: Action -> H.HalogenM State Action () Output m Unit
  handleAction = case _ of
    SubmitClick -> do
      { name, amount } <- H.get
      case name of
        Just n ->
          case amount of
            Just a -> H.raise $ Submit { name: n, amount: a }
            _ -> pure unit
        _ -> pure unit
    EnterName name -> H.modify_ _ { name = Just name }
    EnterAmount amount -> H.modify_ _ { amount = fromString amount }
    Receive amounts -> H.modify_ _ { amounts = amounts }
