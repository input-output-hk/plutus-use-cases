module App.Types 
  ( Action(..)
  , getSelectedWalleId
  , getSelectedContractSig
  , getSelectedFunctionSchema
  , FieldEvent(..)
  , handleFormEvent
  , State
  ) where

import Prelude

import Data.Array as Array
import Data.BigInt (BigInt)
import Debug.Trace (trace)
import Data.Generic.Rep (class Generic)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Show (class Show)
import Data.Show.Generic (genericShow)
import Matryoshka (Algebra, ana, cata)
import PAB.Types (ContractSignatureResponse, CurrencySymbol, Fix(..), FormArgument, FormArgumentF(..), FormSchema, FunctionSchema, Interval, TokenName, Value, toArgument)
import Web.HTML.Event.EventTypes (offline)

--------------------------------------------------------------------------------

type State = 
  { contractDefinitions :: Array ContractSignatureResponse
  , walletIds :: Array Int
  , selectedWalletIdx :: Int
  , selectedContractIdx :: Int
  , selectedEndpointIdx :: Int
  , argument :: FormArgument
  }

getSelectedWalleId :: State -> Maybe Int
getSelectedWalleId state = 
  let
    idx = state.selectedWalletIdx
  in
    Array.index state.walletIds idx

getSelectedContractSig :: State -> Maybe ContractSignatureResponse
getSelectedContractSig state = 
  let 
    idx = state.selectedContractIdx
  in 
    Array.index state.contractDefinitions idx

getSelectedFunctionSchema :: State -> Maybe (FunctionSchema FormSchema)
getSelectedFunctionSchema state = 
 let 
   contractIdx = state.selectedContractIdx
   endpointIdx = state.selectedEndpointIdx
   maybeSchemas = _.csrSchemas <$> Array.index state.contractDefinitions contractIdx
 in
   case maybeSchemas of
     Nothing      -> Nothing
     Just schemas -> Array.index schemas endpointIdx

data Action
  = Initialize'
  | SetSelectedWalletIdx Int
  | SetSelectedContractIdx Int
  | SetSelectedEndpointIdx Int
  | SetField FieldEvent
  | SetSubField Int Action
  | Submit

-- TODO: Use BigInt for SetBigIntegerField
data FieldEvent
  = SetIntField (Maybe Int)
  | SetBigIntegerField (Maybe Int)
  | SetBoolField Boolean
  | SetStringField String
  | SetHexField String
  | SetRadioField String
  -- | SetValueField ValueEvent
  | SetPOSIXTimeRangeField Interval

handleFormEvent ::
  Value ->
  Action ->
  FormArgument ->
  FormArgument
handleFormEvent initialValue event = 
  trace event \_ -> cata (Fix <<< algebra event)
  where
  algebra (SetField (SetIntField n)) (FormIntF _) = FormIntF n

  algebra (SetField (SetBigIntegerField n)) (FormIntegerF _) = FormIntegerF n

  algebra (SetField (SetBoolField n)) (FormBoolF _) = FormBoolF n

  algebra (SetField (SetStringField s)) (FormStringF _) = FormStringF (Just s)

  algebra (SetField (SetHexField s)) (FormHexF _) = FormHexF (Just s)

  algebra (SetField (SetRadioField s)) (FormRadioF options _) = FormRadioF options (Just s)

  -- algebra (SetField (SetValueField valueEvent)) (FormValueF value) = FormValueF $ handleValueEvent valueEvent value

  algebra (SetField (SetPOSIXTimeRangeField newInterval)) arg@(FormPOSIXTimeRangeF _) = FormPOSIXTimeRangeF newInterval

  algebra (SetSubField 1 subEvent) (FormTupleF field1 field2) = FormTupleF (handleFormEvent initialValue subEvent field1) field2

  algebra (SetSubField 2 subEvent) (FormTupleF field1 field2) = FormTupleF field1 (handleFormEvent initialValue subEvent field2)

  -- algebra (SetSubField 0 subEvent) (FormMaybeF schema field) = FormMaybeF schema $ over _Just (handleFormEvent initialValue subEvent) field

  -- algebra (SetSubField n subEvent) (FormArrayF schema fields) = FormArrayF schema $ over (ix n) (handleFormEvent initialValue subEvent) fields

  -- algebra (SetSubField n subEvent) s@(FormObjectF fields) = FormObjectF $ over (ix n <<< _Newtype <<< _2) (handleFormEvent initialValue subEvent) fields

  -- As the code stands, this is the only guarantee we get that every
  -- value in the array will conform to the schema: the fact that we
  -- create the 'empty' version from the same schema template.
  -- Is more type safety than that possible? Probably.
  -- Is it worth the research effort? Perhaps. :thinking_face:

  algebra _ arg = arg