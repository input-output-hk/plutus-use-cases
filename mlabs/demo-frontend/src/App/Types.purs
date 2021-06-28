module App.Types 
  ( Action(..)
  , getSelectedWalleId
  , getSelectedContractSig
  , getSelectedFunctionSchema
  , FieldEvent(..)
  , State
  , ValueEvent(..)
  ) where

import Prelude

import Data.Array as Array
import Data.BigInt (BigInt)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import PAB.Types (ContractSignatureResponse, CurrencySymbol, FormArgument, FunctionSchema, Interval, TokenName)
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

getSelectedWalleId :: State -> Int -> Maybe Int
getSelectedWalleId state idx = Array.index state.walletIds idx

getSelectedContractSig :: State -> Int -> Maybe ContractSignatureResponse
getSelectedContractSig state idx = Array.index state.contractDefinitions idx

getSelectedFunctionSchema :: State -> Int -> Int -> Maybe FunctionSchema
getSelectedFunctionSchema state contractIdx endpointIdx = 
 let 
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
  | SetFormField FieldEvent
  | SetFormSubField Int FieldEvent

data FieldEvent
  = SetIntField (Maybe Int)
  | SetBigIntegerField (Maybe BigInt)
  | SetBoolField Boolean
  | SetStringField String
  | SetHexField String
  | SetRadioField String
  | SetValueField ValueEvent
  | SetPOSIXTimeRangeField Interval

data ValueEvent
  = SetBalance CurrencySymbol TokenName BigInt