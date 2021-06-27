module App.Types where

import Data.BigInt (BigInt)
import Data.Maybe (Maybe)

import PAB.Types (CurrencySymbol, Interval, TokenName)

--------------------------------------------------------------------------------

data FormEvent
  = SetField FieldEvent
  | SetSubField Int FormEvent
  | AddSubField
  | RemoveSubField Int

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