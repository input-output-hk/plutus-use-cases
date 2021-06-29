{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TemplateHaskell#-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Plutus.Contract.Data.Payment
where

import PlutusTx.Prelude
import qualified PlutusTx.AssocMap as AssocMap
import GHC.Generics
import Ledger
import Ledger.Value
import PlutusTx
import Ledger.Ada
import Data.Aeson (ToJSON, FromJSON)
import Prelude (Show)



-- Map from PubKeyHash to Value.
--

newtype Payment = Payment ( AssocMap.Map PubKeyHash Value ) deriving(Generic,ToJSON,FromJSON,Show)

instance Semigroup Payment where
    {-# INLINABLE (<>) #-}
    (<>) (Payment a) (Payment b) = Payment (a <> b)

instance Monoid Payment where
  {-# INLINABLE mempty   #-}
  mempty = Payment AssocMap.empty

{-# INLINABLE payment  #-}
payment :: PubKeyHash -> Value -> Payment
payment pkHash value=Payment  (AssocMap.singleton pkHash value)

{-# INLINABLE paymentValue #-}
paymentValue :: Payment -> PubKeyHash -> Value
paymentValue (Payment p) pkh=case AssocMap.lookup pkh p of
    Just v ->  v
    _      ->Value AssocMap.empty

{-# INLINABLE paymentPkhs #-}
paymentPkhs :: Payment -> [PubKeyHash]
paymentPkhs (Payment x) =  AssocMap.keys x

makeLift ''Payment
PlutusTx.unstableMakeIsData ''Payment
