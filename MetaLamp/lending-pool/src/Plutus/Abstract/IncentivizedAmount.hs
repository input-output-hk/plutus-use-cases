{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fobject-code #-}

module Plutus.Abstract.IncentivizedAmount where

import qualified Control.Lens            as Lens
import           Data.Aeson              (FromJSON, ToJSON)
import           GHC.Generics
import qualified Plutus.Abstract.TxUtils as TxUtils
import           Plutus.V1.Ledger.Slot   (Slot (..))
import qualified PlutusTx
import           PlutusTx.Prelude
import           PlutusTx.Ratio          (Ratio)
import qualified Prelude
import           Schema                  (ToSchema)

data IncentivizedAmount = IncentivizedAmount{ iaSlot :: Slot, iaRate :: Rational, iaAmount :: Rational }
    deriving stock (Prelude.Eq, Prelude.Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

Lens.makeClassy_ ''IncentivizedAmount

instance Eq IncentivizedAmount where
  a == b = iaSlot a == iaSlot b && iaAmount a == iaAmount b && iaRate a == iaRate b

{-# INLINABLE accrue #-}
accrue :: Rational -> Slot -> IncentivizedAmount -> IncentivizedAmount
accrue newRate newSlot (IncentivizedAmount oldSlot oldRate amount) = IncentivizedAmount newSlot newRate (amount * oldRate)
