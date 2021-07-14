{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ViewPatterns          #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}

module Mlabs.Demo.Contract.Burn
  ( burnScrAddress
  , burnValHash
  ) where
      
import PlutusTx.Prelude hiding (Monoid(..), Semigroup(..))

import qualified Ledger as Ledger
import Ledger.Contexts
import Ledger.Scripts
import qualified Ledger.Typed.Scripts.Validators as Validators
import qualified PlutusTx as PlutusTx

{-# INLINABLE mkValidator #-}
-- | A validator script that can be used to burn any tokens sent to it.
mkValidator :: () -> () -> ScriptContext -> Bool
mkValidator _ _ _ = False

data Burning
instance Validators.ValidatorTypes Burning where
  type DatumType Burning = ()
  type RedeemerType Burning = ()

burnInst :: Validators.TypedValidator Burning
burnInst = Validators.mkTypedValidator @Burning
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Validators.wrapValidator @() @()

burnValidator :: Validator
burnValidator = Validators.validatorScript burnInst

burnValHash :: Ledger.ValidatorHash
burnValHash = validatorHash burnValidator

burnScrAddress :: Ledger.Address
burnScrAddress = Validators.validatorAddress burnInst