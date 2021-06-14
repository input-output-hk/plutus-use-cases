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
import qualified Ledger.Typed.Scripts as Scripts
import qualified PlutusTx as PlutusTx

{-# INLINABLE mkValidator #-}
-- | A validator script that can be used to burn any tokens sent to it.
mkValidator :: () -> () -> ScriptContext -> Bool
mkValidator _ _ _ = False

data Burning
instance Scripts.ScriptType Burning where
  type DatumType Burning = ()
  type RedeemerType Burning = ()

burnInst :: Scripts.ScriptInstance Burning
burnInst = Scripts.validator @Burning
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @() @()

burnValidator :: Validator
burnValidator = Scripts.validatorScript burnInst

burnValHash :: Ledger.ValidatorHash
burnValHash = validatorHash burnValidator

burnScrAddress :: Ledger.Address
burnScrAddress = Scripts.scriptAddress burnInst