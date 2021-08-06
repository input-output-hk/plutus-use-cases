{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}

module Mlabs.Demo.Contract.Burn (
  burnScrAddress,
  burnValHash,
) where

import Ledger (Address, ScriptContext, Validator, ValidatorHash, validatorHash)
import Ledger.Typed.Scripts.Validators qualified as Validators
import PlutusTx qualified
import PlutusTx.Prelude (Bool (False))

{-# INLINEABLE mkValidator #-}

-- | A validator script that can be used to burn any tokens sent to it.
mkValidator :: () -> () -> ScriptContext -> Bool
mkValidator _ _ _ = False

data Burning
instance Validators.ValidatorTypes Burning where
  type DatumType Burning = ()
  type RedeemerType Burning = ()

burnInst :: Validators.TypedValidator Burning
burnInst =
  Validators.mkTypedValidator @Burning
    $$(PlutusTx.compile [||mkValidator||])
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Validators.wrapValidator @() @()

burnValidator :: Validator
burnValidator = Validators.validatorScript burnInst

burnValHash :: Ledger.ValidatorHash
burnValHash = validatorHash burnValidator

burnScrAddress :: Ledger.Address
burnScrAddress = Validators.validatorAddress burnInst
