{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fobject-code #-}
module Plutus.TestnetMVP.Services.Sale.Validator where

import Plutus.TestnetMVP.Services.Sale.Script
import qualified Data.Aeson                                   as J
import qualified Schema
import qualified GHC.Generics                                 as Haskell
import qualified Prelude                                      as Haskell
import           Ledger.Value
import qualified PlutusTx
import qualified Ledger.Typed.Scripts                             as Scripts
import           PlutusTx.Prelude                                 
import qualified PlutusTx.AssocMap                               as AssocMap
import Plutus.V1.Ledger.Contexts (ScriptContext)
import Plutus.V1.Ledger.Address (Address)
import Ledger.Address (scriptAddress)

newtype SaleThreadToken = 
    SaleThreadToken
        {saleProtocolInstance :: AssetClass}
        deriving stock    (Haskell.Eq, Haskell.Show, Haskell.Generic)
    deriving anyclass (J.ToJSON, J.FromJSON, Schema.ToSchema)

PlutusTx.makeLift ''SaleThreadToken

saleInstance :: Sale -> Scripts.TypedValidator SaleScript
saleInstance sale = Scripts.mkTypedValidator @SaleScript
    ($$(PlutusTx.compile [|| makeSaleValidator ||])
        `PlutusTx.applyCode` PlutusTx.liftCode sale)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @SaleDatum @SaleRedeemer

makeSaleValidator :: Sale -> SaleDatum -> SaleRedeemer -> ScriptContext -> Bool
makeSaleValidator _ _ (Buy buyer) _ = trace "Buy" $ True
makeSaleValidator _ _ Redeem _ = trace "Redeem" $ True

saleValidator :: Sale -> Scripts.Validator
saleValidator = Scripts.validatorScript . saleInstance

saleAddress :: Sale -> Address
saleAddress = scriptAddress . saleValidator
