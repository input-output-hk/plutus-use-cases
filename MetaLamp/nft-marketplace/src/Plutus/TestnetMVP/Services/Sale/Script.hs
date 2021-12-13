{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}

module Plutus.TestnetMVP.Services.Sale.Script where

import qualified Control.Lens                                     as Lens
import qualified Data.Aeson                 as J
import           GHC.Generics               (Generic)
import           Ledger
import qualified Ledger.Typed.Scripts             as Scripts
import qualified PlutusTx
import           Prelude
import qualified Prelude                                          as Haskell
import qualified GHC.Generics                                     as Haskell

type Saler = PubKeyHash
type Buyer = PubKeyHash
type LovelacePrice = Integer

data Sale =
  Sale
    { saleProtocolToken :: !AssetClass,
      salePrice         :: !LovelacePrice,
      saleValue         :: !Value,
      saleOwner         :: !Saler
    }
  deriving stock (Haskell.Eq, Haskell.Show, Haskell.Generic)
  deriving anyclass (J.ToJSON, J.FromJSON)

PlutusTx.unstableMakeIsData ''Sale

PlutusTx.makeLift ''Sale

data SaleRedeemer
  = Buy Buyer
  | Redeem
  deriving  (Haskell.Show)

PlutusTx.unstableMakeIsData ''SaleRedeemer

PlutusTx.makeLift ''SaleRedeemer

data SaleDatum =
    SaleOngoing
  | SaleClosed
  deriving  (Haskell.Show)

PlutusTx.unstableMakeIsData ''SaleDatum

PlutusTx.makeLift ''SaleDatum

data SaleScript
instance Scripts.ValidatorTypes SaleScript where
    type instance DatumType SaleScript = SaleDatum
    type instance RedeemerType SaleScript = SaleRedeemer
