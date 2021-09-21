{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fobject-code #-}
{-# LANGUAGE StandaloneDeriving    #-}

module Plutus.Contracts.Services.Sale.Core where

import qualified Control.Lens                 as Lens
import qualified Data.Aeson                   as J
import qualified Data.Text                    as T
import qualified GHC.Generics                 as Haskell
import           Ledger
import qualified Ledger.Ada                   as Ada
import qualified Ledger.Constraints           as Constraints
import qualified Ledger.Typed.Scripts         as Scripts
import           Ledger.Value
import           Plutus.Contract
import           Plutus.Contract.StateMachine
import qualified PlutusTx
import qualified PlutusTx.AssocMap            as AssocMap
import           PlutusTx.Prelude             hiding (Semigroup (..))
import           Prelude                      (Semigroup (..))
import qualified Prelude                      as Haskell
import qualified Schema

type Saler = PubKeyHash
type Buyer = PubKeyHash
type LovelacePrice = Integer

data Sale =
  Sale
    { saleProtocolToken :: !ThreadToken,
      salePrice         :: !LovelacePrice,
      saleValue         :: !Value
      -- TODO add owner, it is needed for client, we need to hide "close sale" button from users other than owner
      -- saleOwner :: PubKeyHash
    }
  deriving stock (Haskell.Eq, Haskell.Show, Haskell.Generic)
  deriving anyclass (J.ToJSON, J.FromJSON)

PlutusTx.unstableMakeIsData ''Sale

PlutusTx.makeLift ''Sale

Lens.makeClassy_ ''Sale
