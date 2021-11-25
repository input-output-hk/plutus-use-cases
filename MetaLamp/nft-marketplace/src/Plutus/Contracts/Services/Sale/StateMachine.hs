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

module Plutus.Contracts.Services.Sale.StateMachine where

import qualified Control.Lens                        as Lens
import qualified Data.Aeson                          as J
import qualified Data.Text                           as T
import qualified GHC.Generics                        as Haskell
import           Ledger
import qualified Ledger.Ada                          as Ada
import qualified Ledger.Constraints                  as Constraints
import qualified Ledger.Typed.Scripts                as Scripts
import           Ledger.Value
import           Plutus.Abstract.Percentage          (Percentage (..))
import qualified Plutus.Abstract.Percentage          as Percentage
import qualified Plutus.Abstract.PercentageInterface as Percentage
import           Plutus.Contract
import           Plutus.Contract.StateMachine
import           Plutus.Contracts.Services.Sale.Core
import qualified PlutusTx
import qualified PlutusTx.AssocMap                   as AssocMap
import           PlutusTx.Prelude                    hiding (Semigroup (..))
import           Prelude                             (Semigroup (..), (/))
import qualified Prelude                             as Haskell
import qualified Schema
import Ext.Plutus.Ledger.Index (minAdaTxOutValue)

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

type GetAdditionalConstraints = Sale -> TxConstraints Void Void

saleWithFeeBuyConstraints :: SaleFee -> GetAdditionalConstraints
saleWithFeeBuyConstraints SaleFee {..} Sale {..} =
  let saleProfit = salePrice - operatorFee
      operatorFee = Percentage.calculatePercentageRounded sfSaleFee salePrice
  in
    Constraints.mustPayToPubKey saleOwner (Ada.lovelaceValueOf saleProfit) <>
    Constraints.mustPayToPubKey sfSaleOperator (Ada.lovelaceValueOf operatorFee)

saleWithoutFeeBuyConstraints :: GetAdditionalConstraints
saleWithoutFeeBuyConstraints Sale {..} =
    Constraints.mustPayToPubKey saleOwner (Ada.lovelaceValueOf salePrice)

{-# INLINABLE transition #-}
transition :: GetAdditionalConstraints -> Sale -> State SaleDatum -> SaleRedeemer -> Maybe (TxConstraints Void Void, State SaleDatum)
transition additionalConstraints sale@Sale{..} state redeemer = case (stateData state, redeemer) of
    (SaleOngoing, Redeem)
        -> Just ( Constraints.mustBeSignedBy saleOwner <>
                  Constraints.mustPayToPubKey saleOwner val
                , State SaleClosed mempty
                )
    (SaleOngoing, Buy buyer) | saleValueWithMinAdaTxOut == val
        -> Just ( Constraints.mustBeSignedBy buyer <>
                  Constraints.mustPayToPubKey buyer saleValueWithMinAdaTxOut <>  -- TODO: is it okay that buyer receive additional 2ADA? Should we initially add them to the sale price?
                  additionalConstraints sale
                , State SaleClosed mempty
                )
    _                                        -> Nothing
  where
    val = stateValue state
    saleValueWithMinAdaTxOut = saleValue + minAdaTxOutValue

{-# INLINABLE isFinal #-}
isFinal :: SaleDatum -> Bool
isFinal SaleClosed = True
isFinal _          = False

{-# INLINABLE saleStateMachine #-}
saleStateMachine :: Sale -> StateMachine SaleDatum SaleRedeemer
saleStateMachine sale = StateMachine
    { smTransition  = getTransition $ saleOperatorFee sale
    , smFinal       = isFinal
    , smCheck       = \d r ctx -> True
    , smThreadToken = Just $ saleProtocolToken sale
    }
    where
      getTransition (Just fee) = transition (saleWithFeeBuyConstraints fee) sale
      getTransition Nothing    = transition saleWithoutFeeBuyConstraints sale

{-# INLINABLE mkSaleValidator #-}
mkSaleValidator :: Sale -> SaleDatum -> SaleRedeemer -> ScriptContext -> Bool
mkSaleValidator sale = mkValidator $ saleStateMachine sale

type SaleScript = StateMachine SaleDatum SaleRedeemer

saleInst :: Sale -> Scripts.TypedValidator SaleScript
saleInst sale = Scripts.mkTypedValidator @SaleScript
    ($$(PlutusTx.compile [|| mkSaleValidator ||])
        `PlutusTx.applyCode` PlutusTx.liftCode sale)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @SaleDatum @SaleRedeemer

saleClient :: Sale -> StateMachineClient SaleDatum SaleRedeemer
saleClient sale = mkStateMachineClient $ StateMachineInstance (saleStateMachine sale) (saleInst sale)

saleValidator :: Sale -> Validator
saleValidator = Scripts.validatorScript . saleInst

saleAddress :: Sale -> Ledger.Address
saleAddress = scriptAddress . saleValidator
