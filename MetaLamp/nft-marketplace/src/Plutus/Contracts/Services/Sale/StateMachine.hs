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
import           Plutus.Contract
import           Plutus.Contract.StateMachine
import           Plutus.Contracts.Services.Sale.Core
import qualified PlutusTx
import qualified PlutusTx.AssocMap                   as AssocMap
import           PlutusTx.Prelude                    hiding (Semigroup (..))
import           Prelude                             (Semigroup (..))
import qualified Prelude                             as Haskell
import qualified Schema

data SaleRedeemer
  = Buy Buyer
  | Redeem
  deriving  (Haskell.Show)

PlutusTx.unstableMakeIsData ''SaleRedeemer

PlutusTx.makeLift ''SaleRedeemer

data SaleDatum =
    LotInfo Saler
  | SaleClosed
  deriving  (Haskell.Show)

PlutusTx.unstableMakeIsData ''SaleDatum

PlutusTx.makeLift ''SaleDatum

{-# INLINABLE transition #-}
transition :: Sale -> State SaleDatum -> SaleRedeemer -> Maybe (TxConstraints Void Void, State SaleDatum)
transition Sale{..} state redeemer = case (stateData state, redeemer) of
    (LotInfo saler, Redeem)
        -> Just ( Constraints.mustBeSignedBy saler <>
                  Constraints.mustPayToPubKey saler val
                , State SaleClosed mempty
                )
    (LotInfo saler, Buy buyer) | saleValue == (val - stateToken)
        -> Just ( Constraints.mustBeSignedBy buyer <>
                  Constraints.mustPayToPubKey saler (stateToken <> Ada.lovelaceValueOf salePrice) <>
                  Constraints.mustPayToPubKey buyer saleValue
                , State SaleClosed mempty
                )
    _                                        -> Nothing
  where
    stateToken :: Value
    stateToken = assetClassValue saleProtocolToken 1

    val = stateValue state

{-# INLINABLE isFinal #-}
isFinal :: SaleDatum -> Bool
isFinal SaleClosed = True
isFinal _          = False

{-# INLINABLE saleStateMachine #-}
saleStateMachine :: Sale -> StateMachine SaleDatum SaleRedeemer
saleStateMachine sale = StateMachine
    { smTransition  = transition sale
    , smFinal       = isFinal
    , smCheck       = \d r ctx -> True
    , smThreadToken = Just $ saleProtocolToken sale
    }

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

saleProtocolName :: TokenName
saleProtocolName = "Sale"

saleValidator :: Sale -> Validator
saleValidator = Scripts.validatorScript . saleInst

saleAddress :: Sale -> Ledger.Address
saleAddress = scriptAddress . saleValidator
