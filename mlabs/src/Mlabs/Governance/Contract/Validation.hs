{-# LANGUAGE UndecidableInstances #-}
{-
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fobject-code #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
-}

-- | Validation, on-chain code for governance application 
module Mlabs.Governance.Contract.Validation (
    scrAddress
  , scrInstance
  , scrValidator
  , govValueOf
  , xgovValueOf
  , xGovMintingPolicy
  , xGovCurrencySymbol
  , Governance
  , GovernanceDatum(..)
  , AssetClassNft(..)
  , AssetClassGov(..)
  ) where

import Data.Coerce (coerce)
import GHC.Generics (Generic)
import Playground.Contract (FromJSON, ToJSON, ToSchema)
import PlutusTx.AssocMap qualified as AssocMap
import PlutusTx qualified
import PlutusTx.Prelude hiding (Semigroup(..), unless)
import Ledger hiding (singleton)
import Ledger.Typed.Scripts      qualified as Scripts
import Plutus.V1.Ledger.Value    qualified as Value
import Plutus.V1.Ledger.Contexts qualified as Contexts
import Prelude qualified as Hask 

import Mlabs.Governance.Contract.Api (AssetClassNft(..), AssetClassGov(..), Deposit(..), Withdraw(..))

-- there's a discussion to be had about whether we want the AssetClasses to parametrize
-- the contract or just sit in the datum. 
data GovernanceDatum = GovernanceDatum {
    gdNft :: !AssetClassNft 
  , gdGov :: !AssetClassGov
  , gdDepositMap :: !(AssocMap.Map PubKeyHash Integer)
  } deriving (Hask.Show, Generic, ToJSON, FromJSON, ToSchema)

PlutusTx.unstableMakeIsData ''GovernanceDatum
PlutusTx.makeLift ''GovernanceDatum

data GovernanceValidator = GVDeposit !Deposit | GVWithdraw !Withdraw
  deriving (Hask.Show, Generic)

PlutusTx.unstableMakeIsData ''GovernanceValidator

data Governance
instance Scripts.ScriptType Governance where
    type instance DatumType Governance = GovernanceDatum
    type instance RedeemerType Governance = GovernanceValidator

-- Validator of the governance contract
{-# INLINABLE mkValidator #-}
mkValidator :: GovernanceDatum -> GovernanceValidator -> ScriptContext -> Bool
mkValidator _ _ _ = True -- todo: can't do it w/o validator type, do that one first
    
scrInstance :: Scripts.ScriptInstance Governance
scrInstance = Scripts.validator @Governance
  $$(PlutusTx.compile [|| mkValidator ||])
  $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @(Scripts.DatumType Governance) @(Scripts.RedeemerType Governance)

scrValidator :: Validator
scrValidator = Scripts.validatorScript scrInstance

scrAddress :: Ledger.Address
scrAddress = scriptAddress scrValidator

govValueOf :: AssetClassGov -> Integer -> Value
govValueOf AssetClassGov{..}  = Value.singleton acGovCurrencySymbol acGovTokenName

xgovValueOf :: CurrencySymbol -> TokenName -> Integer -> Value
xgovValueOf csym tok = Value.singleton csym tok

-- xGOV minting policy
{-# INLINABLE mkPolicy #-}
mkPolicy :: AssetClassGov -> ScriptContext -> Bool
mkPolicy AssetClassGov{..} ctx =
  traceIfFalse "More than one signature" checkOneSignature  &&
  traceIfFalse "Incorrect tokens minted" checkxGov &&
  traceIfFalse "GOV not paid to the script" checkGovToScr
  where
    info = scriptContextTxInfo ctx

    checkOneSignature = length (txInfoSignatories info) == 1 

    checkxGov = case Value.flattenValue (Contexts.txInfoForge info) of
      [(cur, tn, amm)] -> cur == Contexts.ownCurrencySymbol ctx && amm == govPaidAmm && [coerce tn] == txInfoSignatories info -- to be tested
      _ -> False

    -- checks that the GOV was paid to the governance script, returns the value of it
    -- TODO: uncomment after plutus upgrade (with the ByteString eq pr)
    (checkGovToScr, govPaidAmm) = case fmap txOutValue . find (\txout -> {- scrAddress == txOutAddress txout -} True) $ txInfoOutputs info of
      Nothing  -> (False,0) 
      Just val -> case Value.flattenValue val of
        [(cur, tn, amm)] -> (cur == acGovCurrencySymbol && tn == acGovTokenName, amm)
        _ -> (False,0)
        
xGovMintingPolicy :: AssetClassGov -> Scripts.MonetaryPolicy
xGovMintingPolicy gov = mkMonetaryPolicyScript $
  $$(PlutusTx.compile [|| Scripts.wrapMonetaryPolicy . mkPolicy ||]) `PlutusTx.applyCode` PlutusTx.liftCode gov

-- may be a good idea to newtype these two
-- | takes in the GOV CurrencySymbol, returns the xGOV CurrencySymbol
xGovCurrencySymbol :: AssetClassGov -> CurrencySymbol
xGovCurrencySymbol = scriptCurrencySymbol . xGovMintingPolicy 
