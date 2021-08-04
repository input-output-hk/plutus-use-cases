{-# LANGUAGE UndecidableInstances #-}

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

-- we can't have those two be one type with type param due to
-- Data type erasure. I hate it.
data AssetClassNft = AssetClassNft {
    acNftCurrencySymbol :: !CurrencySymbol
  , acNftTokenName :: !TokenName
  } deriving (Hask.Show, Hask.Eq, Generic, ToJSON, FromJSON, ToSchema)

PlutusTx.unstableMakeIsData ''AssetClassNft
PlutusTx.makeLift ''AssetClassNft

data AssetClassGov = AssetClassGov {
    acGovCurrencySymbol :: !CurrencySymbol
  , acGovTokenName :: !TokenName
  } deriving (Hask.Show, Hask.Eq, Generic, ToJSON, FromJSON, ToSchema)

PlutusTx.unstableMakeIsData ''AssetClassGov
PlutusTx.makeLift ''AssetClassGov

-- there's a discussion to be had about whether we want the AssetClasses to parametrize
-- the contract or just sit in the datum. 
data GovernanceDatum = GovernanceDatum {
    gdNft :: !AssetClassNft 
  , gdGov :: !AssetClassGov
  , gdDepositMap :: !(AssocMap.Map PubKeyHash Integer)
  } deriving (Hask.Show, Generic, ToJSON, FromJSON, ToSchema)

PlutusTx.unstableMakeIsData ''GovernanceDatum
PlutusTx.makeLift ''GovernanceDatum

data Governance
instance Scripts.ScriptType Governance where
    type instance DatumType Governance = GovernanceDatum
    type instance RedeemerType Governance = () -- todo: needs to restructure so that Api types have a representation here

-- Validator of the governance contract
{-# INLINABLE mkValidator #-}
mkValidator :: GovernanceDatum -> () -> ScriptContext -> Bool
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
mkPolicy gov ctx =
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
    -- TODO: either fix the ByteString problems OR wait until they get patched (preferrably)
    (checkGovToScr, govPaidAmm) = case fmap txOutValue . find (\txout -> {- scrAddress csym == txOutAddress txout -} True) $ txInfoOutputs info of
      Nothing  -> (False,0) 
      Just val -> case Value.flattenValue val of
        [(cur, tn, amm)] -> (True {- cur == csym  && tn == govToken -}, amm)
        _ -> (False,0)
        
xGovMintingPolicy :: AssetClassGov -> Scripts.MonetaryPolicy
xGovMintingPolicy gov = mkMonetaryPolicyScript $
  $$(PlutusTx.compile [|| Scripts.wrapMonetaryPolicy . mkPolicy ||]) `PlutusTx.applyCode` PlutusTx.liftCode gov

-- may be a good idea to newtype these two
-- | takes in the GOV CurrencySymbol, returns the xGOV CurrencySymbol
xGovCurrencySymbol :: AssetClassGov -> CurrencySymbol
xGovCurrencySymbol = scriptCurrencySymbol . xGovMintingPolicy 
