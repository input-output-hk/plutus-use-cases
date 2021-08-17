{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fobject-code #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}

-- | Validation, on-chain code for governance application 
module Mlabs.Governance.Contract.Validation (
    scrAddress
  , scrInstance
  , scrValidator
  , govSingleton
  , xgovSingleton
  , xGovMintingPolicy
  , xGovCurrencySymbol
  , Governance
  , GovParams(..)
  , GovernanceDatum(..)
  , GovernanceRedeemer(..)
  , AssetClassNft(..)
  , AssetClassGov(..)
  ) where

import           PlutusTx.Prelude hiding (Semigroup(..), unless)
import qualified Prelude as Hask 

import Data.Coerce (coerce)
import GHC.Generics (Generic)

import           Playground.Contract (FromJSON, ToJSON, ToSchema)
import qualified PlutusTx.AssocMap as AssocMap
import qualified PlutusTx

import           Ledger hiding (before, after)
import           Ledger.Typed.Scripts (wrapMintingPolicy)
import qualified Ledger.Typed.Scripts.Validators as Validators
import qualified Plutus.V1.Ledger.Value    as Value
import qualified Plutus.V1.Ledger.Contexts as Contexts
import qualified Plutus.V1.Ledger.Address  as Address
import           Plutus.V1.Ledger.Credential (Credential(..))

-- TODO: Once AssetClass has a ToSchema instance, change this to a newtype.
--       or not. this is fine really. 
data AssetClassGov = AssetClassGov {
    acGovCurrencySymbol :: !CurrencySymbol
  , acGovTokenName :: !TokenName
  } deriving (Hask.Show, Hask.Eq, Generic, ToJSON, FromJSON, ToSchema)

instance Eq AssetClassGov where
  {-# INLINABLE (==) #-}
  n1 == n2 = acGovCurrencySymbol n1 == acGovCurrencySymbol n2
    && acGovTokenName n1 == acGovTokenName n2

PlutusTx.unstableMakeIsData ''AssetClassGov
PlutusTx.makeLift ''AssetClassGov

data GovernanceRedeemer 
  = GRDeposit !Integer 
  | GRWithdraw !Integer
  deriving Hask.Show

instance Eq GovernanceRedeemer where
  {-# INLINABLE (==) #-}
  (GRDeposit n1) == (GRDeposit n2) = n1 == n2
  (GRWithdraw n1) == (GRWithdraw n2) = n1 == n2
  _ == _ = False

PlutusTx.unstableMakeIsData ''GovernanceRedeemer
PlutusTx.makeLift ''GovernanceRedeemer

data GovernanceDatum = GovernanceDatum {
    gdLastRedeemer :: !GovernanceRedeemer,
    gdxGovCurrencySymbol :: !CurrencySymbol,
  } deriving Hask.Show

PlutusTx.unstableMakeIsData ''GovernanceDatum
PlutusTx.makeLift ''GovernanceDatum

data Governance
instance Validators.ValidatorTypes Governance where
    type instance DatumType Governance = GovernanceDatum
    type instance RedeemerType Governance = GovernanceRedeemer

-- gov tn == xgov tn
-- | governance validator
{-# INLINABLE govValidator #-}
govValidator :: PubKeyHash -> AssetClassGov -> GovernanceDatum -> GovernanceRedeemer -> ScriptContext -> Bool
govValidator pkh ac@AssetClassGov{..} datum redeemer ctx =
  traceIfFalse "incorrect value from redeemer"         checkCorrectValue &&
  traceIfFalse "invalid pkh redeeming"                 checkCorrectPkh &&
  traceIfFalse "incorrect minting script involvenment" checkMinting &&
  traceIfFalse "invalid datum update"                  checkCorrectDatumUpdate 
  where 
    info = scriptContextTxInfo ctx

    ownInput :: Contexts.TxInInfo
    ownInput = case findOwnInput ctx of
      Just o  -> o
      Nothing -> traceError "no self in input"

    ownOutput :: Contexts.TxOut
    ownOutput = case Contexts.getContinuingOutputs ctx of
      [o] -> o
      _   -> traceError "expected exactly one continuing output"

    inValueGov :: Value
    inValueGov = txOutValue $ txInInfoResolved ownInput

    outValueGov :: Value
    outValueGov = txOutValue ownOutput

    xGovCS :: CurrencySymbol
    xGovCS = gdxGovCurrencySymbol datum

    xGovTN :: TokenName
    xGovTN = gdxGovTokenName datum

    isForging :: Bool
    isForging = case AssocMap.lookup xGov . Value.getValue $ txInfoForge info of
      Nothing -> False
      Just _  -> True

    -- checks 

    checkMinting :: Bool
    checkMinting = case redeemer of
      GRDeposit _ -> isForging
      GRDeposit _ -> isForging
  
    -- any can pay to any gov but only pkh can withdraw
    checkCorrectPkh :: Bool
    checkCorrectPkh = case redeemer of
      GRDeposit _  -> True
      GRWithdraw _ -> pkh `elem` txInfoSignatories info

    checkCorrectValue :: Bool
    checkCorrectValue = case redeemer of
      GRDeposit n  -> n > 0 && inValueGov + (govSingleton ac n) == outValueGov
      GRWithdraw n -> n > 0 && inValueGov - (govSingleton ac n) == outValueGov

    checkCorrectDatumUpdate =
      redeemer == (gdLastRedeemer outputDatum) &&
      xGovCS == (gdxGovCurrencySymbol outputDatum) &&
      xGovTN == (gdxGovTokenName outputDatum)
              
govInstance :: PubKeyHash -> AssetClassGov -> Validators.TypedValidator Governance
govInstance pkh gov = Validators.mkTypedValidator @Governance
  ($$(PlutusTx.compile [|| mkValidator ||])
   `PlutusTx.applyCode` PlutusTx.liftCode pkh
   `PlutusTx.applyCode` PlutusTx.liftCode gov)
  $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Validators.wrapValidator @GovernanceDatum @GovernanceRedeemer

{-# INLINABLE scrValidator #-}
govValidator :: PubKeyHash -> AssetClassGov -> Validator
govValidator = Validators.validatorScript . govInstance

govAddress :: PubKeyHash -> AssetClassGov -> Ledger.Address
govAddress = scriptAddress . govValidator

govSingleton :: AssetClassGov -> Integer -> Value
govSingleton AssetClassGov{..} = Value.singleton acGovCurrencySymbol acGovTokenName

xgovSingleton :: PubKeyHash -> AssetClassGov -> Integer -> Value
xgovSingleton pkh gov = Value.singleton (xGovCurrencySymbol pkh gov) (acGovTokenName gov)

-- xGOV minting policy
{-# INLINABLE mkPolicy #-}
mkPolicy :: AssetClassGov -> ValidatorHash -> ScriptContext -> Bool
mkPolicy AssetClassGov{..} valh ctx =
  checkScrInTransaction &&
  checkEndpointCorrect
  where 
    info = scriptContextTxInfo ctx

    isGov (ScriptCredential v) = v == valh
    isGov _                    = False

    getGovernanceIn :: TxOut
    getGovernanceIn = case filter isGov . map (addressCredential . txOutAddress . txInInfoResolved) $ txInfoInputs info of
      [o] -> o
      _   -> traceError "expected only one governance script in input"

    getGovernanceOut :: TxOut
    getGovernanceOut = case filter isGov . map (addressCredential . txOutAddress) $ txInfoOutputs info of
      [o] -> o
      _   -> traceError "expected only one governance script in output"

    isCorrectForgeAmount :: Integer -> Bool
    isCorrectForgeAmount n =
      case AssocMap.lookup (ownCurrencySymbol ctx) . Value.getValue $ txInfoForge info of
        Nothing -> traceError "no currency minted"
        Just mp -> case AssocMap.lookup acGovTokenName mp of
          Nothing -> traceError "wrong TokenName"
          Just m  -> traceIfFalse "wrong amount in redeemer" $ n == m

    -- checks
    
    checkScrInTransaction :: Bool
    checkScrInTransaction = getGovernanceIn `Hask.seq` True 

    checkEndpointCorrect :: Bool
    checkEndpointCorrect = case txOutDatumHash getGovernanceOut of
        Nothing -> traceError "no datum hash on governance"
        Just h  -> case findDatum h info of
          Nothing        -> traceError "no datum on governance"
          Just (Datum d) -> case PlutusTx.fromBuiltinData d of
            Nothing -> traceError "no datum parse"            
            Just gd -> case gdLastRedeemer gd of
              (GRWithdraw n) -> isCorrectForgeAmount (negate n)
              (GRDeposit n)  -> isCorrectForgeAmount n
         
xGovMintingPolicy :: PubKeyHash -> AssetClassGov -> MintingPolicy
xGovMintingPolicy pkh gov = mkMintingPolicyScript $
  $$(PlutusTx.compile [|| (wrapMintingPolicy .). mkPolicy ||])
  `PlutusTx.applyCode` PlutusTx.liftCode gov

-- | takes in the GOV CurrencySymbol, returns the xGOV CurrencySymbol
xGovCurrencySymbol :: PubKeyHash -> AssetClassGov -> CurrencySymbol
xGovCurrencySymbol pkh = scriptCurrencySymbol . xGovMintingPolicy pkh
