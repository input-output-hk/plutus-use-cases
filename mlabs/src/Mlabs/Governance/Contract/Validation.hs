{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fobject-code #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}

-- | Validation, on-chain code for governance application 
module Mlabs.Governance.Contract.Validation (
    govAddress
  , govInstance
  , govValidator
  , govSingleton
  , xgovSingleton
  , xGovMintingPolicy
  , xGovCurrencySymbol
  , Governance
  , GovernanceDatum(..)
  , GovernanceRedeemer(..)
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
    gdPubKeyHash :: !PubKeyHash
  , gdxGovCurrencySymbol :: !CurrencySymbol
  } deriving Hask.Show

PlutusTx.unstableMakeIsData ''GovernanceDatum
PlutusTx.makeLift ''GovernanceDatum

data Governance
instance Validators.ValidatorTypes Governance where
    type instance DatumType Governance = GovernanceDatum
    type instance RedeemerType Governance = GovernanceRedeemer

-- | governance validator
{-# INLINABLE govValidator #-}
mkValidator :: AssetClassGov -> GovernanceDatum -> GovernanceRedeemer -> ScriptContext -> Bool
mkValidator gov datum redeemer ctx =
  traceIfFalse "incorrect value from redeemer"         checkCorrectValue &&
  traceIfFalse "incorrect minting script involvenment" checkForging &&
  traceIfFalse "invalid datum update"                  checkCorrectDatumUpdate 
  where 
    info = scriptContextTxInfo ctx

    ownInput :: Contexts.TxInInfo
    ownInput = case findOwnInput ctx of
      Just o  -> o
      Nothing -> traceError "no self in input"

    ownOutput :: Contexts.TxOut
    ownOutput = case Contexts.getContinuingOutputs ctx of
      [o] -> o -- this may crash here, may need to filter by pkh
      _   -> traceError "expected exactly one continuing output"

    outputDatum :: GovernanceDatum
    outputDatum = case txOutDatumHash ownOutput of
      Nothing -> traceError "no datum hash on governance"
      Just h  -> case findDatum h info of
        Nothing        -> traceError "no datum on governance"
        Just (Datum d) -> case PlutusTx.fromBuiltinData d of
          Nothing -> traceError "no datum parse"            
          Just gd -> gd

    valueIn :: Value
    valueIn = txOutValue $ txInInfoResolved ownInput

    valueOut :: Value
    valueOut = txOutValue ownOutput

    pkh :: PubKeyHash
    pkh = gdPubKeyHash datum

    xGov :: CurrencySymbol
    xGov = gdxGovCurrencySymbol datum

    --- checks

    checkForging :: Bool
    checkForging = case AssocMap.lookup xGov . Value.getValue $ txInfoForge info of
      Nothing -> False
      Just mp -> case (redeemer, AssocMap.lookup (coerce pkh) mp) of
        (GRDeposit n, Just m)  -> n == m
        (GRWithdraw n, Just m) -> n == negate m
        _                      -> False
  
    checkCorrectValue :: Bool
    checkCorrectValue = case redeemer of
      GRDeposit n  -> n > 0 && valueIn + (govSingleton gov n) == valueOut
      GRWithdraw n -> n > 0 && valueIn - (govSingleton gov n) == valueOut

    checkCorrectDatumUpdate :: Bool
    checkCorrectDatumUpdate =
      pkh == gdPubKeyHash outputDatum &&
      xGov == gdxGovCurrencySymbol outputDatum
              
govInstance :: AssetClassGov -> Validators.TypedValidator Governance
govInstance gov = Validators.mkTypedValidator @Governance
  ($$(PlutusTx.compile [|| mkValidator ||])
   `PlutusTx.applyCode` PlutusTx.liftCode gov)
  $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Validators.wrapValidator @GovernanceDatum @GovernanceRedeemer

govValidator :: AssetClassGov -> Validator
govValidator = Validators.validatorScript . govInstance

govAddress :: AssetClassGov -> Ledger.Address
govAddress = scriptAddress . govValidator

{-# INLINABLE govSingleton #-}
govSingleton :: AssetClassGov -> Integer -> Value
govSingleton AssetClassGov{..} = Value.singleton acGovCurrencySymbol acGovTokenName

xgovSingleton :: AssetClassGov -> PubKeyHash -> Integer -> Value
xgovSingleton gov pkh = Value.singleton (xGovCurrencySymbol gov) (coerce pkh)

-- xGOV minting policy
{-# INLINABLE mkPolicy #-}
mkPolicy :: ValidatorHash -> AssetClassGov -> () -> ScriptContext -> Bool
mkPolicy vh AssetClassGov{..} _ ctx =
  traceIfFalse "attempt to mint unpaid-for xGOV" checkMintedSubsetGovDeposits
  where 
    info = scriptContextTxInfo ctx

    isGov (ScriptCredential v) = v == vh
    isGov _                    = False

    getGovernanceIn :: [TxOut]
    getGovernanceIn = filter (isGov. addressCredential . txOutAddress) . map txInInfoResolved $ txInfoInputs info 
      
    getGovernanceOut :: [TxOut]
    getGovernanceOut = filter (isGov . addressCredential . txOutAddress) $ txInfoOutputs info 

    -- how much GOV sits 'at every pkh'
    pkhWithGov :: [TxOut] -> [(PubKeyHash, Integer)]
    pkhWithGov inout = inout >>= extractDatum
      where
        extractDatum utxo = case txOutDatumHash utxo of
          Nothing -> traceError "no datum hash on governance"
          Just h  -> case findDatum h info of
            Nothing        -> traceError "no datum on governance"
            Just (Datum d) -> case PlutusTx.fromBuiltinData d of
              Nothing -> traceError "no datum parse"            
              Just gd -> case AssocMap.lookup acGovCurrencySymbol . Value.getValue $ txOutValue utxo of
                Nothing -> [] -- just in case someone puts some other tokens in the script
                Just mp -> [(gdPubKeyHash gd, snd . head $ AssocMap.toList mp)]

    differenceGovDeposits :: [(PubKeyHash, Integer)]
    differenceGovDeposits = filter ((>0) . snd) $ foldr foo [] (pkhWithGov getGovernanceOut)
      where
        inMap = AssocMap.fromList $ pkhWithGov getGovernanceIn

        foo (pkh, n) xs = case AssocMap.lookup pkh inMap of
          Nothing -> (pkh, n):xs
          Just m  -> (pkh, n-m):xs

    mintedDeposit :: [(TokenName, Integer)]
    mintedDeposit = case AssocMap.lookup (ownCurrencySymbol ctx) . Value.getValue $ txInfoForge info of
      Nothing -> traceError "no self minting"
      Just mp -> filter ((>0) . snd) $ AssocMap.toList mp

    -- checks

    -- mintedDeposit \subseteq differenceGovDeposits => minteddeposit == differencegovdeposits
    checkMintedSubsetGovDeposits :: Bool
    checkMintedSubsetGovDeposits = foldr memb True (map (\(a,b) -> (coerce a,b)) mintedDeposit)
      where
        memb pair b = (b &&) . foldr (||) False $ map (== pair) differenceGovDeposits 
        -- yes, I've only done it ^this way so that it compiles
          
xGovMintingPolicy :: AssetClassGov -> MintingPolicy
xGovMintingPolicy gov = mkMintingPolicyScript $
  $$(PlutusTx.compile [|| (wrapMintingPolicy .). mkPolicy ||])
  `PlutusTx.applyCode` PlutusTx.liftCode (validatorHash $ govValidator gov)
  `PlutusTx.applyCode` PlutusTx.liftCode gov

-- | takes in the GOV CurrencySymbol, returns the xGOV CurrencySymbol
xGovCurrencySymbol :: AssetClassGov -> CurrencySymbol
xGovCurrencySymbol = scriptCurrencySymbol . xGovMintingPolicy
