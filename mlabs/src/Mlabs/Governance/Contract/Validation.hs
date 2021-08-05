{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

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
  , GovernanceRedeemer(..)
  , AssetClassNft(..)
  , AssetClassGov(..)
  ) where

import Data.Coerce (coerce)
import Data.List (sortOn)
import GHC.Generics (Generic)
import Playground.Contract (FromJSON, ToJSON, ToSchema)
import PlutusTx.AssocMap qualified as AssocMap
import PlutusTx qualified
import PlutusTx.Prelude hiding (Semigroup(..), unless)
import Ledger hiding (singleton)
import Ledger.Typed.Scripts      qualified as Scripts
import Plutus.V1.Ledger.Value    qualified as Value
import Plutus.V1.Ledger.Contexts qualified as Contexts
import Plutus.V1.Ledger.Address  qualified as Address
import Plutus.V1.Ledger.Credential (Credential(..))
import Prelude qualified as Hask 

-- TODO: Once AssetClass has a ToSchema instance, change this to a newtype.
--       or not. this is fine really. 
data AssetClassNft = AssetClassNft {
    acNftCurrencySymbol :: !CurrencySymbol
  , acNftTokenName :: !TokenName
  } deriving (Show, Hask.Eq, Generic, ToJSON, FromJSON, ToSchema)

instance Eq AssetClassNft where
  {-# INLINABLE (==) #-}
  n1 == n2 = acNftCurrencySymbol n1 == acNftCurrencySymbol n2
    && acNftTokenName n1 == acNftTokenName n2

PlutusTx.unstableMakeIsData ''AssetClassNft
PlutusTx.makeLift ''AssetClassNft

data AssetClassGov = AssetClassGov {
    acGovCurrencySymbol :: !CurrencySymbol
  , acGovTokenName :: !TokenName
  } deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

instance Eq AssetClassGov where
  {-# INLINABLE (==) #-}
  n1 == n2 = acGovCurrencySymbol n1 == acGovCurrencySymbol n2
    && acGovTokenName n1 == acGovTokenName n2

PlutusTx.unstableMakeIsData ''AssetClassGov
PlutusTx.makeLift ''AssetClassGov

-- there's a discussion to be had about whether we want the AssetClasses to parametrize
-- the contract or just sit in the datum. 
data GovernanceDatum = GovernanceDatum {
    gdDepositMap :: !(AssocMap.Map PubKeyHash Integer)
  } deriving (Hask.Show, Generic, ToJSON, FromJSON, ToSchema)

PlutusTx.unstableMakeIsData ''GovernanceDatum
PlutusTx.makeLift ''GovernanceDatum

data GovernanceRedeemer = GRDeposit !PubKeyHash | GRWithdraw !PubKeyHash
  deriving (Hask.Show, Generic)

PlutusTx.unstableMakeIsData ''GovernanceRedeemer

data Governance
instance Scripts.ScriptType Governance where
    type instance DatumType Governance = GovernanceDatum
    type instance RedeemerType Governance = GovernanceRedeemer

-- Validator of the governance contract
{-# INLINABLE mkValidator #-}
mkValidator :: AssetClassNft -> AssetClassGov -> GovernanceDatum -> GovernanceRedeemer -> ScriptContext -> Bool
mkValidator nft gov govDatum redeemer ctx = True {- checkCorrectOutputs
  where
    info :: Contexts.TxInfo
    info = scriptContextTxInfo ctx

    -- honestly we could tweak this a bit. TBD.
    userInput :: PubKeyHash -> Value
    userInput pkh =
      let isByPkh x = case Address.addressCredential . txOutAddress $ txInInfoResolved x of
            PubKeyCredential key | key == pkh -> True
            _                                 -> False
      in case filter isByPkh $ txInfoInputs info of
        [o] -> txOutValue $ txInInfoResolved o
        _   -> traceError "expected exactly one payment from the pkh"
    
    ownOutput :: Contexts.TxOut
    outputDatum :: GovernanceDatum
    (ownOutput, outputDatum) = case Contexts.getContinuingOutputs ctx of
      [o] -> case txOutDatumHash o of
        Nothing -> traceError "wrong output type"
        Just h  -> case findDatum h info of
          Nothing        -> traceError "datum not found"
          Just (Datum d) -> case PlutusTx.fromData d of
            Just gd -> (o, gd)
            Nothing -> traceError "error decoding data"
      _ -> traceError "expected one continuing output"

    checkCorrectOutputs = case redeemer of
        GRDeposit pkh ->
          let prev = maybe 0 id $ AssocMap.lookup pkh (gdDepositMap govDatum)
              paidGov = case Value.flattenValue (userInput pkh) of
                [(csym, tn, amm)] | (AssetClassGov csym tn) == gov -> amm 
                _ -> traceError "incorrect payment type or unnescesary tokens in input"                                                             
          in case AssocMap.lookup pkh (gdDepositMap outputDatum) of
            Just after | after == prev + paidGov -> True 
            Nothing                              -> traceError "no record of user's deposit in datum"
            _                                    -> traceError "incorrect update of user's deposited amount"
        GRWithdraw pkh ->          
          let paidxGov = case Value.flattenValue (userInput pkh) of
                [] -> traceError "no payments made"
                xs | all isxGovCorrect xs -> sum $ map (\(_,_,amm) -> amm) xs
                   where
                     isxGovCorrect (csym, tn, amm) =
                       xGovCurrencySymbol nft gov == csym &&
                       case AssocMap.lookup (coerce tn) (gdDepositMap govDatum) of
                         Nothing -> traceError "detected unregistered xGOV tokens"
                         Just before  -> case AssocMap.lookup (coerce tn) (gdDepositMap outputDatum) of
                           Nothing | amm == before            -> True
                           Just after | before == after + amm -> True
                           Nothing | amm > before             -> traceError "detected unregistered xGOV tokens"
                           Nothing                            -> traceError "premature erasure of deposit record"
                           Just after | before > after + amm  -> traceError "loss of tokens in datum"
                           Just _                             -> traceError "withdrawal of too many tokens in datum"
          in case Value.flattenValue (valuePaidTo info pkh) of
            [(csym, tn, amm)] | amm == paidxGov -> traceIfFalse "non-GOV payment by script on withdrawal"
                                                     $ AssetClassGov csym tn == gov
            [_]                                 -> traceError "imbalanced ammount of xGOV to GOV"
            _                                   -> traceError "more than one assetclass paid by script"
-}
scrInstance :: AssetClassNft -> AssetClassGov -> Scripts.ScriptInstance Governance
scrInstance nft gov = Scripts.validator @Governance
  ($$(PlutusTx.compile [|| mkValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode nft `PlutusTx.applyCode` PlutusTx.liftCode gov)
  $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @(Scripts.DatumType Governance) @(Scripts.RedeemerType Governance)

scrValidator :: AssetClassNft -> AssetClassGov ->Validator
scrValidator nft = Scripts.validatorScript . scrInstance nft

scrAddress :: AssetClassNft -> AssetClassGov -> Ledger.Address
scrAddress nft = scriptAddress . scrValidator nft

govValueOf :: AssetClassGov -> Integer -> Value
govValueOf AssetClassGov{..} = Value.singleton acGovCurrencySymbol acGovTokenName

xgovValueOf :: CurrencySymbol -> TokenName -> Integer -> Value
xgovValueOf csym tok = Value.singleton csym tok

-- xGOV minting policy
{-# INLINABLE mkPolicy #-}
mkPolicy :: AssetClassNft -> AssetClassGov -> ScriptContext -> Bool
mkPolicy nft gov ctx =
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
    (checkGovToScr, govPaidAmm) = case fmap txOutValue . find (\txout -> True {- scrAddress nft gov == txOutAddress txout-} ) $ txInfoOutputs info of
      Nothing  -> (False,0) 
      Just val -> case Value.flattenValue val of
        [(cur, tn, amm)] -> (True {- cur == acGovCurrencySymbol gov && tn == acGovTokenName gov -}, amm)
        _ -> (False,0)
        
xGovMintingPolicy :: AssetClassNft -> AssetClassGov -> Scripts.MonetaryPolicy
xGovMintingPolicy nft gov = mkMonetaryPolicyScript $
  $$(PlutusTx.compile [|| (Scripts.wrapMonetaryPolicy .). mkPolicy ||])
  `PlutusTx.applyCode` PlutusTx.liftCode nft
  `PlutusTx.applyCode` PlutusTx.liftCode gov

-- may be a good idea to newtype these two
-- | takes in the GOV CurrencySymbol, returns the xGOV CurrencySymbol
xGovCurrencySymbol :: AssetClassNft -> AssetClassGov -> CurrencySymbol
xGovCurrencySymbol nft = scriptCurrencySymbol . xGovMintingPolicy nft
