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
import GHC.Generics (Generic)
import Playground.Contract (FromJSON, ToJSON, ToSchema)
import PlutusTx.AssocMap qualified as AssocMap
import PlutusTx qualified
import PlutusTx.Prelude hiding (Semigroup(..), unless)
import Ledger hiding (before, after)
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

data GovernanceRedeemer = GRDeposit !PubKeyHash !Integer | GRWithdraw !PubKeyHash
  deriving Hask.Show

instance Eq GovernanceRedeemer where
  {-# INLINABLE (==) #-}
  (GRDeposit pkh1 n1) == (GRDeposit pkh2 n2) = pkh1 == pkh2 && n1 == n2
  (GRWithdraw pkh1) == (GRWithdraw pkh2) = pkh1 == pkh2
  _ == _ = False

PlutusTx.unstableMakeIsData ''GovernanceRedeemer
PlutusTx.makeLift ''GovernanceRedeemer

data GovernanceDatum = GovernanceDatum {
    gdLastRedeemer :: !GovernanceRedeemer
  , gdDepositMap :: !(AssocMap.Map PubKeyHash Integer)
  } deriving Hask.Show

PlutusTx.unstableMakeIsData ''GovernanceDatum
PlutusTx.makeLift ''GovernanceDatum

data Governance
instance Scripts.ScriptType Governance where
    type instance DatumType Governance = GovernanceDatum
    type instance RedeemerType Governance = GovernanceRedeemer

-- Validator of the governance contract
{-# INLINABLE mkValidator #-}
mkValidator :: AssetClassNft -> AssetClassGov -> CurrencySymbol -> GovernanceDatum -> GovernanceRedeemer -> ScriptContext -> Bool
mkValidator nft gov xgovCS govDatum redeemer ctx =
  checkOutputHasNft &&
  checkCorrectLastRedeemer &&
  checkCorrectDepositMap &&
  checkCorrectPayment
  where
    info :: Contexts.TxInfo
    info = scriptContextTxInfo ctx

    -- honestly we could tweak this a bit. TBD.
    userInput :: PubKeyHash -> Value
    userInput pkh =
      let isByPkh x = case Address.addressCredential . txOutAddress $ txInInfoResolved x of
            PubKeyCredential key -> key == pkh 
            _                    -> False
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

    checkCorrectPayment = traceError "incorrect payment made" $ case redeemer of
      GRDeposit pkh n -> Value.valueOf (userInput pkh) (acGovCurrencySymbol gov) (acGovTokenName gov) == n
      GRWithdraw _    -> True -- handled in checkCorrectDepositMap

    checkOutputHasNft = Value.valueOf (txOutValue ownOutput) (acNftCurrencySymbol nft) (acNftTokenName nft) == 1    
    
    checkCorrectLastRedeemer = traceIfFalse "wrong last endpoint record in datum"
      $ redeemer == (gdLastRedeemer outputDatum)

    checkCorrectDepositMap = case redeemer of
      GRDeposit pkh _ ->
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
              xs@(_:_) | all isxGovCorrect xs -> sum $ map (\(_,_,amm) -> amm) xs
                 where
                   isxGovCorrect (csym, tn, amm) | csym == xgovCS = 
                     case AssocMap.lookup (coerce tn) (gdDepositMap govDatum) of
                       Nothing -> traceError "detected unregistered xGOV tokens"
                       Just before  -> case AssocMap.lookup (coerce tn) (gdDepositMap outputDatum) of
                         Nothing    | amm == before         -> True
                         Just after | before == after + amm -> True
                         Nothing    | amm > before          -> traceError "detected unregistered xGOV tokens"
                         Nothing                            -> traceError "premature erasure of deposit record"
                         Just after | before > after + amm  -> traceError "loss of tokens in datum"
                         Just _                             -> traceError "withdrawal of too many tokens in datum"
                   isxGovCorrect _ = traceError "non-xGOV tokens paid by pkh" 
              _ -> traceError "no payments made" 
        in case Value.flattenValue (valuePaidTo info pkh) of
          [(csym, tn, amm)] | amm == paidxGov -> traceIfFalse "non-GOV payment by script on withdrawal"
                                                   $ AssetClassGov csym tn == gov
          [_]                                 -> traceError "imbalanced ammount of xGOV to GOV"
          _                                   -> traceError "more than one assetclass paid by script"

scrInstance :: AssetClassNft -> AssetClassGov -> Scripts.ScriptInstance Governance
scrInstance nft gov = Scripts.validator @Governance
  ($$(PlutusTx.compile [|| mkValidator ||])
   `PlutusTx.applyCode` PlutusTx.liftCode nft
   `PlutusTx.applyCode` PlutusTx.liftCode gov
   `PlutusTx.applyCode` PlutusTx.liftCode (xGovCurrencySymbol nft))
  $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @(Scripts.DatumType Governance) @(Scripts.RedeemerType Governance)

{-# INLINABLE scrValidator #-}
scrValidator :: AssetClassNft -> AssetClassGov -> Validator
scrValidator nft = Scripts.validatorScript . scrInstance nft

scrAddress :: AssetClassNft -> AssetClassGov -> Ledger.Address
scrAddress nft = scriptAddress . scrValidator nft

govValueOf :: AssetClassGov -> Integer -> Value
govValueOf AssetClassGov{..} = Value.singleton acGovCurrencySymbol acGovTokenName

xgovValueOf :: CurrencySymbol -> TokenName -> Integer -> Value
xgovValueOf csym tok = Value.singleton csym tok

-- xGOV minting policy, the parameter is the NFT HELD BY THE GOVERNANCE SCRIPT
{-# INLINABLE mkPolicy #-}
mkPolicy :: AssetClassNft -> ScriptContext -> Bool
mkPolicy nft ctx =
  traceIfFalse "governance script not in transaction" checkScrInTransaction &&
  traceIfFalse "endpoint called on governance does not permit minting of xGOV" checkIsMintingEndpoint
  where 
    info = scriptContextTxInfo ctx

    hasNft utxo = Value.valueOf (txOutValue utxo) (acNftCurrencySymbol nft) (acNftTokenName nft) == 1

    -- may be an unnescesary check
    checkScrInTransaction :: Bool
    checkScrInTransaction = any hasNft . map txInInfoResolved $ txInfoInputs info

    checkIsMintingEndpoint :: Bool
    checkIsMintingEndpoint = case find hasNft $ txInfoOutputs info of
      Nothing -> False
      Just o  -> case txOutDatumHash o of
        Nothing -> False
        Just h  -> case findDatum h info of
          Nothing        -> False
          Just (Datum d) -> case PlutusTx.fromData d of
            Nothing -> False            
            Just gd -> case gdLastRedeemer gd of
              (GRWithdraw _)  -> False
              (GRDeposit pkh n) -> isCorrectTokenName pkh n 

    isCorrectTokenName :: PubKeyHash -> Integer -> Bool
    isCorrectTokenName pkh n =
      case AssocMap.lookup (ownCurrencySymbol ctx) . Value.getValue $ txInfoForge info of
        Nothing   -> traceError "no currency minted"
        (Just mp) -> case AssocMap.toList mp of
          [(tn, amm)] ->
            traceIfFalse "wrong ammount of xGOV minted" (amm == n) &&
            traceIfFalse "wrong TokenName minted" (tn == (coerce pkh))
          _ -> traceError "expected exactly one token minted under xGOV CurrencySymbol"
          
xGovMintingPolicy :: AssetClassNft -> Scripts.MonetaryPolicy
xGovMintingPolicy nft = mkMonetaryPolicyScript $
  $$(PlutusTx.compile [|| Scripts.wrapMonetaryPolicy . mkPolicy ||])
  `PlutusTx.applyCode` PlutusTx.liftCode nft

-- | takes in the GOV CurrencySymbol, returns the xGOV CurrencySymbol
xGovCurrencySymbol :: AssetClassNft -> CurrencySymbol
xGovCurrencySymbol = scriptCurrencySymbol . xGovMintingPolicy
