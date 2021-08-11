{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

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
data AssetClassNft = AssetClassNft {
    acNftCurrencySymbol :: !CurrencySymbol
  , acNftTokenName :: !TokenName
  } deriving (Hask.Show, Hask.Eq, Generic, ToJSON, FromJSON, ToSchema)

instance Eq AssetClassNft where
  {-# INLINABLE (==) #-}
  n1 == n2 = acNftCurrencySymbol n1 == acNftCurrencySymbol n2
    && acNftTokenName n1 == acNftTokenName n2

PlutusTx.unstableMakeIsData ''AssetClassNft
PlutusTx.makeLift ''AssetClassNft

data AssetClassGov = AssetClassGov {
    acGovCurrencySymbol :: !CurrencySymbol
  , acGovTokenName :: !TokenName
  } deriving (Hask.Show, Generic, ToJSON, FromJSON, ToSchema)

instance Eq AssetClassGov where
  {-# INLINABLE (==) #-}
  n1 == n2 = acGovCurrencySymbol n1 == acGovCurrencySymbol n2
    && acGovTokenName n1 == acGovTokenName n2

PlutusTx.unstableMakeIsData ''AssetClassGov
PlutusTx.makeLift ''AssetClassGov

data GovernanceRedeemer = GRDeposit !PubKeyHash !Integer | GRWithdraw !PubKeyHash !Integer
  deriving Hask.Show

instance Eq GovernanceRedeemer where
  {-# INLINABLE (==) #-}
  (GRDeposit pkh1 n1) == (GRDeposit pkh2 n2) = pkh1 == pkh2 && n1 == n2
  (GRWithdraw pkh1 n1) == (GRWithdraw pkh2 n2) = pkh1 == pkh2 && n1 == n2
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
instance Validators.ValidatorTypes Governance where
    type instance DatumType Governance = GovernanceDatum
    type instance RedeemerType Governance = GovernanceRedeemer

-- Validator of the governance contract
{-# INLINABLE mkValidator #-}
mkValidator :: AssetClassNft -> AssetClassGov -> CurrencySymbol -> GovernanceDatum -> GovernanceRedeemer -> ScriptContext -> Bool
mkValidator nft gov xgovCS govDatum redeemer ctx =
  checkOutputHasNft &&
  checkCorrectLastRedeemer &&
  checkCorrectDepositMap &&
  checkCorrectPayment &&
  checkForging
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

    ownInput :: Contexts.TxOut
    ownInput = case findOwnInput ctx of
      Nothing -> traceError "no self in input"
      Just tx -> txInInfoResolved tx
    
    ownOutput :: Contexts.TxOut
    outputDatum :: GovernanceDatum
    (ownOutput, outputDatum) = case Contexts.getContinuingOutputs ctx of
      [o] -> case txOutDatumHash o of
        Nothing -> traceError "wrong output type"
        Just h  -> case findDatum h info of
          Nothing        -> traceError "datum not found"
          Just (Datum d) -> case PlutusTx.fromBuiltinData d of
            Just gd -> (o, gd)
            Nothing -> traceError "error decoding data"
      _ -> traceError "expected one continuing output"

    isMinting :: Bool
    isMinting = case AssocMap.lookup xgovCS . Value.getValue $ txInfoForge info of
      Nothing -> False
      Just _  -> True

    -- on which endpoints the minting script needs to be invoked
    checkForging :: Bool
    checkForging = case redeemer of
      GRDeposit  _ _ -> isMinting  
      GRWithdraw _ _ -> isMinting

    checkCorrectPayment = case redeemer of
      -- we don't care about from whom the payment came
      GRDeposit _ n  -> traceIfFalse "incorrect value paid to script" $
        txOutValue ownInput Hask.<> govSingleton gov n == txOutValue ownOutput
      GRWithdraw pkh n -> case AssocMap.lookup xgovCS . Value.getValue $ (userInput pkh) of -- this may have the same issue that gov had
        Nothing -> traceError "no xGOV paid"
        Just mp -> traceIfFalse "wrong amount told in redeemer" . (== n) . sum . map snd $ AssocMap.toList mp

    checkOutputHasNft = Value.valueOf (txOutValue ownOutput) (acNftCurrencySymbol nft) (acNftTokenName nft) == 1    
    
    checkCorrectLastRedeemer = traceIfFalse "wrong last endpoint record in datum"
      $ redeemer == (gdLastRedeemer outputDatum)

    checkCorrectDepositMap = case redeemer of
      GRDeposit pkh n ->
        let prev = maybe 0 id $ AssocMap.lookup pkh (gdDepositMap govDatum)
            newMap = AssocMap.insert pkh (n+prev) (gdDepositMap govDatum)
        in traceIfFalse "wrong update of deposit map" $ newMap == (gdDepositMap outputDatum)
      GRWithdraw pkh n ->
        let newMap =
              foldr (\(tn, amm) mp ->
                let prev = maybe (traceError "withdraw from non-recorded deposit") id $ AssocMap.lookup (coerce tn) mp
                    newMapInner = case prev - amm of
                      p | p > 0  -> AssocMap.insert (coerce tn) p mp
                      p | p == 0 -> AssocMap.delete (coerce tn) mp
                      _          -> traceError "withdraw into negative - non-recorded deposit"
                in  newMapInner
                )
                (gdDepositMap govDatum) $
                case AssocMap.lookup xgovCS $ Value.getValue (userInput pkh) of
                  Nothing -> traceError "no xGOV paid"
                  Just mp -> AssocMap.toList $ mp          
        in traceIfFalse "wrong update of deposit map" (newMap == (gdDepositMap outputDatum)) &&
          case Value.flattenValue (valuePaidTo info pkh) of -- possibly need to change this here
          [(csym, tn, amm)] | amm == n -> traceIfFalse "non-GOV payment by script on withdrawal"
                                            $ AssetClassGov csym tn == gov
          [_]                          -> traceError "imbalanced ammount of xGOV to GOV"
          _                            -> traceError "more than one assetclass paid by script"

scrInstance :: AssetClassNft -> AssetClassGov -> Validators.TypedValidator Governance
scrInstance nft gov = Validators.mkTypedValidator @Governance
  ($$(PlutusTx.compile [|| mkValidator ||])
   `PlutusTx.applyCode` PlutusTx.liftCode nft
   `PlutusTx.applyCode` PlutusTx.liftCode gov
   `PlutusTx.applyCode` PlutusTx.liftCode (xGovCurrencySymbol nft))
  $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Validators.wrapValidator @GovernanceDatum @GovernanceRedeemer

{-# INLINABLE scrValidator #-}
scrValidator :: AssetClassNft -> AssetClassGov -> Validator
scrValidator nft = Validators.validatorScript . scrInstance nft

scrAddress :: AssetClassNft -> AssetClassGov -> Ledger.Address
scrAddress nft = scriptAddress . scrValidator nft

govSingleton :: AssetClassGov -> Integer -> Value
govSingleton AssetClassGov{..} = Value.singleton acGovCurrencySymbol acGovTokenName

xgovSingleton :: AssetClassNft -> TokenName -> Integer -> Value
xgovSingleton nft tok = Value.singleton (xGovCurrencySymbol nft) tok

-- xGOV minting policy
{-# INLINABLE mkPolicy #-} -- there's something wrong with this 'unit' hack.
mkPolicy :: AssetClassNft -> () -> ScriptContext -> Bool
mkPolicy nft _ ctx =
  traceIfFalse "governance script not in transaction" checkScrInTransaction &&
  checkEndpointCorrect
  where 
    info = scriptContextTxInfo ctx

    hasNft utxo = Value.valueOf (txOutValue utxo) (acNftCurrencySymbol nft) (acNftTokenName nft) == 1

    -- may be an unnescesary check
    checkScrInTransaction :: Bool
    checkScrInTransaction = any hasNft . map txInInfoResolved $ txInfoInputs info

    checkEndpointCorrect :: Bool
    checkEndpointCorrect = case find hasNft $ txInfoOutputs info of
      Nothing -> False
      Just o  -> case txOutDatumHash o of
        Nothing -> False
        Just h  -> case findDatum h info of
          Nothing        -> False
          Just (Datum d) -> case PlutusTx.fromBuiltinData d of
            Nothing -> False            
            Just gd -> case gdLastRedeemer gd of
              (GRWithdraw _ n) ->
                traceIfFalse "burned xGOV not equal to specified amount"
                  $ isCorrectBurnAmount n
              (GRDeposit pkh n)  ->
                traceIfFalse "endpoint called on governance does not permit minting of xGOV"
                  $ isCorrectTokenName pkh n 

    -- is that how burning gets signalled? by having negative forge? it must be.
    isCorrectBurnAmount :: Integer -> Bool
    isCorrectBurnAmount n =
      case AssocMap.lookup (ownCurrencySymbol ctx) . Value.getValue $ txInfoForge info of
        Nothing -> traceError "no currency minted"
        Just mp -> (== n) . negate . sum . map snd $ AssocMap.toList mp

    isCorrectTokenName :: PubKeyHash -> Integer -> Bool
    isCorrectTokenName pkh n =
      case AssocMap.lookup (ownCurrencySymbol ctx) . Value.getValue $ txInfoForge info of
        Nothing -> traceError "no currency minted"
        Just mp -> case AssocMap.toList mp of
          [(tn, amm)] ->
            traceIfFalse "wrong ammount of xGOV minted" (amm == n) &&
            traceIfFalse "wrong TokenName minted" (tn == (coerce pkh))
          _ -> traceError "expected exactly one token minted under xGOV CurrencySymbol"
          
xGovMintingPolicy :: AssetClassNft -> MintingPolicy
xGovMintingPolicy nft = mkMintingPolicyScript $
  $$(PlutusTx.compile [|| wrapMintingPolicy . mkPolicy ||])
  `PlutusTx.applyCode` PlutusTx.liftCode nft

-- | takes in the GOV CurrencySymbol, returns the xGOV CurrencySymbol
xGovCurrencySymbol :: AssetClassNft -> CurrencySymbol
xGovCurrencySymbol = scriptCurrencySymbol . xGovMintingPolicy
