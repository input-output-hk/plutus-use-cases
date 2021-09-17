{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE DerivingStrategies    #-}

module Contracts.Oracle.OnChain
    ( typedOracleValidator
    , oracleValidator
    , oracleAddress
    , oracleScriptAsShortBs
    , oraclePlutusScript
    , verifyOracleValueSigned
    ) where

import           Cardano.Api.Shelley       (PlutusScript (..), PlutusScriptV1)
import           Contracts.Types     
import           Control.Monad             hiding (fmap)
import           Codec.Serialise
import           Data.Aeson                (FromJSON, ToJSON)
import qualified Data.ByteString.Short     as SBS
import qualified Data.ByteString.Lazy      as LBS
import qualified Data.Map                  as Map
import           Data.Maybe                (catMaybes)
import           Data.Monoid               (Last (..))
import           Data.Text                 (Text, pack)
import qualified Data.List.NonEmpty        as NonEmpty
import           GHC.Generics              (Generic)
import           Plutus.Contract           as Contract
import qualified PlutusTx
import           PlutusTx.Prelude          hiding (Semigroup(..), unless)
import           Ledger                    hiding (singleton, MintingPolicyHash)
import qualified Ledger.Scripts            as LedgerScripts
import qualified Ledger.Tx                 as LedgerScripts
import           Ledger.Constraints        as Constraints
import qualified Ledger.Contexts           as Validation
import           Ledger.Oracle             (Observation, SignedMessage(..), signMessage, SignedMessageCheckError(..), checkSignature)
import qualified Ledger.Typed.Scripts      as Scripts
import           Ledger.Value              as Value
import           Ledger.Ada                as Ada
import           Plutus.Contracts.Currency as Currency
import           Plutus.Contract.Types     (Promise (..))
import           Prelude                   (Semigroup (..), Show (..), String)
import qualified Prelude                   as Haskell
import           Schema                    (ToSchema)
import Contracts.Oracle.Types
import Contracts.Oracle.RequestToken

{-# INLINABLE mkOracleValidator #-}
mkOracleValidator :: Oracle -> OracleData -> OracleRedeemer -> ScriptContext -> Bool
mkOracleValidator oracle oracleData r ctx =
    traceIfFalse "request token missing from input" inputHasRequestToken  &&
    case r of
        Use    -> traceIfFalse "signed by request owner" (txSignedBy info $ ovRequestAddress oracleData )
                  && traceIfFalse "value signed by oracle" (isValueSigned)
                  && traceIfFalse "expected requester to get oracle token" 
                     (sentToAddress (Just $ ovRequestAddress oracleData) $ requestTokenExpectedVal)
        Update -> traceIfFalse "operator signature missing" (txSignedBy info $ oOperator oracle) 
                  && traceIfFalse "invalid output datum" validOutputDatum

  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    forged :: Value
    forged = txInfoMint $ scriptContextTxInfo ctx

    requestTokenExpectedVal:: Value
    requestTokenExpectedVal = Value.singleton (oRequestTokenSymbol oracle) oracleTokenName 1

    requestTokenValOf:: Value -> Integer 
    requestTokenValOf value = valueOf (txOutValue ownInput) (oRequestTokenSymbol oracle) oracleTokenName

    sentToAddress :: Maybe PubKeyHash -> Value -> Bool
    sentToAddress h v =
        let
        [o] = [ o'
              | o' <- txInfoOutputs info
              , txOutValue o' == v
              ]
        in
        fromMaybe False ((==) <$> Validation.pubKeyOutput o <*> h )

    ownInput :: TxOut
    ownInput = case findOwnInput ctx of
        Nothing -> traceError "oracle input missing"
        Just i  -> txInInfoResolved i

    inputHasRequestToken :: Bool
    inputHasRequestToken = requestTokenValOf (txOutValue ownInput) == 1

    ownOutput :: TxOut
    ownOutput = case getContinuingOutputs ctx of
        [o] -> o
        _   -> traceError "expected exactly one oracle request output"

    -- outputHasToken :: Bool
    -- outputHasToken = assetClassValueOf (txOutValue ownOutput) (oracleAsset oracle) == 1

    outputDatumMaybe :: Maybe OracleData
    outputDatumMaybe = oracleValue ownOutput (`findDatum` info)

    validOutputDatum :: Bool
    validOutputDatum = isJust outputDatumMaybe

    outputDatum :: OracleData
    outputDatum = case outputDatumMaybe of
        Nothing -> traceError "Input data is invalid"
        Just h  -> h

    isValueSigned = isJust $ verifyOracleValueSigned (ovWinnerSigned oracleData) (oOperatorKey oracle)

    feesPaid :: Bool
    feesPaid =
      let
        inVal  = txOutValue ownInput
        outVal = txOutValue ownOutput
      in
        outVal `geq` (inVal <> Ada.lovelaceValueOf (oFee oracle))

{-# INLINABLE verifyOracleValueSigned #-}
verifyOracleValueSigned :: Maybe (SignedMessage Integer) -> PubKey -> Maybe Integer
verifyOracleValueSigned smMaybe pk =
    case smMaybe of
        Nothing -> Nothing
        Just sm@SignedMessage{osmMessageHash, osmSignature, osmDatum=Datum dv} -> do
            case checkSignature osmMessageHash pk osmSignature of
                Left err -> Nothing
                Right _  -> maybe (Nothing) pure (PlutusTx.fromBuiltinData dv) 


data Oracling
instance Scripts.ValidatorTypes Oracling where
    type instance DatumType Oracling = OracleData
    type instance RedeemerType Oracling = OracleRedeemer

typedOracleValidator :: Oracle -> Scripts.TypedValidator Oracling
typedOracleValidator oracle = Scripts.mkTypedValidator @Oracling
    ($$(PlutusTx.compile [|| mkOracleValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode oracle)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @OracleData @OracleRedeemer

oracleValidator :: Oracle -> Validator
oracleValidator = Scripts.validatorScript . typedOracleValidator

oracleValidatorHash :: Oracle -> Ledger.ValidatorHash
oracleValidatorHash oracle = LedgerScripts.validatorHash . oracleValidator $ oracle

oracleAddress :: Oracle -> Ledger.Address
oracleAddress = scriptAddress . oracleValidator

oracleScriptAsShortBs :: Oracle -> SBS.ShortByteString
oracleScriptAsShortBs = SBS.toShort . LBS.toStrict . serialise . oracleValidator

oraclePlutusScript :: Oracle -> PlutusScript PlutusScriptV1
oraclePlutusScript = PlutusScriptSerialised . oracleScriptAsShortBs