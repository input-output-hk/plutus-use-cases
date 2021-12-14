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
    , oracleValidatorHash
    , oracleAddress
    , oracleScriptAsShortBs
    , oraclePlutusScript
    , verifyOracleValueSigned
    , extractSignedMessage
    , Oracling
    ) where

import           Cardano.Api.Shelley       (PlutusScript (..), PlutusScriptV1)
import           Types.Game    
import           Control.Monad             hiding (fmap)
import           Codec.Serialise
import qualified Data.ByteString.Short     as SBS
import qualified Data.ByteString.Lazy      as LBS
import           Data.Void                 (Void)
import qualified PlutusTx
import           PlutusTx.Prelude          hiding (Semigroup(..), unless)
import           Ledger                    hiding (singleton, MintingPolicyHash)
import qualified Ledger.Scripts            as LedgerScripts
import           Ledger.Constraints        as Constraints
import           Plutus.Contract.Oracle    (SignedMessage(..), signMessage, verifySignedMessageConstraints)
import qualified Ledger.Typed.Scripts      as Scripts
import           Ledger.Value              as Value
import           Ledger.Ada                as Ada
import Contracts.Oracle.Types

{-# INLINABLE mkOracleValidator #-}
mkOracleValidator :: Oracle -> OracleData -> OracleRedeemer -> ScriptContext -> Bool
mkOracleValidator oracle oracleData r ctx =
    traceIfFalse "request token missing from input" inputHasRequestToken 
    && case r of
        OracleRedeem   -> traceIfFalse "signed by request owner" (txSignedBy info $ ovRequestAddress oracleData )
                        && traceIfFalse "should redeem request token" (requestTokenValOf forged == -1)
        Update         -> traceIfFalse "operator signature missing" (txSignedBy info $ (oOperator oracle)) 
                        && traceIfFalse "invalid output datum" validOutputDatum
                        && traceIfFalse "update data is invalid" isUpdateValid
    where 
    info :: TxInfo
    info = scriptContextTxInfo ctx

    forged :: Value
    forged = txInfoMint $ scriptContextTxInfo ctx

    requestTokenValOf:: Value -> Integer 
    requestTokenValOf value = valueOf value (oRequestTokenSymbol oracle) oracleRequestTokenName

    ownInput :: TxOut
    ownInput = case findOwnInput ctx of
        Nothing -> traceError "oracle input missing"
        Just i  -> txInInfoResolved i

    inputHasRequestToken :: Bool
    inputHasRequestToken = requestTokenValOf (txOutValue ownInput) == 1

    ownOutput :: TxOut
    ownOutput = case [ o
                     | o <- getContinuingOutputs ctx
                     , requestTokenValOf (txOutValue o) == 1 &&
                       Ada.fromValue (txOutValue o) == oCollateral oracle
                     ] of
        [o] -> o
        _   -> traceError "expected request token with collateral ada value"

    outputDatumMaybe :: Maybe OracleData
    outputDatumMaybe = oracleValue ownOutput (`findDatum` info)

    validOutputDatum :: Bool
    validOutputDatum = isJust outputDatumMaybe

    oraclePubKey:: PubKey
    oraclePubKey = (oOperatorKey oracle) 

    outputSignedMessage = (outputDatumMaybe >>= ovSignedMessage)
    outputMessage = fromMaybe (traceError "Not signed ouput") $ extractSignedMessage oraclePubKey outputSignedMessage
    
    inputSignedMessage = ovSignedMessage oracleData
    inputMessageMaybe =  extractSignedMessage oraclePubKey inputSignedMessage
    inputMessage = fromMaybe (traceError "Not signed input message") $ inputMessageMaybe

    isInputMessageSigned = isJust $ inputMessageMaybe

    isUpdateValid = (not isInputMessageSigned) || 
        validateGameStatusChanges (osmGameStatus inputMessage) (osmGameStatus outputMessage)


{-# INLINABLE extractSignedMessage #-}
extractSignedMessage :: PubKey -> Maybe (SignedMessage OracleSignedMessage) -> Maybe OracleSignedMessage
extractSignedMessage pubkey signedMessage = signedMessage
                                        >>= verifyOracleValueSigned pubkey
                                        >>= (\(message, _) -> Just message)

{-# INLINABLE isValueSigned #-}
isValueSigned:: PubKey -> Maybe (SignedMessage OracleSignedMessage) -> Bool
isValueSigned pubKey signedMessage = isJust $ signedMessage >>= verifyOracleValueSigned pubKey


{-# INLINABLE verifyOracleValueSigned #-}
verifyOracleValueSigned :: PubKey -> SignedMessage OracleSignedMessage -> Maybe (OracleSignedMessage, TxConstraints Void Void)
verifyOracleValueSigned pubKey sm = case verifySignedMessageConstraints pubKey sm of
    Left _                   -> Nothing
    Right (osm, constraints) -> Just (osm, constraints)

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