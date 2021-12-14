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

module Contracts.Oracle.RequestToken
    ( OracleRequestToken (..)
    , requestTokenSymbol
    , requestTokenClass
    , requestTokenClassFromOracle
    , requestTokenPolicy
    , requestTokenValue
    , mintingScript
    , mintingScriptShortBs
    , oracleRequestMintPolicyHash
    ) where

import           Cardano.Api.Shelley       (PlutusScript (..), PlutusScriptV1)
import           Codec.Serialise
import qualified Data.ByteString.Short     as SBS
import qualified Data.ByteString.Lazy      as LBS
import qualified PlutusTx
import           PlutusTx.Prelude          hiding (Semigroup(..), unless)
import           Ledger                    hiding (singleton, MintingPolicyHash)
import qualified Ledger.Scripts            as LedgerScripts
import qualified Ledger.Typed.Scripts      as Scripts
import           Ledger.Value              as Value
import           Ledger.Ada                as Ada
import           Prelude                   (Semigroup (..))
import Contracts.Oracle.Types

{-# INLINABLE checkRequesTokenPolicy #-}
checkRequesTokenPolicy :: OracleRequestToken -> OracleRequestRedeemer -> ScriptContext -> Bool
checkRequesTokenPolicy requestToken r ctx@ScriptContext{scriptContextTxInfo=TxInfo{txInfoInputs}, scriptContextPurpose=Minting _} = 
    case r of
        Request     -> traceIfFalse "Should forge one token" (forgedCount == 1)
                    && traceIfFalse "Is fee paid" (valuePaidTo info (ortOperator requestToken) == feeValue)
                    -- todo: https://github.com/input-output-hk/plutus-apps/issues/161
                    && traceIfFalse "Is forged with collateral" (isForgedWithCollateral)
        RedeemToken -> traceIfFalse "Should redeem one token" (forgedCount == -1)
    where
        ownSymbol = ownCurrencySymbol ctx
        info = scriptContextTxInfo ctx
        forged = txInfoMint info
        forgedCount = valueOf forged ownSymbol oracleRequestTokenName
        feeValue = Ada.toValue $ ortFee requestToken
        collateralValue = Ada.toValue $ ortCollateral requestToken
        isForgedWithCollateral :: Bool
        isForgedWithCollateral = isJust . find (isForgetTokenOutput) $ txInfoOutputs info
        isForgetTokenOutput:: TxOut -> Bool
        isForgetTokenOutput o =  
            txOutValue o == Value.singleton ownSymbol oracleRequestTokenName 1 
                            <> collateralValue


requestTokenPolicy :: OracleRequestToken -> LedgerScripts.MintingPolicy
requestTokenPolicy oracle = LedgerScripts.mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \o -> Scripts.wrapMintingPolicy (checkRequesTokenPolicy o) ||])
        `PlutusTx.applyCode`
            PlutusTx.liftCode oracle

oracleRequestMintPolicyHash :: OracleRequestToken -> LedgerScripts.MintingPolicyHash
oracleRequestMintPolicyHash = mintingPolicyHash . requestTokenPolicy

requestTokenSymbol :: OracleRequestToken -> CurrencySymbol
requestTokenSymbol = Value.mpsSymbol . oracleRequestMintPolicyHash

requestTokenClass :: OracleRequestToken -> AssetClass
requestTokenClass oracleRequest = AssetClass (requestTokenSymbol oracleRequest, oracleRequestTokenName)

requestTokenClassFromOracle :: Oracle -> AssetClass
requestTokenClassFromOracle = requestTokenClass . oracleToRequestToken

requestTokenValue:: Oracle -> Value
requestTokenValue oracle = assetClassValue (requestTokenClassFromOracle oracle) 1

mintinPlutusScript :: OracleRequestToken -> Script
mintinPlutusScript =
  unMintingPolicyScript . requestTokenPolicy

mintingValidator :: OracleRequestToken -> Validator
mintingValidator = Validator . mintinPlutusScript

mintingScriptAsCbor :: OracleRequestToken -> LBS.ByteString
mintingScriptAsCbor = serialise . mintingValidator

mintingScript :: OracleRequestToken ->PlutusScript PlutusScriptV1
mintingScript = PlutusScriptSerialised . SBS.toShort . LBS.toStrict . mintingScriptAsCbor

mintingScriptShortBs :: OracleRequestToken -> SBS.ShortByteString
mintingScriptShortBs = SBS.toShort . LBS.toStrict . mintingScriptAsCbor