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
import           Types.Game   
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
import qualified Ledger.Typed.Scripts      as Scripts
import           Ledger.Value              as Value
import           Ledger.Ada                as Ada
import           Plutus.Contracts.Currency as Currency
import           Plutus.Contract.Types     (Promise (..))
import           Prelude                   (Semigroup (..), Show (..), String)
import qualified Prelude                   as Haskell
import           Schema                    (ToSchema)
import Contracts.Oracle.Types

{-# INLINABLE checkRequesTokenPolicy #-}
checkRequesTokenPolicy :: OracleRequestToken -> OracleRequestRedeemer -> ScriptContext -> Bool
checkRequesTokenPolicy requestToken r ctx@ScriptContext{scriptContextTxInfo=TxInfo{txInfoInputs}, scriptContextPurpose=Minting _} = 
    case r of
        Request     -> traceIfFalse "Should forge one token" (forgedCount == 1)
                    && traceIfFalse "Is forged with collateral" (isForgedWithCollateral)
                    && traceIfFalse "Is fee paid" (isFeePaid (Just $ ortOperator requestToken))
        RedeemToken -> traceIfFalse "Should redeem one token" (forgedCount == -1)
    where
        ownSymbol = ownCurrencySymbol ctx
        info = scriptContextTxInfo ctx
        forged = txInfoMint info
        forgedSymbolsCount = length $ symbols forged
        forgedCount = valueOf forged ownSymbol oracleRequestTokenName
        feeValue = Ada.toValue $ ortFee requestToken
        collateralValue = Ada.toValue $ ortCollateral requestToken
        isFeePaid :: Maybe PubKeyHash -> Bool
        isFeePaid operatorAddr = isJust . find (\o ->
            txOutValue o == feeValue  &&
            operatorAddr == Validation.pubKeyOutput o) $ txInfoOutputs info
        isForgedWithCollateral :: Bool
        isForgedWithCollateral = isJust . find (\o -> 
            txOutValue o == Value.singleton ownSymbol oracleRequestTokenName 1 
                            <> (Ada.toValue $ ortCollateral requestToken)
            ) $ txInfoOutputs info

requestTokenPolicy :: OracleRequestToken -> LedgerScripts.MintingPolicy
requestTokenPolicy oracle = LedgerScripts.mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \oracle -> Scripts.wrapMintingPolicy (checkRequesTokenPolicy oracle) ||])
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