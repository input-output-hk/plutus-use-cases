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
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE NamedFieldPuns        #-}


module Contracts.Test
    ( TestData(..)
    , runTest
    , address
    ) where

import           Control.Monad             hiding (fmap)
import           Data.Aeson                (FromJSON, ToJSON)
import qualified Data.Map                  as Map
import           Data.Monoid               (Last (..))
import           Data.Text                 (Text, pack)
import           GHC.Generics              (Generic)
import           Plutus.Contract           as Contract
import qualified PlutusTx
import           PlutusTx.Prelude          hiding (Semigroup(..), unless)
import           Ledger                    hiding (singleton, MintingPolicyHash)
import qualified Ledger.Scripts            as LedgerScripts
import           Ledger.Constraints        as Constraints
import           Ledger.Oracle             (Observation, SignedMessage, signMessage)
import qualified Ledger.Typed.Scripts      as Scripts
import           Ledger.Value              as Value
import           Ledger.Ada                as Ada
import qualified Ledger.Contexts           as Validation
import           Plutus.Contracts.Currency as Currency
import           Plutus.Contract.Types     (Promise (..))
import           Plutus.ChainIndex.Tx      (fromOnChainTx)
import           Prelude                   (Semigroup (..), Show (..), String)
import qualified Prelude                   as Haskell
import           Schema                    (ToSchema)

data TestData = TestData
    { testFee :: Integer
    } deriving (Show, Generic, FromJSON, ToJSON, ToSchema, Haskell.Eq, Haskell.Ord)

PlutusTx.makeLift ''TestData

{-# INLINABLE mkValidator #-}
mkValidator :: TestData -> Integer -> () -> ScriptContext -> Bool
mkValidator testdata datum r ctx = True
    
data Test
instance Scripts.ValidatorTypes Test where
    type instance DatumType Test = Integer
    type instance RedeemerType Test = ()

typedValidator :: TestData -> Scripts.TypedValidator Test
typedValidator testData = Scripts.mkTypedValidator @Test
    ($$(PlutusTx.compile [|| mkValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode testData)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @Integer @()

validator :: TestData -> Validator
validator = Scripts.validatorScript . typedValidator

validatorHash :: TestData -> Ledger.ValidatorHash
validatorHash testData = LedgerScripts.validatorHash . validator $ testData

address :: TestData -> Ledger.Address
address = scriptAddress . validator

tokenSymbol :: TestData -> Ledger.Address -> CurrencySymbol
tokenSymbol testData address = Value.mpsSymbol . mintingPolicyHash $ tokenPolicy testData address

{-# INLINABLE checkPolicy #-}
checkPolicy :: TestData -> Ledger.Address -> () -> ScriptContext -> Bool
checkPolicy testdata address _ ctx@ScriptContext{scriptContextTxInfo=TxInfo{txInfoInputs}, scriptContextPurpose=Minting _} = 
    traceIfFalse "Should forge one token" (forgedSymbolsCount == 1)
    && traceIfFalse "Is fee paid to the script" (isTokenAndFeeSendToOracleAddress address)
    where
        ownSymbol = ownCurrencySymbol ctx
        info = scriptContextTxInfo ctx
        forged = txInfoMint info
        forgedSymbolsCount = length $ symbols forged
        feeValue = Ada.toValue . Ada.lovelaceOf $ testFee testdata
        oracleValue = forged --feeValue -- <> forged
        isTokenAndFeeSendToOracleAddress :: Address -> Bool
        isTokenAndFeeSendToOracleAddress addr = isJust . find (\o ->
            txOutValue o == oracleValue &&
            toPubKeyHash addr == Validation.pubKeyOutput o) $ txInfoOutputs info

tokenPolicy :: TestData -> Ledger.Address -> LedgerScripts.MintingPolicy
tokenPolicy testData address = LedgerScripts.mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \td a -> Scripts.wrapMintingPolicy (checkPolicy td a) ||])
        `PlutusTx.applyCode`
            PlutusTx.liftCode testData
        `PlutusTx.applyCode`
            PlutusTx.liftCode address

runTest :: forall w s. TestData -> Contract w s Text ()
runTest testData = do 
    -- let tokenMintPolicyHash = mintPolicyHash testData
    --     redeemer = Redeemer $ PlutusTx.toBuiltinData  ()
    --     tx = Constraints.mustMintCurrencyWithRedeemer tokenMintPolicyHash redeemer 
        
    -- ledgerTx <- submitTxConstraints (typedValidator testData) tx
    -- awaitTxConfirmed $ txId ledgerTx

    let inst = typedValidator testData
        mrScript = validator testData
        addr = address testData
        tokenMintingPolicy = tokenPolicy testData addr
        forgedToken = Value.singleton (tokenSymbol testData addr) "tokenName" 1
        feeVal = Ada.toValue . Ada.lovelaceOf $ testFee testData 

    let lookups = Constraints.typedValidatorLookups inst 
                <> Constraints.otherScript mrScript
                <> Constraints.mintingPolicy tokenMintingPolicy

        tx      = Constraints.mustPayToTheScript (testFee testData) feeVal --(feeVal <> forgedToken)
                <> Constraints.mustMintValue forgedToken

    ledgerTx <- submitTxConstraintsWith lookups tx
    void $ awaitTxConfirmed $ txId ledgerTx

