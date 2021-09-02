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


module Contracts.Test1
    ( ContractParam(..)
    , initTest
    , runTest
    , address
    , testTokenName
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
import           Ledger.Oracle
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

data ContractParam = ContractParam
    { cpSigner :: PubKey
    } deriving (Show, Generic, FromJSON, ToJSON, ToSchema, Haskell.Eq, Haskell.Ord)

PlutusTx.makeLift ''ContractParam

data ContractDatum = ContractDatum
    { cdMessageId       :: Integer
    , cdSignedMessageId :: Maybe (SignedMessage Integer)
    }
    deriving (Show, Generic, FromJSON, ToJSON, Haskell.Eq)

PlutusTx.unstableMakeIsData ''ContractDatum
PlutusTx.makeLift ''ContractDatum

type SwapOracleMessage = SignedMessage (Observation Rational)



{-# INLINABLE mkValidator #-}
mkValidator :: ContractParam -> ContractDatum -> () -> ScriptContext -> Bool
mkValidator contractParam datum r ctx = 
    traceIfFalse "value signed" isValueSigned
  where 
    verifyValueSigned :: Maybe (SignedMessage Integer) -> PubKey -> Maybe Integer
    verifyValueSigned smMaybe pk = case smMaybe of
        Just sm -> case verifySignedMessageOnChain ctx pk sm of
            Left err -> case err of
                SignatureMismatch sig pk hash -> traceError "SignatureMismatch"
                DatumMissing hash ->  traceError "DatumMissing"
                DecodingError -> traceError "DecodingError"
                DatumNotEqualToExpected -> traceError "DatumNotEqualToExpected"
            Right res -> Just res
        Nothing -> traceError "No signed message"

    isValueSigned = isJust $ verifyValueSigned (cdSignedMessageId datum) (cpSigner contractParam)


    
data Test
instance Scripts.ValidatorTypes Test where
    type instance DatumType Test = ContractDatum
    type instance RedeemerType Test = ()

typedValidator :: ContractParam -> Scripts.TypedValidator Test
typedValidator contractParam = Scripts.mkTypedValidator @Test
    ($$(PlutusTx.compile [|| mkValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode contractParam)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @ContractDatum @()

validator :: ContractParam -> Validator
validator = Scripts.validatorScript . typedValidator

validatorHash :: ContractParam -> Ledger.ValidatorHash
validatorHash testData = LedgerScripts.validatorHash . validator $ testData

address :: ContractParam -> Ledger.Address
address = scriptAddress . validator

testTokenName :: TokenName
testTokenName = "testTokenName"

initTest :: forall w s. ContractParam -> PrivateKey -> AssetClass -> Contract w s Text ()
initTest contractParam pk uniqueTokenAsset = do 

    let inst = typedValidator contractParam
        mrScript = validator contractParam
        addr = address contractParam
        messageId = 123
        uniqueTokenVal = assetClassValue uniqueTokenAsset 1
    let contractDatum = ContractDatum { cdMessageId = messageId, cdSignedMessageId = Just $ signMessage messageId pk }
    let lookups = Constraints.typedValidatorLookups inst 
                <> Constraints.otherScript mrScript

        tx      = Constraints.mustPayToTheScript contractDatum uniqueTokenVal

    ledgerTx <- submitTxConstraintsWith lookups tx
    void $ awaitTxConfirmed $ txId ledgerTx

runTest :: forall w s. ContractParam -> AssetClass -> Contract w s Text ()
runTest contractParam uniqueTokenAsset = do 
    logInfo @String "runTest"
    let inst = typedValidator contractParam
        mrScript = validator contractParam
        addr = address contractParam
        uniqueTokenVal = assetClassValue uniqueTokenAsset 1
        redeemer = Redeemer $ PlutusTx.toBuiltinData $ ()

    pkh <- pubKeyHash <$> Contract.ownPubKey
    utxos <- utxoAt addr
    let xs = [ (oref, o)
            | (oref, o) <- Map.toList utxos
            , assetClassValueOf (txOutValue $ txOutTxOut o) uniqueTokenAsset == 1
            ]
    case xs of 
        [(oref, o)] -> do
            let lookups = Constraints.typedValidatorLookups inst 
                        <> Constraints.otherScript mrScript
                        <> Constraints.unspentOutputs (Map.singleton oref o)

                tx      = Constraints.mustSpendScriptOutput oref redeemer
                        <> Constraints.mustPayToPubKey pkh uniqueTokenVal
            ledgerTx <- submitTxConstraintsWith lookups tx
            void $ awaitTxConfirmed $ txId ledgerTx
        _ -> throwError "token not found"
   
