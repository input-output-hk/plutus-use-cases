{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Plutus.Contracts.StableCoin
    (
      StableCoinSchema
    , endpoints 
    ) where

import           Control.Monad              hiding (fmap)
import           Control.Monad.Freer.Extras as Extras
import           Data.Aeson                 (ToJSON, FromJSON)
import           Data.Text                  (Text)
import           Data.Void                  (Void)
import           GHC.Generics               (Generic)
import           Plutus.Contract            as Contract hiding (when)
import           Plutus.Trace.Emulator      as Emulator
import qualified PlutusTx
import           PlutusTx.Prelude           hiding (Semigroup(..), unless)
import           Ledger                     hiding (singleton)
import           Ledger.Constraints         as Constraints
import qualified Ledger.Typed.Scripts       as Scripts
import           Ledger.Value               as Value
import           Playground.Contract        (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema)
import           Playground.TH              (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types           (KnownCurrency (..))
import           Text.Printf                (printf)
import           Wallet.Emulator.Wallet

import           Prelude                   (Semigroup (..))
import qualified Prelude                   as Prelude


data StableCoinParam = StableCoin | StableCoinParams
    { coinSymbol :: !CurrencySymbol
    , coinAsset :: !AssetClass
    } deriving (Show, Generic, FromJSON, ToJSON, Prelude.Eq, Prelude.Ord)

--Validator for withdrawing locked fund inside this contract
{-# INLINABLE mkTokenValidator #-}
mkTokenValidator :: StableCoinParam -> Integer -> Integer -> ScriptContext -> Bool
mkTokenValidator scParam noOfTokensAvailable noOfTokensToWithdraw ctx =  
        traceIfFalse "Required tokens missing from input"  inputHasTokens 
    where
        info = scriptContextTxInfo ctx

        inputHasTokens :: Bool
        inputHasTokens = True

        --ownInput :: TxOut
        --ownInput = case findOwnInput ctx of
         --   Nothing -> traceError "Input is missing"
       --     Just i  -> txInInfoResolved i

        --inputHasTokens :: Bool
        --inputHasTokens = assetClassValueOf (txOutValue ownInput) (coinAsset scParam) == noOfTokens

data StableCoinType
instance Scripts.ScriptType StableCoinType where
    type instance DatumType StableCoinType = Integer
    type instance RedeemerType StableCoinType = Integer

-- tokenInst :: StableCoinParam -> Scripts.ScriptInstance StableCoinType
-- tokenInst scParam = Scripts.validator @StableCoinType
--     ($$(PlutusTx.compile [|| mkTokenValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode scParam)
--     $$(PlutusTx.compile [|| wrap ||])
--   where
--     wrap = Scripts.wrapValidator @Integer @Integer

-- tokenValidator :: StableCoinParam -> Validator
-- tokenValidator = Scripts.validatorScript . tokenInst

-- tokenValidatorAddress :: StableCoinParam -> Ledger.Address
-- tokenValidatorAddress = scriptAddress . tokenValidator

-- TODO policy that checks if sufficent amount of ada is transsfered to the script address
--TODO uses of script address here
{-# INLINABLE mkPolicy #-}
mkPolicy :: ScriptContext -> Bool
mkPolicy ctx = traceIfFalse "Wrong script address used"  isValidScriptAddress
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    -- TODO Check address from context
    isValidScriptAddress :: Bool
    isValidScriptAddress = True


policy :: Scripts.MonetaryPolicy
policy = mkMonetaryPolicyScript $$(PlutusTx.compile [|| Scripts.wrapMonetaryPolicy mkPolicy ||])

curSymbol :: CurrencySymbol
curSymbol = scriptCurrencySymbol policy

data TokenParams = MintParams
    {noOfTokens    :: !Integer
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

type StableCoinSchema =
    BlockchainActions
        .\/ Endpoint "lockFundAndMintTokens" TokenParams
        .\/ Endpoint "withdrawFundAndBurnTokens" TokenParams

tnName="CollateralCoin"

--TODO Uses of oracle interface here
--TOOD use no of tokens to lock fund to contract accordingly
lockFundAndMintTokens :: TokenParams -> Contract w StableCoinSchema Text ()
lockFundAndMintTokens tokenParams = do
    let val     = Value.singleton curSymbol tnName (noOfTokens tokenParams)
        lookups = Constraints.monetaryPolicy $ policy
        tx      = Constraints.mustForgeValue val
    ledgerTx <- submitTxConstraintsWith @Void lookups tx
    void $ awaitTxConfirmed $ txId ledgerTx
    Contract.logInfo @String $ printf "forged %s" (show val)


withdrawFundAndBurnTokens :: TokenParams -> Contract w StableCoinSchema Text ()
withdrawFundAndBurnTokens tokenParams = do
    let val     = Value.singleton curSymbol tnName (noOfTokens tokenParams)
        lookups = Constraints.monetaryPolicy $ policy
        -- <>Constraints.scriptInstanceLookups tokenInst 
        tx      = Constraints.mustForgeValue (negate val)
        -- <>Constraints.mustPayToTheScript
    ledgerTx <- submitTxConstraintsWith @Void lookups tx
    void $ awaitTxConfirmed $ txId ledgerTx
    Contract.logInfo @String $ printf "Burned tokens %s" (show val)


endpoints :: Contract () StableCoinSchema Text ()
endpoints = (lockFundAndMintTokens' `select` withdrawFundAndBurnTokens') >> endpoints
  where
    lockFundAndMintTokens' = endpoint @"lockFundAndMintTokens" >>= lockFundAndMintTokens
    withdrawFundAndBurnTokens' = endpoint @"withdrawFundAndBurnTokens" >>= withdrawFundAndBurnTokens

mkSchemaDefinitions ''StableCoinSchema

mkKnownCurrencies []
