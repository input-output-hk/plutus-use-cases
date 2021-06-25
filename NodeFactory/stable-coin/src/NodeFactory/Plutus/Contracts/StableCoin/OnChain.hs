{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# options_ghc -fno-strictness         #-}
{-# options_ghc -fno-specialise         #-}

module NodeFactory.Plutus.Contracts.StableCoin.OnChain
    ( mkStableCoinValidator
    , validateStableCoinForging
    ) where

import           Ledger
import           Ledger.Ada           (Ada)
import qualified Ledger.Ada           as Ada
import           Ledger.Constraints.OnChain       as Constraints
import           Ledger.Constraints.TxConstraints as Constraints
import           Ledger.Value                     (AssetClass (..), symbols)
import           NodeFactory.Plutus.Contracts.StableCoin.Types
import qualified PlutusTx
import           PlutusTx.Prelude

{-# INLINABLE findOwnInput' #-}
findOwnInput' :: ScriptContext -> TxInInfo
findOwnInput' ctx = fromMaybe (error ()) (findOwnInput ctx)

{-# INLINABLE valueWithin #-}
valueWithin :: TxInInfo -> Value
valueWithin = txOutValue . txInInfoResolved

{-# INLINABLE validateCreate #-}
-- | Validates the creation of the stable coin vault. Conditions:
--
-- 1,2. Check that stable coin factory utxo is in input and output
-- 3.   Check that we are creating new vault
-- 4.   Check that one vault state coin has been forged
-- 5.   Check that more than minimum amount of lovelace has been sent
-- 6.   Check that enough lovelace has been sent to mint stablecoin
-- 7.   Check if appropriate amount of stablecoin has been minted
-- 8.   Check oracle input/output
validateCreate :: StableCoin
            -> Coin VaultState
            -> [StableCoinVault]
            -> StableCoinVault
            -> ScriptContext
            -> Bool
validateCreate StableCoin{..} c vs v@StableCoinVault{..} ctx =
    traceIfFalse "StableCoin coin not present" (isUnity (valueWithin $ findOwnInput' ctx) sCoin)        && -- 1
    Constraints.checkOwnOutputConstraint ctx (OutputConstraint (Factory $ v : vs) $ unitValue sCoin)    && -- 2
    all (/= v) vs                                                                                       && -- 3
    isUnity forged c                                                                                    && -- 4
    traceIfFalse "Less than minimum" (amountOfAdaInInput > minimumLovelaceAmount)                       && -- 5
    traceIfFalse "Not enough ada sent" (requiredAmountOfAda amount <= amountOfAdaInInput)               && -- 6
    (unAmount (amountOf forged stableCoin') == amount)                                                  -- 7
    -- 8 TODO - Check oracle input/output
  where 
    info :: TxInfo
    info = scriptContextTxInfo ctx

    forged :: Value
    forged = txInfoForge $ scriptContextTxInfo ctx

    adaValueIn :: Value -> Integer
    adaValueIn v = Ada.getLovelace (Ada.fromValue v)

    -- Maybe define as const outside validator
    minimumLovelaceAmount = 10

    amountOfAdaInInput = adaValueIn (valueWithin $ findOwnInput' ctx)

    requiredAmountOfAda :: Integer -> Integer
    requiredAmountOfAda a = a * 1 -- TODO use value from oracle

    stableCoin' :: Coin USDc
    stableCoin' = let AssetClass (cs, _) = unCoin c in mkCoin cs $ scStablecoinTokenName

    requiredAmountOfAda :: Integer -> Integer
    requiredAmountOfAda a = a * 1 -- TODO use value from oracle

    -- oracleInput :: TxOut
    -- oracleInput =
    --   let
    --     -- all inputs that sit at addr
    --     ins = [ o
    --           | i <- txInfoInputs info
    --           , let o = txInInfoResolved i
    --           , txOutAddress o == addr
    --           ]
    --   in
    --     case ins of
    --         [o] -> o
    --         _   -> traceError "expected exactly one oracle input"

    -- oracleValue' = case oracleValue oracleInput (`findDatum` info) of
    --     Nothing -> traceError "oracle value not found"
    --     Just x  -> x

    -- isAmountCollateralized :: Integer -> Integer -> Bool
    -- isAmountCollateralized lovelace oracleValue = oracleValue * lovelace > amount

    -- hasEnoughLovelace :: Bool
    -- hasEnoughLovelace =
    --   let
    --     lovelaceIn = case findOwnInput ctx of
    --         Nothing -> traceError "own input not found"
    --         Just i  -> lovelaces $ txOutValue $ txInInfoResolved i
    --   in
    --     isAmountCollateralized lovelaceIn oracleValue'
    

{-# INLINABLE validateCloseVault #-}
-- | Validates the closing of the stable coin vault. Conditions:
-- 
validateCloseVault :: StableCoin 
                -> ScriptContext 
                -> Bool
validateCloseVault sc ctx = hasFactoryInput
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    hasFactoryInput :: Bool
    hasFactoryInput =
        traceIfFalse "Stable coin factory input expected" $ 
        isUnity (valueSpent info) (sCoin sc)

-- TODO
-- {-# INLINABLE validateLiquidateVault #-}
-- validateLiquidateVault :: StableCoin...

mkStableCoinValidator :: StableCoin
                    -> Coin VaultState
                    -> StableCoinDatum
                    -> StableCoinAction
                    -> ScriptContext
                    -> Bool
mkStableCoinValidator sc c (Factory vs) (Create v)  ctx = validateCreate sc c vs v ctx    -- case: create vault
mkStableCoinValidator sc _ (Vault _)    Close       ctx = validateCloseVault sc ctx       -- case: close vault
mkStableCoinValidator _  _ _            _           _   = False                           -- case: default
-- TODO case: liquidate vault

{-# INLINABLE validateStableCoinForging #-}
validateStableCoinForging :: StableCoin -> TokenName -> ScriptContext -> Bool
validateStableCoinForging StableCoin{..} tn ctx
  = case [ i
         | i <- txInfoInputs $ scriptContextTxInfo ctx
         , let v = valueWithin i
         , isUnity v sCoin || isUnity v lpC
         ] of
    [_]    -> True
    [_, _] -> True
    _      -> traceError "pool state forging without StableCoin input"
  where
    lpC :: Coin sUSD
    lpC = mkCoin (ownCurrencySymbol ctx) tn