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
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module Plutus.Contracts.AToken where

import           Data.ByteString                  (ByteString)
import           Data.Text                        (Text)
import           Ledger                           hiding (singleton)
import           Ledger.Constraints               as Constraints
import           Ledger.Constraints.OnChain       as Constraints
import           Ledger.Constraints.TxConstraints as Constraints
import           Ledger.Typed.Scripts             (MonetaryPolicy)
import qualified Ledger.Typed.Scripts             as Scripts
import           Plutus.Contract
import           Plutus.Contracts.Core            (Aave, LendingPool (..))
import qualified Plutus.Contracts.Core            as Core
import           Plutus.V1.Ledger.Contexts        (ScriptContext,
                                                   scriptCurrencySymbol)
import qualified Plutus.V1.Ledger.Scripts         as Scripts
import           Plutus.V1.Ledger.Value
import           Plutus.V1.Ledger.Value           (AssetClass (unAssetClass),
                                                   TokenName (..), assetClass, assetClassValue)
import qualified PlutusTx
import           PlutusTx.Prelude                 hiding (Semigroup (..))
import           Prelude                          (Semigroup (..))
import qualified Prelude

-- TODO: check that ScriptContext has enough liquidity
validator :: AssetClass -> ScriptContext -> Bool
validator _ _ = True

makeLiquidityPolicy :: AssetClass -> MonetaryPolicy
makeLiquidityPolicy asset = Scripts.mkMonetaryPolicyScript $
  $$(PlutusTx.compile [|| Scripts.wrapMonetaryPolicy . validator ||])
    `PlutusTx.applyCode`
        PlutusTx.liftCode asset

makeAToken :: AssetClass -> AssetClass
makeAToken asset = assetClass (scriptCurrencySymbol . makeLiquidityPolicy $ asset) (TokenName aTokenName)
  where
    (_, tokenName) = unAssetClass asset
    aTokenName = "a" <> unTokenName tokenName

forgeATokensFrom :: (HasBlockchainActions s) => Aave -> LendingPool -> PubKeyHash -> Integer -> Contract w s Text ()
forgeATokensFrom aave reserve pkh amount = do
    let script = Core.aaveInstance aave
        policy = makeLiquidityPolicy (lpCurrency reserve)
        lookups = Constraints.scriptInstanceLookups script
            <> Constraints.monetaryPolicy policy
            <> Constraints.ownPubKeyHash pkh
        aTokenAmount = amount -- / lpLiquidityIndex reserve -- TODO: how should we divide?
        outValue = assetClassValue (lpAToken reserve) aTokenAmount
        tx = mustForgeValue outValue <> mustPayToPubKey pkh outValue
    ledgerTx <- submitTxConstraintsWith lookups tx
    _ <- awaitTxConfirmed $ txId ledgerTx
    pure ()

burnATokensFrom :: (HasBlockchainActions s) => Aave -> LendingPool -> PubKeyHash -> Integer -> Contract w s Text ()
burnATokensFrom aave reserve pkh amount = do
    logInfo @String "BURN"
    let script = Core.aaveInstance aave
        policy = makeLiquidityPolicy (lpCurrency reserve)
        lookups = Constraints.scriptInstanceLookups script
            <> Constraints.monetaryPolicy policy
            <> Constraints.ownPubKeyHash pkh
        aTokenAmount = amount -- / lpLiquidityIndex reserve -- TODO: how should we divide?
        outValue = negate (assetClassValue (lpAToken reserve) aTokenAmount)
        tx = mustForgeValue outValue <> mustPayToPubKey pkh (assetClassValue (lpCurrency reserve) aTokenAmount)
    ledgerTx <- submitTxConstraintsWith lookups tx
    _ <- awaitTxConfirmed $ txId ledgerTx
    pure ()
