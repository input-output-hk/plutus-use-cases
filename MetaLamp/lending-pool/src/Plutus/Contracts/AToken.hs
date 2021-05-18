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

import           Control.Monad                    (void)
import           Data.ByteString                  (ByteString)
import qualified Data.Map                         as Map
import           Data.Text                        (Text)
import           Data.Void                        (Void)
import           Ledger                           hiding (singleton)
import           Ledger.Constraints               as Constraints
import           Ledger.Constraints.OnChain       as Constraints
import           Ledger.Constraints.TxConstraints as Constraints
import           Ledger.Typed.Scripts             (MonetaryPolicy)
import qualified Ledger.Typed.Scripts             as Scripts
import           Plutus.Contract
import           Plutus.Contracts.Core            (Aave, Reserve (..))
import qualified Plutus.Contracts.Core            as Core
import qualified Plutus.Contracts.FungibleToken   as FungibleToken
import qualified Plutus.Contracts.State           as State
import           Plutus.V1.Ledger.Contexts        (ScriptContext,
                                                   scriptCurrencySymbol)
import qualified Plutus.V1.Ledger.Scripts         as Scripts
import           Plutus.V1.Ledger.Value           (AssetClass (unAssetClass),
                                                   TokenName (..), assetClass,
                                                   assetClassValue,
                                                   assetClassValueOf)
import qualified PlutusTx
import           PlutusTx.Prelude                 hiding (Semigroup (..))
import           Prelude                          (Semigroup (..))
import qualified Prelude

{-# INLINABLE validator #-}
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

forgeATokensFrom :: (HasBlockchainActions s) => Aave -> Reserve -> PubKeyHash -> Integer -> Contract w s Text ()
forgeATokensFrom aave reserve pkh amount = do
    let script = Core.aaveInstance aave
        policy = makeLiquidityPolicy (rCurrency reserve)
        lookups = Constraints.scriptInstanceLookups script
            <> Constraints.monetaryPolicy policy
            <> Constraints.ownPubKeyHash pkh
        aTokenAmount = amount -- / rLiquidityIndex reserve -- TODO: how should we divide?
        outValue = assetClassValue (rAToken reserve) aTokenAmount
        tx = mustForgeValue outValue <> mustPayToPubKey pkh outValue
    ledgerTx <- submitTxConstraintsWith lookups tx
    _ <- awaitTxConfirmed $ txId ledgerTx
    pure ()

burnATokensFrom :: (HasBlockchainActions s) => Aave -> Reserve -> PubKeyHash -> Integer -> Contract w s Text ()
burnATokensFrom aave reserve pkh amount = do
    let asset = rCurrency reserve
        aTokenAmount = amount
        script = Core.aaveInstance aave
        policy = makeLiquidityPolicy asset
        burnLookups = Constraints.scriptInstanceLookups script
            <> Constraints.otherScript (Core.aaveValidator aave)
            <> Constraints.ownPubKeyHash pkh
            <> Constraints.monetaryPolicy policy
        outValue = negate $ assetClassValue (rAToken reserve) aTokenAmount
        burnTx = mustForgeValue outValue
    ledgerTx <- submitTxConstraintsWith burnLookups burnTx
    _ <- awaitTxConfirmed $ txId ledgerTx

    utxos <-
        Map.filter ((> 0) . flip assetClassValueOf asset . txOutValue . txOutTxOut)
        <$> utxoAt (Core.aaveAddress aave)
    let balance = mconcat . fmap (txOutValue . txOutTxOut) . map snd . Map.toList $ utxos
        remainder = assetClassValueOf balance asset - aTokenAmount
        withdrawLookups = Constraints.scriptInstanceLookups script
            <> Constraints.otherScript (Core.aaveValidator aave)
            <> Constraints.unspentOutputs utxos
            <> Constraints.ownPubKeyHash pkh
        orefs = fst <$> Map.toList utxos
        spendTx = mconcat $ fmap (\ref -> mustSpendScriptOutput ref $ Redeemer $ PlutusTx.toData Core.WithdrawRedeemer) orefs
        withdrawTx = mustPayToPubKey pkh (assetClassValue asset aTokenAmount)
            <> spendTx
            <> mustPayToTheScript Core.DepositDatum (assetClassValue asset remainder)
    ledgerTx <- submitTxConstraintsWith withdrawLookups withdrawTx
    _ <- awaitTxConfirmed $ txId ledgerTx

    pure ()
