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
import           Ext.Plutus.Ledger.Contexts       (scriptInputsAt)
import           Ledger                           hiding (singleton)
import           Ledger.Constraints               as Constraints
import           Ledger.Constraints.OnChain       as Constraints
import           Ledger.Constraints.TxConstraints as Constraints
import           Ledger.Typed.Scripts             (MonetaryPolicy)
import qualified Ledger.Typed.Scripts             as Scripts
import           Plutus.Contract
import           Plutus.Contracts.Core            (Aave, AaveScript,
                                                   Reserve (..))
import qualified Plutus.Contracts.Core            as Core
import qualified Plutus.Contracts.FungibleToken   as FungibleToken
import qualified Plutus.Contracts.State           as State
import qualified Plutus.Contracts.TxUtils         as TxUtils
import           Plutus.OutputValue               (OutputValue (..))
import           Plutus.V1.Ledger.Contexts        (ScriptContext,
                                                   scriptCurrencySymbol)
import qualified Plutus.V1.Ledger.Scripts         as Scripts
import           Plutus.V1.Ledger.Value           (AssetClass (..),
                                                   TokenName (..), assetClass,
                                                   assetClassValue,
                                                   assetClassValueOf)
import qualified PlutusTx
import           PlutusTx.Prelude                 hiding (Semigroup (..))
import qualified PlutusTx.Semigroup               as Semigroup
import           Prelude                          (Semigroup (..))
import qualified Prelude

{-# INLINABLE validator #-}
validator :: ValidatorHash -> AssetClass -> TokenName -> ScriptContext -> Bool
validator aaveScript underlyingAsset aTokenName ctx =
    traceIfFalse "Aave tokens mint forbidden" $ amountMinted /= 0 && amountScriptAsset == amountMinted
    where
        txInfo :: TxInfo
        txInfo = scriptContextTxInfo ctx
        aTokenCurrency :: AssetClass
        aTokenCurrency = assetClass (ownCurrencySymbol ctx) aTokenName
        amountAsset :: Value -> Integer
        amountAsset = flip assetClassValueOf underlyingAsset

        amountMinted :: Integer
        amountMinted = assetClassValueOf (txInfoForge txInfo) aTokenCurrency

        amountScriptAsset :: Integer
        amountScriptAsset =
          let outputValue = foldMap snd $ scriptOutputsAt aaveScript txInfo
              inputValue = foldMap snd $ scriptInputsAt aaveScript txInfo
           in amountAsset outputValue - amountAsset inputValue

makeLiquidityPolicy :: ValidatorHash -> AssetClass -> MonetaryPolicy
makeLiquidityPolicy aaveScript asset = Scripts.mkMonetaryPolicyScript $
  $$(PlutusTx.compile [|| \s a t -> Scripts.wrapMonetaryPolicy $ validator s a t||])
    `PlutusTx.applyCode`
        PlutusTx.liftCode aaveScript
    `PlutusTx.applyCode`
        PlutusTx.liftCode asset
    `PlutusTx.applyCode`
        PlutusTx.liftCode aToken
        where
            aToken = aTokenName asset

makeAToken :: ValidatorHash -> AssetClass -> AssetClass
makeAToken aaveScript asset = assetClass (scriptCurrencySymbol $ makeLiquidityPolicy aaveScript asset) (aTokenName asset)

{-# INLINABLE aTokenName #-}
aTokenName :: AssetClass -> TokenName
aTokenName asset = TokenName $ "a" Semigroup.<> case asset of
    AssetClass (_,TokenName n) -> n

forgeATokensFrom :: forall w s. (HasBlockchainActions s) => Aave -> Reserve -> PubKeyHash -> Integer -> Contract w s Text (TxUtils.TxPair AaveScript)
forgeATokensFrom aave reserve pkh amount = do
    let policy = makeLiquidityPolicy (Core.aaveHash aave) (rCurrency reserve)
        aTokenAmount = amount -- / rLiquidityIndex reserve -- TODO: how should we divide?
        forgeValue = assetClassValue (rAToken reserve) aTokenAmount
    let payment = assetClassValue (rCurrency reserve) amount
    pure $
        TxUtils.mustForgeValue @AaveScript policy forgeValue
        <> (Prelude.mempty, mustPayToPubKey pkh forgeValue)
        <> TxUtils.mustPayToScript (Core.aaveInstance aave) pkh Core.ReserveFundsDatum payment

burnATokensFrom :: (HasBlockchainActions s) => Aave -> Reserve -> PubKeyHash -> Integer -> Contract w s Text (TxUtils.TxPair AaveScript)
burnATokensFrom aave reserve pkh amount = do
    let asset = rCurrency reserve
    let userConfigId = (asset, pkh)
    utxos <-
        Map.filter ((> 0) . flip assetClassValueOf asset . txOutValue . txOutTxOut)
        <$> utxoAt (Core.aaveAddress aave)
    let balance = mconcat . fmap (txOutValue . txOutTxOut) . map snd . Map.toList $ utxos
        aTokenAmount = amount
        remainder = assetClassValueOf balance asset - aTokenAmount
        policy = makeLiquidityPolicy (Core.aaveHash aave) asset
        burnValue = negate $ assetClassValue (rAToken reserve) aTokenAmount
        spendInputs = (\(ref, tx) -> OutputValue ref tx (Core.WithdrawRedeemer userConfigId)) <$> Map.toList utxos
    pure $
        TxUtils.mustForgeValue policy burnValue
        <> TxUtils.mustSpendFromScript (Core.aaveInstance aave) spendInputs pkh (assetClassValue asset aTokenAmount)
        <> TxUtils.mustPayToScript (Core.aaveInstance aave) pkh Core.ReserveFundsDatum (assetClassValue asset remainder)
