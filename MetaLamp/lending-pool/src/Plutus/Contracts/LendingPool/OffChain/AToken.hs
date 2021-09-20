{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module Plutus.Contracts.LendingPool.OffChain.AToken where

import           Control.Monad                               (void)
import           Data.ByteString                             (ByteString)
import qualified Data.Map                                    as Map
import           Data.Text                                   (Text)
import           Data.Void                                   (Void)
import           Ext.Plutus.Ledger.Contexts                  (scriptInputsAt)
import           Ledger                                      hiding (singleton)
import           Ledger.Constraints                          as Constraints
import           Ledger.Constraints.OnChain                  as Constraints
import           Ledger.Constraints.TxConstraints            as Constraints
import           Ledger.Contexts                             (scriptCurrencySymbol)
import           Ledger.Typed.Scripts                        (MintingPolicy)
import qualified Ledger.Typed.Scripts                        as Scripts
import           Plutus.Abstract.OutputValue                 (OutputValue (..))
import qualified Plutus.Abstract.TxUtils                     as TxUtils
import           Plutus.Contract
import           Plutus.Contracts.LendingPool.OnChain.AToken (makeLiquidityPolicy)
import           Plutus.Contracts.LendingPool.OnChain.Core   (Aave, AaveScript,
                                                              Reserve (..))
import qualified Plutus.Contracts.LendingPool.OnChain.Core   as Core
import qualified Plutus.Contracts.Service.FungibleToken      as FungibleToken
import           Plutus.V1.Ledger.Contexts                   (ScriptContext)

import qualified Plutus.V1.Ledger.Scripts                    as Scripts
import           Plutus.V1.Ledger.Value                      (AssetClass (..),
                                                              TokenName (..),
                                                              assetClass,
                                                              assetClassValue,
                                                              assetClassValueOf)
import qualified PlutusTx
import           PlutusTx.Prelude                            hiding
                                                             (Semigroup (..))
import qualified PlutusTx.Semigroup                          as Semigroup
import           Prelude                                     (Semigroup (..))
import qualified Prelude

forgeATokensFrom :: forall w s. Aave -> Reserve -> PubKeyHash -> Integer -> Contract w s Text (TxUtils.TxPair AaveScript)
forgeATokensFrom aave reserve pkh amount = do
    let policy = makeLiquidityPolicy (Core.aaveHash aave) (rCurrency reserve)
        aTokenAmount = amount -- / rLiquidityIndex reserve -- TODO: how should we divide?
        forgeValue = assetClassValue (rAToken reserve) aTokenAmount
    let payment = assetClassValue (rCurrency reserve) amount
    pure $
        TxUtils.mustForgeValue @AaveScript policy forgeValue
        <> (Prelude.mempty, mustPayToPubKey pkh forgeValue)
        <> TxUtils.mustPayToScript (Core.aaveInstance aave) pkh Core.ReserveFundsDatum payment

burnATokensFrom :: Aave -> Reserve -> PubKeyHash -> Integer -> Contract w s Text (TxUtils.TxPair AaveScript)
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
