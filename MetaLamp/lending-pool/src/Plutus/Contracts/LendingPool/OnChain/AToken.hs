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

module Plutus.Contracts.LendingPool.OnChain.AToken where

import           Control.Monad                             (void)
import           Data.ByteString                           (ByteString)
import qualified Data.Map                                  as Map
import           Data.Text                                 (Text)
import           Data.Void                                 (Void)
import           Ext.Plutus.Ledger.Contexts                (scriptInputsAt)
import           Ledger                                    hiding (singleton)
import           Ledger.Constraints                        as Constraints
import           Ledger.Constraints.OnChain                as Constraints
import           Ledger.Constraints.TxConstraints          as Constraints
import           Ledger.Contexts                           (scriptCurrencySymbol)
import           Ledger.Typed.Scripts                      (MintingPolicy)
import qualified Ledger.Typed.Scripts                      as Scripts
import           Plutus.Abstract.OutputValue               (OutputValue (..))
import qualified Plutus.Abstract.TxUtils                   as TxUtils
import           Plutus.Contract
import           Plutus.Contracts.LendingPool.OnChain.Core (Aave, AaveScript,
                                                            Reserve (..))
import qualified Plutus.Contracts.LendingPool.OnChain.Core as Core
import qualified Plutus.Contracts.Service.FungibleToken    as FungibleToken
import           Plutus.V1.Ledger.Contexts                 (ScriptContext)
import qualified Plutus.V1.Ledger.Scripts                  as Scripts
import           Plutus.V1.Ledger.Value                    (AssetClass (..),
                                                            TokenName (..),
                                                            assetClass,
                                                            assetClassValue,
                                                            assetClassValueOf)
import qualified PlutusTx
import           PlutusTx.Prelude                          hiding
                                                           (Semigroup (..))
import qualified PlutusTx.Semigroup                        as Semigroup
import           Prelude                                   (Semigroup (..))
import qualified Prelude

{-# INLINABLE validator #-}
validator :: ValidatorHash -> AssetClass -> TokenName -> BuiltinData -> ScriptContext -> Bool
validator aaveScript underlyingAsset aTokenName _ ctx =
    traceIfFalse "Aave tokens mint forbidden" $ amountMinted /= 0 && amountScriptAsset == amountMinted
    where
        txInfo :: TxInfo
        txInfo = scriptContextTxInfo ctx
        aTokenCurrency :: AssetClass
        aTokenCurrency = assetClass (ownCurrencySymbol ctx) aTokenName
        amountAsset :: Value -> Integer
        amountAsset = flip assetClassValueOf underlyingAsset

        amountMinted :: Integer
        amountMinted = assetClassValueOf (txInfoMint txInfo) aTokenCurrency

        amountScriptAsset :: Integer
        amountScriptAsset =
          let outputValue = foldMap snd $ scriptOutputsAt aaveScript txInfo
              inputValue = foldMap snd $ scriptInputsAt aaveScript txInfo
           in amountAsset outputValue - amountAsset inputValue

makeLiquidityPolicy :: ValidatorHash -> AssetClass -> MintingPolicy
makeLiquidityPolicy aaveScript asset = Scripts.mkMintingPolicyScript $
  $$(PlutusTx.compile [|| \s a t -> Scripts.wrapMintingPolicy $ validator s a t||])
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
