{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Plutus.Contracts.LendingPool.OffChain.State where

import           Control.Lens
import           Control.Monad                             hiding (fmap)
import qualified Data.ByteString                           as BS
import qualified Data.Map                                  as Map
import           Data.Monoid                               (Last (..))
import           Data.Proxy                                (Proxy (..))
import           Data.Text                                 (Text, pack)
import qualified Data.Text                                 as Text
import           Data.Void                                 (Void)
import           Ledger                                    hiding (singleton)
import           Ledger.Constraints                        as Constraints
import           Ledger.Constraints.OnChain                as Constraints
import           Ledger.Constraints.TxConstraints          as Constraints
import qualified Ledger.Scripts                            as Scripts
import qualified Ledger.Typed.Scripts                      as Scripts
import           Playground.Contract
import           Plutus.Abstract.OutputValue               (OutputValue (..),
                                                            _ovValue)
import qualified Plutus.Abstract.State                     as State
import           Plutus.Abstract.State.Update              (PutStateHandle (..),
                                                            StateHandle (..))
import qualified Plutus.Abstract.TxUtils                   as TxUtils
import           Plutus.Contract                           hiding (when)
import           Plutus.Contracts.Currency                 as Currency
import           Plutus.Contracts.LendingPool.OnChain.Core (Aave (..),
                                                            AaveDatum (..),
                                                            AaveRedeemer (..),
                                                            AaveScript,
                                                            Reserve (..),
                                                            UserConfig (..),
                                                            UserConfigId)
import qualified Plutus.Contracts.LendingPool.OnChain.Core as Core
import qualified Plutus.Contracts.Service.FungibleToken    as FungibleToken
import           Plutus.V1.Ledger.Ada                      (adaValueOf,
                                                            lovelaceValueOf)
import           Plutus.V1.Ledger.Value                    as Value
import qualified PlutusTx
import qualified PlutusTx.AssocMap                         as AssocMap
import           PlutusTx.Prelude                          hiding (Functor (..),
                                                            Semigroup (..),
                                                            unless)
import           Prelude                                   (Semigroup (..),
                                                            fmap)
import qualified Prelude

findOutputsBy :: Aave -> AssetClass -> (AaveDatum -> Maybe a) -> Contract w s Text [OutputValue a]
findOutputsBy aave = State.findOutputsBy (Core.aaveAddress aave)

findOutputBy :: Aave -> AssetClass -> (AaveDatum -> Maybe a) -> Contract w s Text (OutputValue a)
findOutputBy aave = State.findOutputBy (Core.aaveAddress aave)

findAaveOwnerToken :: Aave -> Contract w s Text (OutputValue PubKeyHash)
findAaveOwnerToken aave@Aave{..} = findOutputBy aave aaveProtocolInst (^? Core._LendingPoolDatum)

reserveStateToken, userStateToken :: Aave -> AssetClass
reserveStateToken aave = State.makeStateToken (Core.aaveHash aave) (aaveProtocolInst aave) "aaveReserve"
userStateToken aave = State.makeStateToken (Core.aaveHash aave) (aaveProtocolInst aave) "aaveUser"

findAaveReserves :: Aave -> Contract w s Text (OutputValue (AssocMap.Map AssetClass Reserve))
findAaveReserves aave = findOutputBy aave (reserveStateToken aave) (^? Core._ReservesDatum . _2)

findAaveReserve :: Aave -> AssetClass -> Contract w s Text Reserve
findAaveReserve aave reserveId = do
    reserves <- ovValue <$> findAaveReserves aave
    maybe (throwError "Reserve not found") pure $ AssocMap.lookup reserveId reserves

findAaveUserConfigs :: Aave -> Contract w s Text (OutputValue (AssocMap.Map UserConfigId UserConfig))
findAaveUserConfigs aave = findOutputBy aave (userStateToken aave) (^? Core._UserConfigsDatum . _2)

findAaveUserConfig :: Aave -> UserConfigId -> Contract w s Text UserConfig
findAaveUserConfig aave userConfigId = do
    configs <- ovValue <$> findAaveUserConfigs aave
    maybe (throwError "UserConfig not found") pure $ AssocMap.lookup userConfigId configs

putState :: Aave -> StateHandle AaveScript a -> a -> Contract w s Text (TxUtils.TxPair AaveScript)
putState aave stateHandle newState = do
    ownerTokenOutput <- fmap Core.LendingPoolDatum <$> findAaveOwnerToken aave
    State.putState
        PutStateHandle { script = Core.aaveInstance aave, ownerToken = aaveProtocolInst aave, ownerTokenOutput = ownerTokenOutput }
        stateHandle
        newState

updateState :: Aave ->  StateHandle AaveScript a -> OutputValue a -> Contract w s Text (TxUtils.TxPair AaveScript, a)
updateState aave = State.updateState (Core.aaveInstance aave)

makeReserveHandle :: Aave -> (AssocMap.Map AssetClass Reserve -> AaveRedeemer) -> StateHandle AaveScript (AssocMap.Map AssetClass Reserve)
makeReserveHandle aave toRedeemer =
    let stateToken = reserveStateToken aave in
    StateHandle {
        stateToken = stateToken,
        toDatum = Core.ReservesDatum stateToken,
        toRedeemer = toRedeemer
    }

putReserves :: Aave -> AaveRedeemer -> AssocMap.Map AssetClass Reserve -> Contract w s Text (TxUtils.TxPair AaveScript)
putReserves aave redeemer = putState aave $ makeReserveHandle aave (const redeemer)

updateReserves :: Aave -> AaveRedeemer -> OutputValue (AssocMap.Map AssetClass Reserve) -> Contract w s Text (TxUtils.TxPair AaveScript, AssocMap.Map AssetClass Reserve)
updateReserves aave redeemer = updateState aave $ makeReserveHandle aave (const redeemer)

updateReserve :: Aave -> AaveRedeemer -> AssetClass -> Reserve -> Contract w s Text (TxUtils.TxPair AaveScript, AssocMap.Map AssetClass Reserve)
updateReserve aave redeemer reserveId reserve = do
    reservesOutput <- findAaveReserves aave
    _ <- maybe (throwError "Update failed: reserve not found") pure $
        AssocMap.lookup reserveId (ovValue reservesOutput)
    updateReserves aave redeemer $ Prelude.fmap (AssocMap.insert reserveId reserve) reservesOutput

roundtripReserves :: Aave -> AaveRedeemer -> Contract w s Text (TxUtils.TxPair AaveScript)
roundtripReserves aave redeemer = do
    reservesOutput <- findAaveReserves aave
    fst <$> updateReserves aave redeemer reservesOutput

makeUserHandle :: Aave -> (AssocMap.Map UserConfigId UserConfig -> AaveRedeemer) -> StateHandle AaveScript (AssocMap.Map UserConfigId UserConfig)
makeUserHandle aave toRedeemer =
    let stateToken = userStateToken aave in
    StateHandle {
        stateToken = stateToken,
        toDatum = Core.UserConfigsDatum stateToken,
        toRedeemer = toRedeemer
    }

putUserConfigs :: Aave -> AaveRedeemer -> AssocMap.Map UserConfigId UserConfig -> Contract w s Text (TxUtils.TxPair AaveScript)
putUserConfigs aave redeemer = putState aave $ makeUserHandle aave (const redeemer)

updateUserConfigs :: Aave -> AaveRedeemer -> OutputValue (AssocMap.Map UserConfigId UserConfig) -> Contract w s Text (TxUtils.TxPair AaveScript, AssocMap.Map UserConfigId UserConfig)
updateUserConfigs aave redeemer = updateState aave $ makeUserHandle aave (const redeemer)

addUserConfig :: Aave -> AaveRedeemer -> UserConfigId -> UserConfig -> Contract w s Text (TxUtils.TxPair AaveScript, AssocMap.Map UserConfigId UserConfig)
addUserConfig aave redeemer userConfigId userConfig = do
    configsOutput <- findAaveUserConfigs aave
    _ <- maybe (pure ()) (const $ throwError "Add user config failed: config exists") $
        AssocMap.lookup userConfigId (ovValue configsOutput)
    updateUserConfigs aave redeemer $ Prelude.fmap (AssocMap.insert userConfigId userConfig) configsOutput

updateUserConfig :: Aave -> AaveRedeemer -> UserConfigId -> UserConfig -> Contract w s Text (TxUtils.TxPair AaveScript, AssocMap.Map UserConfigId UserConfig)
updateUserConfig aave redeemer userConfigId userConfig = do
    configsOutput <- findAaveUserConfigs aave
    _ <- maybe (throwError "Update failed: user config not found") pure $
        AssocMap.lookup userConfigId (ovValue configsOutput)
    updateUserConfigs aave redeemer $ Prelude.fmap (AssocMap.insert userConfigId userConfig) configsOutput
