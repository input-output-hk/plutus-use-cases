{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Plutus.Contracts.State where

import           Control.Monad                    hiding (fmap)
import qualified Data.ByteString                  as BS
import qualified Data.Map                         as Map
import           Data.Monoid                      (Last (..))
import           Data.Proxy                       (Proxy (..))
import           Data.Text                        (Text, pack)
import qualified Data.Text                        as Text
import           Data.Void                        (Void)
import           Ledger                           hiding (singleton)
import           Ledger.Constraints               as Constraints
import           Ledger.Constraints.OnChain       as Constraints
import           Ledger.Constraints.TxConstraints as Constraints
import qualified Ledger.Scripts                   as Scripts
import qualified Ledger.Typed.Scripts             as Scripts
import           Playground.Contract
import           Plutus.Contract                  hiding (when)
import           Plutus.Contracts.Core            (Aave (..), AaveDatum (..),
                                                   AaveRedeemer (..),
                                                   AaveScript, Reserve (..),
                                                   ReserveId, UserConfig (..), UserConfigId)
import qualified Plutus.Contracts.Core            as Core
import           Plutus.Contracts.Currency        as Currency
import qualified Plutus.Contracts.FungibleToken   as FungibleToken
import           Plutus.State.Select              (StateOutput (..))
import qualified Plutus.State.Select              as Select
import           Plutus.State.Update              (PutStateHandle (..),
                                                   StateHandle (..))
import qualified Plutus.State.Update              as Update
import           Plutus.V1.Ledger.Ada             (adaValueOf, lovelaceValueOf)
import           Plutus.V1.Ledger.Value           as Value
import qualified PlutusTx
import qualified PlutusTx.AssocMap                as AssocMap
import           PlutusTx.Prelude                 hiding (Functor (..),
                                                   Semigroup (..), unless)
import           Prelude                          (Semigroup (..), fmap)
import qualified Prelude

findOutputsBy :: HasBlockchainActions s => Aave -> AssetClass -> (AaveDatum -> Maybe a) -> Contract w s Text [StateOutput a]
findOutputsBy aave = Select.findOutputsBy (Core.aaveAddress aave)

findOutputBy :: HasBlockchainActions s => Aave -> AssetClass -> (AaveDatum -> Maybe a) -> Contract w s Text (StateOutput a)
findOutputBy aave = Select.findOutputBy (Core.aaveAddress aave)

findAaveOwnerToken :: HasBlockchainActions s => Aave -> Contract w s Text (StateOutput ())
findAaveOwnerToken aave@Aave{..} = findOutputBy aave aaveProtocolInst pickOwnerDatum

pickOwnerDatum :: AaveDatum -> Maybe ()
pickOwnerDatum LendingPoolDatum = Just ()
pickOwnerDatum _                = Nothing

reserveStateToken, userStateToken :: Aave -> AssetClass
reserveStateToken aave = Update.makeStateToken (aaveProtocolInst aave) "aaveReserve"
userStateToken aave = Update.makeStateToken (aaveProtocolInst aave) "aaveUser"

findAaveReserves :: HasBlockchainActions s => Aave -> Contract w s Text (StateOutput (AssocMap.Map ReserveId Reserve))
findAaveReserves aave = findOutputBy aave (reserveStateToken aave) pickReserves

pickReserves :: AaveDatum -> Maybe (AssocMap.Map ReserveId Reserve)
pickReserves (Core.ReservesDatum r) = Just r
pickReserves _                      = Nothing

findAaveReserve :: HasBlockchainActions s => Aave -> ReserveId -> Contract w s Text Reserve
findAaveReserve aave reserveId = do
    reserves <- soValue <$> findAaveReserves aave
    maybe (throwError "Reserve not found") pure $ AssocMap.lookup reserveId reserves

findAaveUserConfigs :: HasBlockchainActions s => Aave -> Contract w s Text (StateOutput (AssocMap.Map UserConfigId UserConfig))
findAaveUserConfigs aave = findOutputBy aave (userStateToken aave) pickUserConfig

pickUserConfig :: AaveDatum -> Maybe (AssocMap.Map UserConfigId UserConfig)
pickUserConfig (Core.UserConfigsDatum userConfig) = Just userConfig
pickUserConfig _                           = Nothing

findAaveUserConfig :: HasBlockchainActions s => Aave -> UserConfigId -> Contract w s Text UserConfig
findAaveUserConfig aave userConfigId = do
    configs <- soValue <$> findAaveUserConfigs aave
    maybe (throwError "UserConfig not found") pure $ AssocMap.lookup userConfigId configs

putState :: (HasBlockchainActions s) => Aave -> StateHandle AaveScript a -> a -> Contract w s Text ()
putState aave stateHandle newState = do
    ownerTokenOutput <- fmap (const Core.LendingPoolDatum) <$> findAaveOwnerToken aave
    Update.putState
        PutStateHandle { script = Core.aaveInstance aave, ownerToken = aaveProtocolInst aave, ownerTokenOutput = ownerTokenOutput }
        stateHandle
        newState

updateState :: (HasBlockchainActions s) => Aave ->  StateHandle AaveScript a -> StateOutput a -> Contract w s Text ()
updateState aave = Update.updateState (Core.aaveInstance aave)

makeReserveHandle :: Aave -> (AssocMap.Map ReserveId Reserve -> AaveRedeemer) -> StateHandle AaveScript (AssocMap.Map ReserveId Reserve)
makeReserveHandle aave toRedeemer =
    StateHandle {
        stateToken = reserveStateToken aave,
        toDatum = Core.ReservesDatum,
        toRedeemer = toRedeemer
    }

putReserves :: (HasBlockchainActions s) => Aave -> AssocMap.Map ReserveId Reserve -> Contract w s Text ()
putReserves aave = putState aave $ makeReserveHandle aave Core.CreateReservesRedeemer

updateReserves :: (HasBlockchainActions s) => Aave -> StateOutput (AssocMap.Map ReserveId Reserve) -> Contract w s Text ()
updateReserves aave = updateState aave $ makeReserveHandle aave (const Core.UpdateReservesRedeemer)

updateReserve :: (HasBlockchainActions s) => Aave -> ReserveId -> Reserve -> Contract w s Text ()
updateReserve aave reserveId reserve = do
    reservesOutput <- findAaveReserves aave
    _ <- maybe (throwError "Update failed: reserve not found") pure $
        AssocMap.lookup reserveId (soValue reservesOutput)
    updateReserves aave $ Prelude.fmap (AssocMap.insert reserveId reserve) reservesOutput

makeUserHandle :: Aave -> (AssocMap.Map UserConfigId UserConfig -> AaveRedeemer) -> StateHandle AaveScript (AssocMap.Map UserConfigId UserConfig)
makeUserHandle aave toRedeemer =
    StateHandle {
        stateToken = userStateToken aave,
        toDatum = Core.UserConfigsDatum,
        toRedeemer = toRedeemer
    }

putUserConfigs :: (HasBlockchainActions s) => Aave -> AssocMap.Map UserConfigId UserConfig -> Contract w s Text ()
putUserConfigs aave = putState aave $ makeUserHandle aave Core.CreateUserConfigsRedeemer

updateUserConfigs :: (HasBlockchainActions s) => Aave -> StateOutput (AssocMap.Map UserConfigId UserConfig) -> Contract w s Text ()
updateUserConfigs aave = updateState aave $ makeUserHandle aave (const Core.UpdateUserConfigsRedeemer)

addUserConfig :: (HasBlockchainActions s) => Aave -> UserConfigId -> UserConfig -> Contract w s Text ()
addUserConfig aave userConfigId userConfig = do
    configsOutput <- findAaveUserConfigs aave
    _ <- maybe (pure ()) (const $ throwError "Add user config failed: config exists") $
        AssocMap.lookup userConfigId (soValue configsOutput)
    updateUserConfigs aave $ Prelude.fmap (AssocMap.insert userConfigId userConfig) configsOutput

updateUserConfig :: (HasBlockchainActions s) => Aave -> UserConfigId -> UserConfig -> Contract w s Text ()
updateUserConfig aave userConfigId userConfig = do
    configsOutput <- findAaveUserConfigs aave
    _ <- maybe (throwError "Update failed: user config not found") pure $
        AssocMap.lookup userConfigId (soValue configsOutput)
    updateUserConfigs aave $ Prelude.fmap (AssocMap.insert userConfigId userConfig) configsOutput
