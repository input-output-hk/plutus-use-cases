{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveFunctor              #-}
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
                                                   AaveScript, Reserve (..), ReserveId,
                                                   UserConfig (..))
import qualified Plutus.Contracts.Core            as Core
import           Plutus.Contracts.Currency        as Currency
import qualified Plutus.Contracts.FungibleToken   as FungibleToken
import           Plutus.State.Select              (StateOutput (..))
import qualified Plutus.State.Select              as Select
import           Plutus.State.Update              (RootStateHandle (..),
                                                   StateHandle (..))
import qualified Plutus.State.Update              as Update
import           Plutus.V1.Ledger.Ada             (adaValueOf, lovelaceValueOf)
import           Plutus.V1.Ledger.Value           as Value
import qualified PlutusTx
import           PlutusTx.Prelude                 hiding (Functor (..),
                                                   Semigroup (..), unless)
import           Prelude                          (Semigroup (..), fmap)
import qualified Prelude

findOutputsBy :: HasBlockchainActions s => Aave -> AssetClass -> (AaveDatum -> Maybe a) -> Contract w s Text [StateOutput a]
findOutputsBy aave = Select.findOutputsBy (Core.aaveAddress aave)

findOutputBy :: HasBlockchainActions s => Aave -> AssetClass -> (AaveDatum -> Maybe a) -> Contract w s Text (StateOutput a)
findOutputBy aave = Select.findOutputBy (Core.aaveAddress aave)

findAaveRoot :: HasBlockchainActions s => Aave -> Contract w s Text (StateOutput ())
findAaveRoot aave@Aave{..} = findOutputBy aave aaveProtocolInst pickLendingPool

pickLendingPool :: AaveDatum -> Maybe ()
pickLendingPool LendingPoolDatum = Just ()
pickLendingPool _                = Nothing

reserveStateToken, userStateToken :: Aave -> AssetClass
reserveStateToken aave = Update.makeStateToken (aaveProtocolInst aave) "aaveReserve"
userStateToken aave = Update.makeStateToken (aaveProtocolInst aave) "aaveUser"

findAaveReserve :: HasBlockchainActions s => Aave -> ReserveId -> Contract w s Text (StateOutput Reserve)
findAaveReserve aave reserveId = findOutputBy aave (reserveStateToken aave) mapState
    where
        mapState (Core.ReserveDatum r) =
            if rCurrency r == reserveId
                then Just r
                else Nothing
        mapState _ = Nothing

findAaveUser :: HasBlockchainActions s => Aave -> PubKeyHash -> ReserveId -> Contract w s Text (StateOutput UserConfig)
findAaveUser aave userAddress reserveId = findOutputBy aave (userStateToken aave) mapState
    where
        mapState (UserConfigDatum user) =
            if ucAddress user == userAddress && ucReserveId user == reserveId
                then Just user
                else Nothing
        mapState _ = Nothing

stateRootHandle :: (HasBlockchainActions s) => Aave -> Contract w s Text (RootStateHandle AaveScript AaveDatum)
stateRootHandle aave = do
    output <- fmap (const Core.LendingPoolDatum) <$> findAaveRoot aave
    pure $
        RootStateHandle { script = Core.aaveInstance aave, rootToken = aaveProtocolInst aave, output = output }

putState :: (HasBlockchainActions s) => Aave -> StateHandle AaveRedeemer AaveDatum a -> a -> Contract w s Text a
putState aave stateHandle newState = do
    rootHandle <- stateRootHandle aave
    Update.putState rootHandle stateHandle newState

updateState :: (HasBlockchainActions s) => Aave ->  StateHandle AaveRedeemer AaveDatum a -> StateOutput a -> Contract w s Text a
updateState aave stateHandle newOutput = do
    rootHandle <- stateRootHandle aave
    Update.updateState rootHandle stateHandle newOutput

makeReserveHandle :: Aave -> (Reserve -> AaveRedeemer) -> StateHandle AaveRedeemer AaveDatum Reserve
makeReserveHandle aave toRedeemer =
    StateHandle {
        stateToken = reserveStateToken aave,
        toDatum = Core.ReserveDatum,
        toRedeemer = toRedeemer
    }

pickReserve :: AaveDatum -> Maybe Reserve
pickReserve (Core.ReserveDatum r) = Just r
pickReserve _                     = Nothing

putReserve :: (HasBlockchainActions s) => Aave -> Reserve -> Contract w s Text Reserve
putReserve aave = putState aave $ makeReserveHandle aave Core.CreateReserveRedeemer

updateReserve :: (HasBlockchainActions s) => Aave -> StateOutput Reserve -> Contract w s Text Reserve
updateReserve aave = updateState aave $ makeReserveHandle aave (const Core.UpdateReserveRedeemer)

makeUserHandle :: Aave -> (UserConfig -> AaveRedeemer) -> StateHandle AaveRedeemer AaveDatum UserConfig
makeUserHandle aave toRedeemer =
    StateHandle {
        stateToken = userStateToken aave,
        toDatum = Core.UserConfigDatum,
        toRedeemer = toRedeemer
    }

pickUserConfig :: AaveDatum -> Maybe UserConfig
pickUserConfig (Core.UserConfigDatum user) = Just user
pickUserConfig _                           = Nothing

putUser :: (HasBlockchainActions s) => Aave -> UserConfig -> Contract w s Text UserConfig
putUser aave = putState aave $ makeUserHandle aave Core.CreateUserRedeemer

updateUser :: (HasBlockchainActions s) => Aave -> StateOutput UserConfig -> Contract w s Text UserConfig
updateUser aave = updateState aave $ makeUserHandle aave (const Core.UpdateUserRedeemer)
