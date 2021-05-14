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
                                                   AaveRedeemer (..), Factory,
                                                   Reserve (..), ReserveId,
                                                   UserConfig (..))
import qualified Plutus.Contracts.Core            as Core
import           Plutus.Contracts.Currency        as Currency
import qualified Plutus.Contracts.FungibleToken   as FungibleToken
import           Plutus.V1.Ledger.Ada             (adaValueOf, lovelaceValueOf)
import           Plutus.V1.Ledger.Value           as Value
import qualified PlutusTx
import           PlutusTx.Prelude                 hiding (Functor (..),
                                                   Semigroup (..), unless)
import           Prelude                          (Semigroup (..), fmap)
import qualified Prelude

data StateOutput a =
    StateOutput {
        soOutRef :: TxOutRef,
        soOutTx  :: TxOutTx,
        soDatum  :: a
    } deriving (Prelude.Show, Prelude.Functor)

getAaveDatum :: TxOutTx -> Contract w s Text AaveDatum
getAaveDatum o = case txOutDatumHash $ txOutTxOut o of
        Nothing -> throwError "datumHash not found"
        Just h -> case Map.lookup h $ txData $ txOutTxTx o of
            Nothing -> throwError "datum not found"
            Just (Datum e) -> case PlutusTx.fromData e of
                Nothing -> throwError "datum has wrong type"
                Just d  -> return d

getAaveState :: HasBlockchainActions s => Aave -> Contract w s Text [StateOutput AaveDatum]
getAaveState aave = do
    utxos <- utxoAt (Core.aaveAddress aave)
    traverse getDatum . Map.toList $ utxos
  where
    getDatum (oref, o) = do
        d <- getAaveDatum o
        pure $ StateOutput oref o d

findOutputsBy :: forall w s a. HasBlockchainActions s => Aave -> AssetClass -> (AaveDatum -> Maybe a) -> Contract w s Text [StateOutput a]
findOutputsBy aave stateToken mapDatum = mapMaybe checkStateToken <$> getAaveState aave
    where
        checkStateToken (StateOutput oref outTx datum) =
            if assetClassValueOf (txOutValue $ txOutTxOut outTx) stateToken == 1
                then fmap (StateOutput oref outTx) (mapDatum datum)
                else Nothing

findOutputBy :: forall w s a. HasBlockchainActions s => Aave -> AssetClass -> (AaveDatum -> Maybe a) -> Contract w s Text (StateOutput a)
findOutputBy aave stateToken mapDatum = do
    outputs <- findOutputsBy aave stateToken mapDatum
    let stateName = Text.pack . Prelude.show . Prelude.snd . unAssetClass $ stateToken
    case outputs of
        [output] -> pure output
        []       -> throwError $ stateName <> " not found"
        xs       -> throwError $ "Multiple " <> stateName <> " found"

pickFactory :: AaveDatum -> Maybe Factory
pickFactory (FactoryDatum f) = Just f
pickFactory _                = Nothing

findAaveFactory :: HasBlockchainActions s => Aave -> Contract w s Text (StateOutput Factory)
findAaveFactory aave@Aave{..} = findOutputBy aave aaveProtocolInst pickFactory

findAaveReserve :: HasBlockchainActions s => Aave -> ReserveId -> Contract w s Text (StateOutput Reserve)
findAaveReserve aave reserveId = findOutputBy aave (Core.reserveStateToken aave) mapState
    where
        mapState (Core.ReserveDatum r) =
            if rCurrency r == reserveId
                then Just r
                else Nothing
        mapState _ = Nothing

findAaveUser :: HasBlockchainActions s => Aave -> PubKeyHash -> ReserveId -> Contract w s Text (StateOutput UserConfig)
findAaveUser aave userAddress reserveId = findOutputBy aave (Core.userStateToken aave) mapState
    where
        mapState (UserConfigDatum user) =
            if ucAddress user == userAddress && ucReserveId user == reserveId
                then Just user
                else Nothing
        mapState _ = Nothing

data StateHandle a = StateHandle {
    getToken :: Aave -> AssetClass,
    toDatum  :: a -> AaveDatum,
    toAction :: a -> AaveRedeemer
}

putState :: (HasBlockchainActions s) => StateHandle a -> Aave -> a -> Contract w s Text a
putState StateHandle{..} aave datum = do
    let stateToken = getToken aave
        lookups = Constraints.scriptInstanceLookups (Core.aaveInstance aave)
            <> Constraints.monetaryPolicy (Core.makeStatePolicy (Prelude.snd . unAssetClass $ stateToken) aave)
        tx = mustForgeValue (assetClassValue stateToken 1)
            <> mustPayToTheScript (toDatum datum) (assetClassValue stateToken 1)
    ledgerTx <- submitTxConstraintsWith lookups tx
    _ <- awaitTxConfirmed $ txId ledgerTx
    pure datum

updateState :: (HasBlockchainActions s) => StateHandle a -> Aave -> StateOutput a -> Contract w s Text a
updateState StateHandle{..} aave (StateOutput oref o datum) = do
    let stateToken = getToken aave
        lookups = Constraints.scriptInstanceLookups (Core.aaveInstance aave)
            <> Constraints.otherScript (Core.aaveScript aave)
            <> Constraints.unspentOutputs (Map.singleton oref o)
        tx = mustPayToTheScript (toDatum datum) (assetClassValue stateToken 1)
            <> mustSpendScriptOutput oref (Redeemer $ PlutusTx.toData (toAction datum))
    ledgerTx <- submitTxConstraintsWith lookups tx
    _ <- awaitTxConfirmed $ txId ledgerTx
    pure datum

makeReserveHandle :: (Reserve -> AaveRedeemer) -> StateHandle Reserve
makeReserveHandle toAction =
    StateHandle {
        getToken = Core.reserveStateToken,
        toDatum = Core.ReserveDatum,
        toAction = toAction
    }

pickReserve :: AaveDatum -> Maybe Reserve
pickReserve (Core.ReserveDatum r) = Just r
pickReserve _                     = Nothing

putReserve :: (HasBlockchainActions s) => Aave -> Reserve -> Contract w s Text Reserve
putReserve = putState $ makeReserveHandle Core.CreateReserveRedeemer

updateReserve :: (HasBlockchainActions s) => Aave -> StateOutput Reserve -> Contract w s Text Reserve
updateReserve = updateState $ makeReserveHandle (const Core.UpdateReserveRedeemer)

makeUserHandle :: (UserConfig -> AaveRedeemer) -> StateHandle UserConfig
makeUserHandle toAction =
    StateHandle {
        getToken = Core.userStateToken,
        toDatum = Core.UserConfigDatum,
        toAction = toAction
    }

pickUserConfig :: AaveDatum -> Maybe UserConfig
pickUserConfig (Core.UserConfigDatum user) = Just user
pickUserConfig _                           = Nothing

putUser :: (HasBlockchainActions s) => Aave -> UserConfig -> Contract w s Text UserConfig
putUser = putState $ makeUserHandle Core.CreateUserRedeemer

updateUser :: (HasBlockchainActions s) => Aave -> StateOutput UserConfig -> Contract w s Text UserConfig
updateUser = updateState $ makeUserHandle (const Core.UpdateUserRedeemer)
