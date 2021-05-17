{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module Plutus.State.Select where

import           Control.Monad                    hiding (fmap)
import qualified Data.ByteString                  as BS
import qualified Data.Map                         as Map
import           Data.Text                        (Text, pack)
import qualified Data.Text                        as Text
import           Data.Void                        (Void)
import           Ledger                           hiding (getDatum, singleton)
import           Ledger.Constraints               as Constraints
import           Ledger.Constraints.OnChain       as Constraints
import           Ledger.Constraints.TxConstraints as Constraints
import qualified Ledger.Scripts                   as Scripts
import qualified Ledger.Typed.Scripts             as Scripts
import           Playground.Contract
import           Plutus.Contract                  hiding (when)
import           Plutus.V1.Ledger.Value
import qualified PlutusTx
import           PlutusTx.Prelude                 hiding (Semigroup (..),
                                                   unless)
import           Prelude                          (Semigroup (..))
import qualified Prelude

data StateOutput a =
    StateOutput {
        soOutRef :: TxOutRef,
        soOutTx  :: TxOutTx,
        soDatum  :: a
    } deriving (Prelude.Show, Prelude.Functor)

getDatum :: PlutusTx.IsData a => TxOutTx -> Contract w s Text a
getDatum o = case txOutDatumHash $ txOutTxOut o of
        Nothing -> throwError "datumHash not found"
        Just h -> case Map.lookup h $ txData $ txOutTxTx o of
            Nothing -> throwError "datum not found"
            Just (Datum e) -> case PlutusTx.fromData e of
                Nothing -> throwError "datum has wrong type"
                Just d  -> return d

getState :: (HasBlockchainActions s, PlutusTx.IsData o) => Address -> Contract w s Text [StateOutput o]
getState address = do
    utxos <- utxoAt address
    traverse getDatum' . Map.toList $ utxos
  where
    getDatum' (oref, o) = do
        d <- getDatum o
        pure $ StateOutput oref o d

findOutputsBy :: (HasBlockchainActions s, PlutusTx.IsData o) =>
  Address ->
  AssetClass ->
  (o -> Maybe a) ->
  Contract w s Text [StateOutput a]
findOutputsBy address stateToken mapDatum = mapMaybe checkStateToken <$> getState address
    where
        checkStateToken (StateOutput oref outTx datum) =
            if assetClassValueOf (txOutValue $ txOutTxOut outTx) stateToken == 1
                then fmap (StateOutput oref outTx) (mapDatum datum)
                else Nothing

findOutputBy :: (HasBlockchainActions s, PlutusTx.IsData o) =>
  Address ->
  AssetClass ->
  (o -> Maybe a) ->
  Contract w s Text (StateOutput a)
findOutputBy address stateToken mapDatum = do
    outputs <- findOutputsBy address stateToken mapDatum
    let stateName = Text.pack . Prelude.show . Prelude.snd . unAssetClass $ stateToken
    case outputs of
        [output] -> pure output
        []       -> throwError $ stateName <> " not found"
        xs       -> throwError $ "Multiple " <> stateName <> " found"
