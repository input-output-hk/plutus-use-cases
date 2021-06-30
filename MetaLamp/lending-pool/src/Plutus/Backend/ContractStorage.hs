{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Plutus.Backend.ContractStorage where

import Control.Monad.IO.Class (MonadIO (..))
import Wallet.Emulator.Types (Wallet (..))
import Wallet.Types (ContractInstanceId)

type WithContractStorage = (?contractStorage :: ContractStorage)

type Endpoint = String

data ContractStorage = ContractStorage
  { getContractIdFromStorage :: forall m. MonadIO m => Wallet -> Endpoint -> m (Maybe ContractInstanceId),
    saveContractIdToStorage :: forall m. MonadIO m => Wallet -> Endpoint -> ContractInstanceId -> m ()
  }

withContractStorage :: ContractStorage -> (WithContractStorage => a) -> a
withContractStorage cs a = let ?contractStorage = cs in a

getContractId :: WithContractStorage => MonadIO m => Wallet -> Endpoint -> m (Maybe ContractInstanceId)
getContractId = let ContractStorage {..} = ?contractStorage in getContractIdFromStorage

saveContractId :: WithContractStorage => MonadIO m => Wallet -> Endpoint -> ContractInstanceId -> m ()
saveContractId = let ContractStorage {..} = ?contractStorage in saveContractIdToStorage
