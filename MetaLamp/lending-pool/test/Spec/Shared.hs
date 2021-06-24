{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}

module Spec.Shared where

import           Control.Lens               ((^?))
import qualified Fixtures
import qualified Ledger
import           Plutus.Contract.Test       (TracePredicate)
import qualified Plutus.Contracts.Core      as Aave
import qualified Plutus.Contracts.Endpoints as Aave
import qualified Plutus.Trace.Emulator      as Trace
import           Plutus.V1.Ledger.Value     (AssetClass)
import qualified PlutusTx.AssocMap          as AssocMap
import qualified Utils.Data                 as Utils
import qualified Utils.Trace                as Utils

getPubKey :: Fixtures.UserHandle -> Trace.EmulatorTrace Ledger.PubKeyHash
getPubKey userHandle = do
    _ <- Trace.callEndpoint @"ownPubKey" userHandle ()
    _ <- Trace.waitNSlots 1
    Utils.getState (^? Aave._GetPubKey) userHandle

reservesChange :: AssocMap.Map AssetClass Aave.Reserve -> TracePredicate
reservesChange reserves = Utils.datumsAtAddress Fixtures.aaveAddress (Utils.one check)
    where
        check (Aave.ReservesDatum _ reserves') = reserves' == reserves
        check _                                = False

modifyAmount :: (Integer -> Integer) -> AssetClass -> AssocMap.Map AssetClass Aave.Reserve -> AssocMap.Map AssetClass Aave.Reserve
modifyAmount f = Utils.modifyAt (\r -> r { Aave.rAmount = f . Aave.rAmount $ r })
