{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Spec.Start where

import qualified Fixtures
import           Plutus.Contract.Test
import qualified Plutus.Contracts.Core as Aave
import           Test.Tasty
import qualified Utils.Data            as Utils
import qualified Utils.Trace           as Utils

tests :: TestTree
tests = testGroup "start" [checkPredicate
        "Should start a new lending pool with a set of available currencies"
        (Utils.datumsAtAddress Fixtures.aaveAddress startDatumValid)
        Fixtures.startTrace]

startDatumValid :: [Aave.AaveDatum] -> Bool
startDatumValid = Utils.allSatisfy . fmap Utils.one $ [hasReserves, hasUsers, hasOperator]
    where
    hasOperator (Aave.LendingPoolDatum _) = True
    hasOperator _                         = False
    hasReserves (Aave.ReservesDatum _ reserves) =
        reserves == Fixtures.initialReserves
    hasReserves _ = False
    hasUsers (Aave.UserConfigsDatum _ users) = users == Fixtures.initialUsers
    hasUsers _                               = False
