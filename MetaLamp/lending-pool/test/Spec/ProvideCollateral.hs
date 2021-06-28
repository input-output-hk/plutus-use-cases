{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Spec.ProvideCollateral where

import           Control.Lens               (over)
import qualified Data.Map                   as Map
import qualified Fixtures
import           Plutus.Contract.Test
import qualified Plutus.Contracts.Core      as Aave
import qualified Plutus.Contracts.Endpoints as Aave
import qualified Plutus.Trace.Emulator      as Trace
import           Plutus.V1.Ledger.Value     (AssetClass, assetClassValue)
import qualified PlutusTx.AssocMap          as AssocMap
import           Spec.Deposit               (deposit)
import qualified Spec.Shared                as Shared
import           Test.Tasty
import qualified Utils.Data                 as Utils

tests :: TestTree
tests = testGroup "provideCollateral" [
    checkPredicate
        "Should succeed if user's aToken balance is sufficient"
        (walletFundsChange
            Fixtures.lenderWallet
            (Fixtures.initialFunds <>
            assetClassValue Fixtures.mogus (negate 100))
        .&&. Shared.reservesChange (Utils.modifyAt (over Aave._rAmount (+100)) Fixtures.mogus Fixtures.initialReserves)
        .&&. Shared.userConfigsChange
            (AssocMap.insert
            (Fixtures.mogus, Utils.getPubKey Fixtures.lenderWallet)
            (Aave.UserConfig { Aave.ucDebt = 0, Aave.ucCollateralizedInvestment = 100 })
            Fixtures.initialUsers)
        )
        $ do
            handles <- Fixtures.defaultTrace
            deposit (handles Map.! Fixtures.lenderWallet) Fixtures.lenderWallet Fixtures.mogus 100
            provideCollateral (handles Map.! Fixtures.lenderWallet) Fixtures.lenderWallet Fixtures.mogus 100,
    checkPredicate
        "Should fail if user's aToken balance is insufficient"
        (walletFundsChange Fixtures.lenderWallet Fixtures.initialFunds
        .&&. Shared.reservesChange Fixtures.initialReserves
        .&&. Shared.userConfigsChange Fixtures.initialUsers
        .&&. assertAccumState Fixtures.userContract (Trace.walletInstanceTag Fixtures.lenderWallet) Utils.isLastError "Contract last state is an error"
        )
        $ do
            handles <- Fixtures.defaultTrace
            provideCollateral (handles Map.! Fixtures.lenderWallet) Fixtures.lenderWallet Fixtures.mogus 100
    ]

provideCollateral :: Fixtures.UserHandle -> Wallet -> AssetClass -> Integer -> Trace.EmulatorTrace ()
provideCollateral userHandle wallet asset amount = do
    Trace.callEndpoint @"provideCollateral" userHandle $ Aave.ProvideCollateralParams asset amount (Utils.getPubKey wallet)
    _ <- Trace.waitNSlots 3
    pure ()
