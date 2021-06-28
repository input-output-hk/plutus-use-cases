{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Spec.RevokeCollateral where

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
import           Spec.ProvideCollateral     (provideCollateral)
import qualified Spec.Shared                as Shared
import           Test.Tasty
import qualified Utils.Data                 as Utils

tests :: TestTree
tests = testGroup "revokeCollateral" [
    checkPredicate
        "Should succeed if user's investment is sufficient"
        (walletFundsChange
            Fixtures.lenderWallet
            (Fixtures.initialFunds <>
            assetClassValue Fixtures.mogus (negate 100) <> assetClassValue Fixtures.amogus (100 - 100 + 50))
        .&&. Shared.reservesChange (Utils.modifyAt (over Aave._rAmount (+100)) Fixtures.mogus Fixtures.initialReserves)
        .&&. Shared.userConfigsChange
            (AssocMap.insert
            (Fixtures.mogus, Utils.getPubKey Fixtures.lenderWallet)
            (Aave.UserConfig { Aave.ucDebt = 0, Aave.ucCollateralizedInvestment = 50 })
            Fixtures.initialUsers)
        )
        $ do
            handles <- Fixtures.defaultTrace
            deposit (handles Map.! Fixtures.lenderWallet) Fixtures.lenderWallet Fixtures.mogus 100
            provideCollateral (handles Map.! Fixtures.lenderWallet) Fixtures.lenderWallet Fixtures.mogus 100
            revokeCollateral (handles Map.! Fixtures.lenderWallet) Fixtures.lenderWallet Fixtures.mogus 50,
    checkPredicate
        "Should fail if user's investment is insufficient"
        (walletFundsChange Fixtures.lenderWallet Fixtures.initialFunds
        .&&. Shared.reservesChange Fixtures.initialReserves
        .&&. assertAccumState Fixtures.userContract (Trace.walletInstanceTag Fixtures.lenderWallet) Utils.isLastError "Contract last state is an error"
        )
        $ do
            handles <- Fixtures.defaultTrace
            revokeCollateral (handles Map.! Fixtures.lenderWallet) Fixtures.lenderWallet Fixtures.mogus 100
    ]

revokeCollateral :: Fixtures.UserHandle -> Wallet -> AssetClass -> Integer -> Trace.EmulatorTrace ()
revokeCollateral userHandle wallet asset amount = do
    Trace.callEndpoint @"revokeCollateral" userHandle $ Aave.RevokeCollateralParams asset amount (Utils.getPubKey wallet)
    _ <- Trace.waitNSlots 3
    pure ()
