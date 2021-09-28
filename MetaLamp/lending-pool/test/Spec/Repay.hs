{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Spec.Repay where

import           Control.Lens                               (over)
import qualified Data.Map                                   as Map
import qualified Fixtures
import           Plutus.Contract.Test
import qualified Plutus.Contracts.LendingPool.OffChain.User as Aave
import qualified Plutus.Contracts.LendingPool.OnChain.Core  as Aave
import qualified Plutus.Trace.Emulator                      as Trace
import           Plutus.V1.Ledger.Value                     (AssetClass,
                                                             assetClassValue)
import qualified PlutusTx.AssocMap                          as AssocMap
import           Spec.Borrow                                (borrow)
import           Spec.Deposit                               (deposit)
import           Spec.ProvideCollateral                     (provideCollateral)
import qualified Spec.Shared                                as Shared
import           Test.Tasty
import qualified Utils.Data                                 as Utils

tests :: TestTree
tests = testGroup "repay" [
    checkPredicate
        "Should succeed if user has a debt and funds to pay"
        (walletFundsChange
            Fixtures.lenderWallet
            (Fixtures.initialFunds <>
            assetClassValue Fixtures.usd (negate 100) <> assetClassValue Fixtures.ausd 100)
        .&&.
        walletFundsChange
            Fixtures.borrowerWallet
            (Fixtures.initialFunds <>
            assetClassValue Fixtures.euro (negate 100) <> assetClassValue Fixtures.usd (50 - 25))
        .&&. Shared.reservesChange (
            Utils.modifyAt (over Aave._rAmount (+100)) Fixtures.euro
            . Utils.modifyAt (over Aave._rAmount ((+25) . subtract 50 . (+100))) Fixtures.usd $ Fixtures.initialReserves)
        .&&. Shared.userConfigsChange
            (
                AssocMap.insert
                (Fixtures.usd, Utils.getPubKey Fixtures.borrowerWallet)
                (Aave.UserConfig { Aave.ucDebt = 50 - 25, Aave.ucCollateralizedInvestment = 0 })
                .
                AssocMap.insert
                (Fixtures.euro, Utils.getPubKey Fixtures.borrowerWallet)
                (Aave.UserConfig { Aave.ucDebt = 0, Aave.ucCollateralizedInvestment = 100 })
                .
                AssocMap.insert
                (Fixtures.usd, Utils.getPubKey Fixtures.lenderWallet)
                (Aave.UserConfig { Aave.ucDebt = 0, Aave.ucCollateralizedInvestment = 0 })
                $ Fixtures.initialUsers
            )
        )
        $ do
            handles <- Fixtures.defaultTrace
            deposit (handles Map.! Fixtures.lenderWallet) Fixtures.lenderWallet Fixtures.usd 100

            deposit (handles Map.! Fixtures.borrowerWallet) Fixtures.borrowerWallet Fixtures.euro 100
            provideCollateral (handles Map.! Fixtures.borrowerWallet) Fixtures.borrowerWallet Fixtures.euro 100
            borrow (handles Map.! Fixtures.borrowerWallet) Fixtures.borrowerWallet Fixtures.usd 50
            repay (handles Map.! Fixtures.borrowerWallet) Fixtures.borrowerWallet Fixtures.usd 25
            ,
    checkPredicate
        "Should fail if user has no debt"
        (walletFundsChange Fixtures.lenderWallet Fixtures.initialFunds
        .&&. walletFundsChange Fixtures.borrowerWallet Fixtures.initialFunds
        .&&. Shared.reservesChange Fixtures.initialReserves
        .&&. Shared.userConfigsChange Fixtures.initialUsers
        .&&. assertAccumState Fixtures.userContract (Trace.walletInstanceTag Fixtures.lenderWallet) Utils.isLastError "Contract last state is an error"
        )
        $ do
            handles <- Fixtures.defaultTrace
            repay (handles Map.! Fixtures.lenderWallet) Fixtures.lenderWallet Fixtures.usd 100
    ]

repay :: Fixtures.UserHandle -> Wallet -> AssetClass -> Integer -> Trace.EmulatorTrace ()
repay userHandle wallet asset amount = do
    Trace.callEndpoint @"repay" userHandle $ Aave.RepayParams asset amount (Utils.getPubKey wallet)
    _ <- Trace.waitNSlots 3
    pure ()
