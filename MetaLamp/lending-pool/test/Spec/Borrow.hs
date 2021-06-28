{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Spec.Borrow where

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
tests = testGroup "borrow" [
    checkPredicate
        "Should succeed if user's collateral is sufficient"
        (walletFundsChange
            Fixtures.lenderWallet
            (Fixtures.initialFunds <>
            assetClassValue Fixtures.usd (negate 100) <> assetClassValue Fixtures.ausd 100)
        .&&.
        walletFundsChange
            Fixtures.borrowerWallet
            (Fixtures.initialFunds <>
            assetClassValue Fixtures.mogus (negate 100) <> assetClassValue Fixtures.usd 50)
        .&&. Shared.reservesChange (
            Utils.modifyAt (over Aave._rAmount (+100)) Fixtures.mogus
            . Utils.modifyAt (over Aave._rAmount (subtract 50 . (+100))) Fixtures.usd $ Fixtures.initialReserves)
        .&&. Shared.userConfigsChange
            (
                AssocMap.insert
                (Fixtures.usd, Utils.getPubKey Fixtures.borrowerWallet)
                (Aave.UserConfig { Aave.ucDebt = 50, Aave.ucCollateralizedInvestment = 0 })
                .
                AssocMap.insert
                (Fixtures.mogus, Utils.getPubKey Fixtures.borrowerWallet)
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

            deposit (handles Map.! Fixtures.borrowerWallet) Fixtures.borrowerWallet Fixtures.mogus 100
            provideCollateral (handles Map.! Fixtures.borrowerWallet) Fixtures.borrowerWallet Fixtures.mogus 100
            borrow (handles Map.! Fixtures.borrowerWallet) Fixtures.borrowerWallet Fixtures.usd 50,
    checkPredicate
        "Should fail if user's collateral is insufficient"
        (walletFundsChange Fixtures.lenderWallet Fixtures.initialFunds
        .&&. walletFundsChange Fixtures.borrowerWallet Fixtures.initialFunds
        .&&. Shared.reservesChange Fixtures.initialReserves
        .&&. Shared.userConfigsChange Fixtures.initialUsers
        .&&. assertAccumState Fixtures.userContract (Trace.walletInstanceTag Fixtures.lenderWallet) Utils.isLastError "Contract last state is an error"
        )
        $ do
            handles <- Fixtures.defaultTrace
            borrow (handles Map.! Fixtures.lenderWallet) Fixtures.lenderWallet Fixtures.usd 100
    ]

borrow :: Fixtures.UserHandle -> Wallet -> AssetClass -> Integer -> Trace.EmulatorTrace ()
borrow userHandle wallet asset amount = do
    Trace.callEndpoint @"borrow" userHandle $ Aave.BorrowParams asset amount (Utils.getPubKey wallet)
    _ <- Trace.waitNSlots 3
    pure ()
