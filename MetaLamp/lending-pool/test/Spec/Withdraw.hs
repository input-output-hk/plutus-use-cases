{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Spec.Withdraw where

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
import           Spec.Deposit                               (deposit)
import qualified Spec.Shared                                as Shared
import           Test.Tasty
import qualified Utils.Data                                 as Utils

tests :: TestTree
tests = testGroup "withdraw" [
    checkPredicate
        "Should succeed if user's protocol balance is sufficient"
        (walletFundsChange
            Fixtures.lenderWallet
            (Fixtures.initialFunds <>
            assetClassValue Fixtures.euro (negate 100 + 50) <> assetClassValue Fixtures.aeuro (100 - 50))
        .&&. Shared.reservesChange (Utils.modifyAt (over Aave._rAmount (subtract 50 . (+100))) Fixtures.euro Fixtures.initialReserves)
        .&&. Shared.userConfigsChange
            (
                AssocMap.insert
                (Fixtures.euro, Utils.getPubKey Fixtures.lenderWallet)
                (Aave.UserConfig { Aave.ucDebt = 0, Aave.ucCollateralizedInvestment = 0 })
                $ Fixtures.initialUsers
            )
        )
        $ do
            handles <- Fixtures.defaultTrace
            deposit (handles Map.! Fixtures.lenderWallet) Fixtures.lenderWallet Fixtures.euro 100
            withdraw (handles Map.! Fixtures.lenderWallet) Fixtures.lenderWallet Fixtures.euro 50,
    checkPredicate
    "Should fail if user's protocol balance is insufficient"
    (walletFundsChange Fixtures.lenderWallet (Fixtures.initialFunds <>
        assetClassValue Fixtures.euro (negate 100) <> assetClassValue Fixtures.aeuro 100)
    .&&. Shared.reservesChange (Utils.modifyAt (over Aave._rAmount (+100)) Fixtures.euro Fixtures.initialReserves)
    .&&. Shared.userConfigsChange
        (
            AssocMap.insert
            (Fixtures.euro, Utils.getPubKey Fixtures.lenderWallet)
            (Aave.UserConfig { Aave.ucDebt = 0, Aave.ucCollateralizedInvestment = 0 })
            $ Fixtures.initialUsers
        )
    .&&. assertAccumState Fixtures.userContract (Trace.walletInstanceTag Fixtures.lenderWallet) Utils.isLastError "Contract last state is an error"
    )
    $ do
        handles <- Fixtures.defaultTrace
        deposit (handles Map.! Fixtures.lenderWallet) Fixtures.lenderWallet Fixtures.euro 100
        withdraw (handles Map.! Fixtures.lenderWallet) Fixtures.lenderWallet Fixtures.euro 200
    ]

withdraw :: Fixtures.UserHandle -> Wallet -> AssetClass -> Integer -> Trace.EmulatorTrace ()
withdraw userHandle wallet asset amount = do
    Trace.callEndpoint @"withdraw" userHandle $ Aave.WithdrawParams asset (Utils.getPubKey wallet) amount
    _ <- Trace.waitNSlots 3
    pure ()
