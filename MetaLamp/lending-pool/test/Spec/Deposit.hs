{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Spec.Deposit where

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
import qualified Spec.Shared                                as Shared
import           Test.Tasty
import qualified Utils.Data                                 as Utils

tests :: TestTree
tests = testGroup "deposit" [
    checkPredicate
        "Should succeed if user's wallet balance is sufficient"
        (walletFundsChange
            Fixtures.lenderWallet
            (Fixtures.initialFunds <>
            assetClassValue Fixtures.mogus (negate 100) <> assetClassValue Fixtures.amogus 100)
        .&&. Shared.reservesChange (Utils.modifyAt (over Aave._rAmount (+100)) Fixtures.mogus Fixtures.initialReserves)
        .&&. Shared.userConfigsChange
            (
                AssocMap.insert
                (Fixtures.mogus, Utils.getPubKey Fixtures.lenderWallet)
                (Aave.UserConfig { Aave.ucDebt = 0, Aave.ucCollateralizedInvestment = 0 })
                $ Fixtures.initialUsers
            )
        )
        $ do
            handles <- Fixtures.defaultTrace
            deposit (handles Map.! Fixtures.lenderWallet) Fixtures.lenderWallet Fixtures.mogus 100,
    checkPredicate
        "Should fail if user's wallet balance is insufficient"
        (walletFundsChange Fixtures.lenderWallet Fixtures.initialFunds
        .&&. Shared.reservesChange Fixtures.initialReserves
        .&&. Shared.userConfigsChange Fixtures.initialUsers
        .&&. assertAccumState Fixtures.userContract (Trace.walletInstanceTag Fixtures.lenderWallet) Utils.isLastError "Contract last state is an error"
        )
        $ do
            handles <- Fixtures.defaultTrace
            deposit (handles Map.! Fixtures.lenderWallet) Fixtures.lenderWallet Fixtures.mogus 10000
    ]

deposit :: Fixtures.UserHandle -> Wallet -> AssetClass -> Integer -> Trace.EmulatorTrace ()
deposit userHandle wallet asset amount = do
    Trace.callEndpoint @"deposit" userHandle $ Aave.DepositParams asset (Utils.getPubKey wallet) amount
    _ <- Trace.waitNSlots 3
    pure ()
