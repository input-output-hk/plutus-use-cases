{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Spec.Deposit where

import qualified Data.Map                   as Map
import qualified Fixtures
import           Plutus.Contract.Test
import qualified Plutus.Contracts.Endpoints as Aave
import qualified Plutus.Trace.Emulator      as Trace
import           Plutus.V1.Ledger.Value     (AssetClass, assetClassValue)
import qualified Spec.Shared                as Shared
import           Test.Tasty
import qualified Utils.Data                 as Utils

tests :: TestTree
tests = testGroup "deposit" [
    checkPredicate
        "Should succeed if user's wallet balance is sufficient"
        (walletFundsChange
            Fixtures.lenderWallet
            (Fixtures.initialFunds <>
            assetClassValue Fixtures.mogus (negate 100) <> assetClassValue Fixtures.amogus 100)
        .&&. Shared.reservesChange (Shared.modifyAmount (+100) Fixtures.mogus Fixtures.initialReserves)
        )
        $ do
            handles <- Fixtures.defaultTrace
            deposit (handles Map.! Fixtures.lenderWallet) Fixtures.mogus 100,
    checkPredicate
        "Should fail if user's wallet balance is insufficient"
        (walletFundsChange Fixtures.lenderWallet Fixtures.initialFunds
        .&&. Shared.reservesChange Fixtures.initialReserves
        .&&. assertAccumState Fixtures.userContract (Trace.walletInstanceTag Fixtures.lenderWallet) Utils.isLastError "Contract last state is an error"
        )
        $ do
            handles <- Fixtures.defaultTrace
            deposit (handles Map.! Fixtures.lenderWallet) Fixtures.mogus 10000
    ]

deposit :: Fixtures.UserHandle -> AssetClass -> Integer -> Trace.EmulatorTrace ()
deposit userHandle asset amount = do
    pkh <- Shared.getPubKey userHandle
    Trace.callEndpoint @"deposit" userHandle $ Aave.DepositParams asset pkh amount
    _ <- Trace.waitNSlots 3
    pure ()
