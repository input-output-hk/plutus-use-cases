{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Spec.Withdraw where

import qualified Data.Map                   as Map
import qualified Fixtures
import           Plutus.Contract.Test
import qualified Plutus.Contracts.Endpoints as Aave
import qualified Plutus.Trace.Emulator      as Trace
import           Plutus.V1.Ledger.Value     (AssetClass, assetClassValue)
import           Spec.Deposit               (deposit)
import qualified Spec.Shared                as Shared
import           Test.Tasty
import qualified Utils.Data as Utils

tests :: TestTree
tests = testGroup "withdraw" [
    checkPredicate
        "Should succeed if user's protocol balance is sufficient"
        (walletFundsChange
            Fixtures.lenderWallet
            (Fixtures.initialFunds <>
            assetClassValue Fixtures.mogus (negate 100) <> assetClassValue Fixtures.amogus 100 <>
            assetClassValue Fixtures.mogus 50 <> assetClassValue Fixtures.amogus (negate 50))
        .&&. Shared.reservesChange (Shared.modifyAmount (subtract 50 . (+100)) Fixtures.mogus Fixtures.initialReserves)
        )
        $ do
            handles <- Fixtures.initTrace
            deposit (handles Map.! Fixtures.lenderWallet) Fixtures.mogus 100
            withdraw (handles Map.! Fixtures.lenderWallet) Fixtures.mogus 50,
    checkPredicate
    "Should fail if user's protocol balance is insufficient"
    (walletFundsChange Fixtures.lenderWallet (Fixtures.initialFunds <>
        assetClassValue Fixtures.mogus (negate 100) <> assetClassValue Fixtures.amogus 100)
    .&&. Shared.reservesChange (Shared.modifyAmount (+100) Fixtures.mogus Fixtures.initialReserves)
    .&&. assertAccumState Fixtures.userContract (Trace.walletInstanceTag Fixtures.lenderWallet) Utils.isLastError "Contract last state is an error"
    )
    $ do
        handles <- Fixtures.initTrace
        deposit (handles Map.! Fixtures.lenderWallet) Fixtures.mogus 100
        withdraw (handles Map.! Fixtures.lenderWallet) Fixtures.mogus 200
    ]

withdraw :: Fixtures.UserHandle -> AssetClass -> Integer -> Trace.EmulatorTrace ()
withdraw userHandle asset amount = do
    pkh <- Shared.getPubKey userHandle
    Trace.callEndpoint @"withdraw" userHandle $ Aave.WithdrawParams asset pkh amount
    _ <- Trace.waitNSlots 3
    pure ()
