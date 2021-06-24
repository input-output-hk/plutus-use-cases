{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Spec.User where

import           Control.Lens               ((^?))
import           Control.Monad              (forM, void)
import qualified Data.Map                   as Map
import           Data.Monoid                (Last (..))
import           Data.Text                  (Text)
import           Data.Void                  (Void)
import qualified Ledger
import           Plutus.Contract            hiding (throwError)
import           Plutus.Contract.Test
import qualified Plutus.Contracts.AToken    as AToken
import qualified Plutus.Contracts.Core      as Aave
import           Plutus.Contracts.Endpoints (ContractResponse (..))
import qualified Plutus.Contracts.Endpoints as Aave
import           Plutus.PAB.Simulation      (initContract, toAsset)
import qualified Plutus.Trace.Emulator      as Trace
import           Plutus.V1.Ledger.Ada       (lovelaceValueOf)
import           Plutus.V1.Ledger.Value     (AssetClass, Value, assetClassValue)
import qualified PlutusTx.AssocMap          as AssocMap
import qualified PlutusTx.Prelude           as PlutusTx
import qualified Spec.Mock                  as Mock
import qualified Spec.Utils                 as TestUtils
import           Test.Tasty

testAssets :: [AssetClass]
testAssets = fmap toAsset ["MOGUS", "USD"]

mogus :: AssetClass
mogus = Prelude.head testAssets

amogus :: AssetClass
amogus = AToken.makeAToken Mock.aaveHash mogus

usd :: AssetClass
usd = testAssets Prelude.!! 1

startParams :: [Aave.CreateParams]
startParams = fmap Aave.CreateParams testAssets

startContract :: Contract () Aave.AaveOwnerSchema Text ()
startContract = void $ Mock.start startParams

userContract :: Contract (Last (ContractResponse Text Aave.UserContractState)) Aave.AaveUserSchema Void ()
userContract = void $ Aave.userEndpoints Mock.aave

ownerWallet :: Wallet
ownerWallet = Wallet 1

lenderWallet :: Wallet
lenderWallet = Wallet 2

borrowerWallet :: Wallet
borrowerWallet = Wallet 3

userWallets :: [Wallet]
userWallets = [lenderWallet, borrowerWallet]

initialFunds :: Value
initialFunds = lovelaceValueOf 1000000 <> mconcat ((`assetClassValue` 1000) <$> testAssets)

type UserHandle = TestUtils.ContractHandle Text Aave.UserContractState Aave.AaveUserSchema Void

startTrace :: Trace.EmulatorTrace (Map.Map Wallet UserHandle)
startTrace = do
    _ <- Trace.activateContractWallet ownerWallet startContract
    _ <- Trace.waitNSlots 5
    _ <- Trace.activateContractWallet ownerWallet $ initContract userWallets testAssets
    _ <- Trace.waitNSlots 5
    fmap Map.fromList $ forM userWallets $ \wallet -> do
        handle <- Trace.activateContractWallet wallet userContract
        pure (wallet, handle)

getPubKey :: UserHandle -> Trace.EmulatorTrace Ledger.PubKeyHash
getPubKey userHandle = do
    _ <- Trace.callEndpoint @"ownPubKey" userHandle ()
    _ <- Trace.waitNSlots 1
    TestUtils.getState (^? Aave._GetPubKey) userHandle

deposit :: UserHandle -> AssetClass -> Integer -> Trace.EmulatorTrace ()
deposit userHandle asset amount = do
    pkh <- getPubKey userHandle
    Trace.callEndpoint @"deposit" userHandle $ Aave.DepositParams asset pkh amount
    _ <- Trace.waitNSlots 3
    pure ()

withdraw :: UserHandle -> AssetClass -> Integer -> Trace.EmulatorTrace ()
withdraw userHandle asset amount = do
    pkh <- getPubKey userHandle
    Trace.callEndpoint @"withdraw" userHandle $ Aave.WithdrawParams asset pkh amount
    _ <- Trace.waitNSlots 3
    pure ()

initialReserves :: AssocMap.Map AssetClass Aave.Reserve
initialReserves = AssocMap.fromList (fmap (\params -> (Aave.cpAsset params, Aave.createReserve Mock.aave params)) startParams)

modifyAt :: PlutusTx.Eq k => (v -> v) -> k -> AssocMap.Map k v -> AssocMap.Map k v
modifyAt f k m = maybe m (\v -> AssocMap.insert k (f v) m) (AssocMap.lookup k m)

modifyAmount :: (Integer -> Integer) -> AssetClass -> AssocMap.Map AssetClass Aave.Reserve -> AssocMap.Map AssetClass Aave.Reserve
modifyAmount f = modifyAt (\r -> r { Aave.rAmount = f . Aave.rAmount $ r })

reservesChange :: AssocMap.Map AssetClass Aave.Reserve -> TracePredicate
reservesChange reserves = TestUtils.datumsAtAddress Mock.aaveAddress (TestUtils.one check)
    where
        check (Aave.ReservesDatum _ reserves') = reserves' == reserves
        check _                                = False

tests :: TestTree
tests = testGroup "deposit" [
    checkPredicate
        "Successful deposit"
        (walletFundsChange
            lenderWallet
            (initialFunds <>
            assetClassValue mogus (negate 100) <> assetClassValue amogus 100)
        .&&. reservesChange (modifyAmount (+100) mogus initialReserves)
        )
        $ do
            handles <- startTrace
            deposit (handles Map.! lenderWallet) mogus 100,
    checkPredicate
        "Successful withdraw"
        (walletFundsChange
            lenderWallet
            (initialFunds <>
            assetClassValue mogus (negate 100) <> assetClassValue amogus 100 <>
            assetClassValue mogus 50 <> assetClassValue amogus (negate 50))
        .&&. reservesChange (modifyAmount (subtract 50 . (+100)) mogus initialReserves)
        )
        $ do
            handles <- startTrace
            deposit (handles Map.! lenderWallet) mogus 100
            withdraw (handles Map.! lenderWallet) mogus 50
    ]
