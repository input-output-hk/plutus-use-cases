{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Spec.Start where

import           Control.Monad              (void)
import           Data.Text                  (Text)
import           Plutus.Contract
import           Plutus.Contract.Test
import qualified Plutus.Contracts.Core      as Aave
import qualified Plutus.Contracts.Endpoints as Aave
import           Plutus.PAB.Simulation      (testAssets)
import qualified Plutus.Trace.Emulator      as Trace
import qualified PlutusTx.AssocMap          as AssocMap
import qualified Spec.Mock                  as Mock
import qualified Spec.Utils                 as TestUtils
import           Test.Tasty

startParams :: [Aave.CreateParams]
startParams = fmap Aave.CreateParams testAssets

startContract :: Contract () Aave.AaveOwnerSchema Text ()
startContract = void $ Mock.start startParams

ownerWallet :: Wallet
ownerWallet = Wallet 1

startTrace :: Trace.EmulatorTrace ()
startTrace = do
  _ <- Trace.activateContractWallet ownerWallet startContract
  _ <- Trace.waitNSlots 5
  pure ()

initSuccess :: [Aave.AaveDatum] -> Bool
initSuccess = TestUtils.allSatisfy . fmap TestUtils.one $ [hasReserves, hasUsers, hasOperator]
    where
    hasOperator (Aave.LendingPoolDatum _) = True
    hasOperator _                         = False
    hasReserves (Aave.ReservesDatum _ reserves) =
        reserves == AssocMap.fromList (fmap (\params -> (Aave.cpAsset params, Aave.createReserve Mock.aave params)) startParams)
    hasReserves _ = False
    hasUsers (Aave.UserConfigsDatum _ users) = users == AssocMap.empty
    hasUsers _                               = False

tests :: TestTree
tests = testGroup "ownerContract" [checkPredicate
        "starts a new lending pool"
        (assertDone startContract (Trace.walletInstanceTag ownerWallet) (const True) "start contract not done"
        .&&. TestUtils.datumsAtAddress Mock.aaveAddress initSuccess)
        startTrace]
