{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}

module Spec.Test
    ( tests
    ) where

import           Control.Lens
import           Control.Monad                      (void, when)
import qualified Control.Monad.Freer                as Freer
import qualified Control.Monad.Freer.Error          as Freer
import           Control.Monad.Freer.Extras as Extras
import           Control.Monad.Freer.Extras.Log     (LogLevel (..))
import           Data.Default                       (Default (def))
import           Data.Monoid                        (Last (..))
import           Data.Text                          (Text, pack)
import           Ledger                             (Ada, Slot (..), Value, pubKeyHash)
import qualified Ledger.Ada                         as Ada
import           Ledger.Oracle             (Observation, SignedMessage, signMessage)
import           Plutus.Contract                    hiding (currentSlot)
import           Plutus.Contract.Test               hiding (not)
import qualified Streaming.Prelude                  as S
import qualified Wallet.Emulator.Folds              as Folds
import qualified Wallet.Emulator.Stream             as Stream

import           Ledger.TimeSlot                    (SlotConfig)
import qualified Ledger.TimeSlot                    as TimeSlot
import qualified Ledger.Value                       as Value
import           Ledger.Value                       (CurrencySymbol)
import           Plutus.Contract.Test.ContractModel
import           Contracts.Test                   
import qualified Plutus.Trace.Emulator              as Trace
import           PlutusTx.Monoid                    (inv)

import           Test.Tasty

auctionEmulatorCfg :: Trace.EmulatorConfig
auctionEmulatorCfg =
    let initialDistribution = defaultDist
    in (def & Trace.initialChainState .~ Left initialDistribution) & Trace.slotConfig .~ slotCfg

options :: CheckOptions
options = set emulatorConfig auctionEmulatorCfg defaultCheckOptions

slotCfg :: SlotConfig
slotCfg = def

testData ::  TestData
testData = TestData
    { testFee = 1000000 }

  
testContract ::  Contract () EmptySchema Text ()
testContract = runTest testData

w1:: Wallet
w1 = Wallet 1


requestOracleTrace :: Trace.EmulatorTrace ()
requestOracleTrace = do
    oracleHdl <- Trace.activateContractWallet w1 $ testContract
    void $ Trace.waitNSlots 3

tests :: TestTree
tests =
    testGroup "test"
        [ checkPredicateOptions options "test"
            (assertNoFailedTransactions
            .&&. valueAtAddress (address testData) (== (Ada.toValue . Ada.lovelaceOf $ 1))
            )
            requestOracleTrace
        ]
