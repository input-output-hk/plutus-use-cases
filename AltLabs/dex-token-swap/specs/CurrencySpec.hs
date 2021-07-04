{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module CurrencySpec
  (
    tests,
    traceMintCurrency
  ) where

import           Control.Monad             (void)
import qualified Ledger
import           Plutus.Contract
import           Plutus.Contract.Test

import Plutus.Contracts.Currency ( forgeContract, CurrencyError, OneShotCurrency )
import qualified Plutus.Trace.Emulator     as Trace

import           Test.Tasty

wallet :: Wallet
wallet = Wallet 1

mintingContract :: Contract () EmptySchema CurrencyError OneShotCurrency
mintingContract =
  forgeContract pkHash amounts
  where
    amounts = [("CUR_X", 100), ("CUR_Y", 1000), ("CUR_Z", 4000)]
    pkHash = Ledger.pubKeyHash . walletPubKey $ wallet

traceMintCurrency :: Trace.EmulatorTrace ()
traceMintCurrency = do
  _ <- Trace.activateContractWallet wallet $ void mintingContract
  _ <- Trace.nextSlot
  void $ Trace.nextSlot

tests :: TestTree
tests = testGroup "Currency" [
    checkPredicate "should create new currency" pred1 traceMintCurrency
  ]
  where
    pred1 = assertDone mintingContract
                       (Trace.walletInstanceTag wallet) (const True)
                       "currency contract not done"