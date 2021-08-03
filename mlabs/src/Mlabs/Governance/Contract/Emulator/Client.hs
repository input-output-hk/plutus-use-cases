-- | Client functions to test contracts in EmulatorTrace monad.
module Mlabs.Governance.Contract.Emulator.Client where

import Control.Monad (void)
import PlutusTx.Prelude hiding (Semigroup(..), unless)
import Plutus.Trace.Emulator (EmulatorTrace, activateContractWallet)
import Ledger hiding (singleton)
import Wallet.Emulator qualified as Emulator

import Mlabs.Plutus.Contract (callEndpoint')
import Mlabs.Governance.Contract.Api qualified as Api
import Mlabs.Governance.Contract.Server qualified as Server

-- imo it would be nicer if we were to take the type to be applied to callEndpoint' from the type sig itself
-- | Deposits the specified amount of GOV into the governance contract
callDeposit :: CurrencySymbol -> Emulator.Wallet -> Api.Deposit -> EmulatorTrace ()
callDeposit csym wal depo = do
  hdl <- activateContractWallet wal (Server.governanceEndpoints csym)
  void $ callEndpoint' @Api.Deposit hdl depo

-- | Withdraws the specified amount of GOV from the governance contract
callWithdraw :: CurrencySymbol -> Emulator.Wallet -> Api.Withdraw -> EmulatorTrace ()
callWithdraw csym wal withd = do
  hdl <- activateContractWallet wal (Server.governanceEndpoints csym)
  void $ callEndpoint' @Api.Withdraw hdl withd

-- | Distributes the given Value amongst the xGOV holders
callProvideRewards :: CurrencySymbol -> Emulator.Wallet -> Api.ProvideRewards -> EmulatorTrace ()
callProvideRewards csym wal withd = do
  hdl <- activateContractWallet wal (Server.governanceEndpoints csym)
  void $ callEndpoint' @Api.ProvideRewards hdl withd

-- | Queries the balance of a given PubKeyHash
queryBalance :: CurrencySymbol -> Emulator.Wallet -> Api.QueryBalance -> EmulatorTrace ()
queryBalance csym wal qb = do
  hdl <- activateContractWallet wal (Server.governanceEndpoints csym)
  void $ callEndpoint' @Api.QueryBalance hdl qb
