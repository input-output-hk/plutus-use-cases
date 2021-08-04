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
callDeposit :: CurrencySymbol -> TokenName -> Emulator.Wallet -> Api.Deposit -> EmulatorTrace ()
callDeposit csym tn wal depo = do
  hdl <- activateContractWallet wal (Server.governanceEndpoints csym tn)
  void $ callEndpoint' @Api.Deposit hdl depo

-- | Withdraws the specified amount of GOV from the governance contract
callWithdraw :: CurrencySymbol -> TokenName -> Emulator.Wallet -> Api.Withdraw -> EmulatorTrace ()
callWithdraw csym tn wal withd = do
  hdl <- activateContractWallet wal (Server.governanceEndpoints csym tn)
  void $ callEndpoint' @Api.Withdraw hdl withd

-- | Distributes the given Value amongst the xGOV holders
callProvideRewards :: CurrencySymbol -> TokenName -> Emulator.Wallet -> Api.ProvideRewards -> EmulatorTrace ()
callProvideRewards csym tn wal withd = do
  hdl <- activateContractWallet wal (Server.governanceEndpoints csym tn)
  void $ callEndpoint' @Api.ProvideRewards hdl withd

-- | Queries the balance of a given PubKeyHash
queryBalance :: CurrencySymbol -> TokenName -> Emulator.Wallet -> Api.QueryBalance -> EmulatorTrace ()
queryBalance csym tn wal qb = do
  hdl <- activateContractWallet wal (Server.governanceEndpoints csym tn)
  void $ callEndpoint' @Api.QueryBalance hdl qb
