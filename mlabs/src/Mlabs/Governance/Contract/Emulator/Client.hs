-- | Client functions to test contracts in EmulatorTrace monad.
module Mlabs.Governance.Contract.Emulator.Client where

import Control.Monad (void)
import Data.Coerce (coerce)
import PlutusTx.Prelude hiding (Semigroup(..), unless)
import Plutus.Trace.Emulator (EmulatorTrace, activateContractWallet)
import Wallet.Emulator qualified as Emulator

import Mlabs.Plutus.Contract (callEndpoint')
import Mlabs.Governance.Contract.Api qualified as Api
import Mlabs.Governance.Contract.Server qualified as Server
import Mlabs.Governance.Contract.Validation (GovParams(..))

startGovernance :: Emulator.Wallet -> Api.StartGovernance -> EmulatorTrace ()
startGovernance wal startGov = do
  hdl <- activateContractWallet wal (Server.governanceEndpoints $ coerce startGov)
  void $ callEndpoint' @Api.StartGovernance hdl startGov

-- imo it would be nicer if we were to take the type to be applied to callEndpoint' from the type sig itself
-- | Deposits the specified amount of GOV into the governance contract
callDeposit :: GovParams -> Emulator.Wallet -> Api.Deposit -> EmulatorTrace ()
callDeposit params wal depo = do
  hdl <- activateContractWallet wal (Server.governanceEndpoints params)
  void $ callEndpoint' @Api.Deposit hdl depo

-- | Withdraws the specified amount of GOV from the governance contract
callWithdraw :: GovParams -> Emulator.Wallet -> Api.Withdraw -> EmulatorTrace ()
callWithdraw params wal withd = do
  hdl <- activateContractWallet wal (Server.governanceEndpoints params)
  void $ callEndpoint' @Api.Withdraw hdl withd

-- | Distributes the given Value amongst the xGOV holders
callProvideRewards :: GovParams -> Emulator.Wallet -> Api.ProvideRewards -> EmulatorTrace ()
callProvideRewards params wal withd = do
  hdl <- activateContractWallet wal (Server.governanceEndpoints params)
  void $ callEndpoint' @Api.ProvideRewards hdl withd

-- | Queries the balance of a given PubKeyHash
queryBalance :: GovParams -> Emulator.Wallet -> Api.QueryBalance -> EmulatorTrace ()
queryBalance params wal qb = do
  hdl <- activateContractWallet wal (Server.governanceEndpoints params)
  void $ callEndpoint' @Api.QueryBalance hdl qb
