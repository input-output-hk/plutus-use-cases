-- | Client functions to test contracts in EmulatorTrace monad.
module Mlabs.Governance.Contract.Emulator.Client where

import Control.Monad (void)
import PlutusTx.Prelude hiding (Semigroup(..), unless)
import Plutus.Trace.Emulator (EmulatorTrace, activateContractWallet)
import Wallet.Emulator qualified as Emulator

import Mlabs.Plutus.Contract (callEndpoint')
import Mlabs.Governance.Contract.Api qualified as Api
import Mlabs.Governance.Contract.Server qualified as Server
import Mlabs.Governance.Contract.Validation (AssetClassNft(..), AssetClassGov(..))

startGovernance :: Emulator.Wallet -> Api.StartGovernance -> EmulatorTrace ()
startGovernance wal startGov@Api.StartGovernance{..} = do
  hdl <- activateContractWallet wal (Server.governanceEndpoints sgNft sgGov)
  void $ callEndpoint' @Api.StartGovernance hdl startGov

-- imo it would be nicer if we were to take the type to be applied to callEndpoint' from the type sig itself
-- | Deposits the specified amount of GOV into the governance contract
callDeposit :: AssetClassNft -> AssetClassGov -> Emulator.Wallet -> Api.Deposit -> EmulatorTrace ()
callDeposit nft gov wal depo = do
  hdl <- activateContractWallet wal (Server.governanceEndpoints nft gov)
  void $ callEndpoint' @Api.Deposit hdl depo

-- | Withdraws the specified amount of GOV from the governance contract
callWithdraw :: AssetClassNft -> AssetClassGov -> Emulator.Wallet -> Api.Withdraw -> EmulatorTrace ()
callWithdraw nft gov wal withd = do
  hdl <- activateContractWallet wal (Server.governanceEndpoints nft gov)
  void $ callEndpoint' @Api.Withdraw hdl withd

-- | Distributes the given Value amongst the xGOV holders
callProvideRewards :: AssetClassNft -> AssetClassGov -> Emulator.Wallet -> Api.ProvideRewards -> EmulatorTrace ()
callProvideRewards nft gov wal withd = do
  hdl <- activateContractWallet wal (Server.governanceEndpoints nft gov)
  void $ callEndpoint' @Api.ProvideRewards hdl withd

-- | Queries the balance of a given PubKeyHash
queryBalance :: AssetClassNft -> AssetClassGov -> Emulator.Wallet -> Api.QueryBalance -> EmulatorTrace ()
queryBalance nft gov wal qb = do
  hdl <- activateContractWallet wal (Server.governanceEndpoints nft gov)
  void $ callEndpoint' @Api.QueryBalance hdl qb
