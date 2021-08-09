-- | Client functions to test contracts in EmulatorTrace monad.
module Mlabs.Lending.Contract.Emulator.Client (
  callUserAct,
  callPriceAct,
  callGovernAct,
  callStartLendex,
  queryAllLendexes,
) where

import Prelude

import Data.Functor (void)
import Data.Semigroup (Last (..))
import Plutus.Trace.Emulator (EmulatorRuntimeError (..), EmulatorTrace, activateContractWallet, callEndpoint, observableState, throwError)
import Plutus.V1.Ledger.Tx
import Wallet.Emulator qualified as Emulator

import Mlabs.Lending.Contract.Api qualified as Api
import Mlabs.Lending.Contract.Server (adminEndpoints, oracleEndpoints, queryEndpoints, userEndpoints)
import Mlabs.Lending.Logic.Types qualified as Types
import Mlabs.Plutus.Contract (callEndpoint')

---------------------------------------------------------
-- call endpoints (for debug and testing)

-- | Calls user act
callUserAct :: Types.LendexId -> Emulator.Wallet -> Types.UserAct -> EmulatorTrace ()
callUserAct lid wal act = do
  hdl <- activateContractWallet wal (userEndpoints lid)
  void $ case act of
    Types.DepositAct {..} -> callEndpoint' hdl $ Api.Deposit act'amount act'asset
    Types.BorrowAct {..} -> callEndpoint' hdl $ Api.Borrow act'amount act'asset (Api.toInterestRateFlag act'rate)
    Types.RepayAct {..} -> callEndpoint' hdl $ Api.Repay act'amount act'asset (Api.toInterestRateFlag act'rate)
    Types.SwapBorrowRateModelAct {..} -> callEndpoint' hdl $ Api.SwapBorrowRateModel act'asset (Api.toInterestRateFlag act'rate)
    Types.AddCollateralAct {..} -> callEndpoint' hdl $ Api.AddCollateral add'asset add'amount
    Types.RemoveCollateralAct {..} -> callEndpoint' hdl $ Api.RemoveCollateral remove'asset remove'amount
    Types.WithdrawAct {..} -> callEndpoint' hdl $ Api.Withdraw act'amount act'asset
    Types.FlashLoanAct -> pure ()
    Types.LiquidationCallAct {..} ->
      case act'debt of
        Types.BadBorrow (Types.UserId pkh) asset -> callEndpoint' hdl $ Api.LiquidationCall act'collateral pkh asset act'debtToCover act'receiveAToken
        _ -> throwError $ GenericError "Bad borrow has wrong settings"

-- | Calls price oracle act
callPriceAct :: Types.LendexId -> Emulator.Wallet -> Types.PriceAct -> EmulatorTrace ()
callPriceAct lid wal act = do
  hdl <- activateContractWallet wal (oracleEndpoints lid)
  void $ case act of
    Types.SetAssetPriceAct coin rate -> callEndpoint @"set-asset-price" hdl $ Api.SetAssetPrice coin rate

-- | Calls govern act
callGovernAct :: Types.LendexId -> Emulator.Wallet -> Types.GovernAct -> EmulatorTrace ()
callGovernAct lid wal act = do
  hdl <- activateContractWallet wal (adminEndpoints lid)
  void $ case act of
    Types.AddReserveAct cfg -> callEndpoint @"add-reserve" hdl $ Api.AddReserve cfg

-- | Calls initialisation of state for Lending pool
callStartLendex :: Types.LendexId -> Emulator.Wallet -> Api.StartLendex -> EmulatorTrace ()
callStartLendex lid wal sl = do
  hdl <- activateContractWallet wal (adminEndpoints lid)
  void $ callEndpoint @"start-lendex" hdl sl

-- todo: make a better query dispatch if the number of queries grows

-- | Queries for all Lendexes started  with given StartParams
queryAllLendexes :: Types.LendexId -> Emulator.Wallet -> Api.QueryAllLendexes -> EmulatorTrace [(Address, Types.LendingPool)]
queryAllLendexes lid wal spm = do
  hdl <- activateContractWallet wal (queryEndpoints lid)
  void $ callEndpoint @"query-all-lendexes" hdl spm
  ls' <- observableState hdl
  let Just (Last (Types.QueryResAllLendexes ls)) = ls'
  pure ls
