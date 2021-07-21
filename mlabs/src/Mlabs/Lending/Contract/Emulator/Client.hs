-- | Client functions to test contracts in EmulatorTrace monad.
module Mlabs.Lending.Contract.Emulator.Client(
    callUserAct
  , callPriceAct
  , callGovernAct
  , callStartLendex
) where

import Prelude

import Data.Functor (void)
import Plutus.Trace.Emulator (EmulatorTrace, throwError, callEndpoint, activateContractWallet, EmulatorRuntimeError(..))
import Wallet.Emulator qualified as Emulator

import Mlabs.Lending.Contract.Api qualified as Api
import Mlabs.Lending.Contract.Server (adminEndpoints, oracleEndpoints, userEndpoints)
import Mlabs.Lending.Logic.Types qualified as Types
import Mlabs.Plutus.Contract (callEndpoint')

---------------------------------------------------------
-- call endpoints (for debug and testing)

-- | Calls user act
callUserAct :: Types.LendexId -> Emulator.Wallet -> Types.UserAct -> EmulatorTrace ()
callUserAct lid wal act = do
  hdl <- activateContractWallet wal (userEndpoints lid)
  void $ case act of
    Types.DepositAct{..}                    -> callEndpoint' hdl $ Api.Deposit act'amount act'asset
    Types.BorrowAct{..}                     -> callEndpoint' hdl $ Api.Borrow  act'amount act'asset (Api.toInterestRateFlag act'rate)
    Types.RepayAct{..}                      -> callEndpoint' hdl $ Api.Repay   act'amount act'asset (Api.toInterestRateFlag act'rate)
    Types.SwapBorrowRateModelAct{..}        -> callEndpoint' hdl $ Api.SwapBorrowRateModel act'asset (Api.toInterestRateFlag act'rate)
    Types.SetUserReserveAsCollateralAct{..} -> callEndpoint' hdl $ Api.SetUserReserveAsCollateral act'asset act'useAsCollateral act'portion
    Types.WithdrawAct{..}                   -> callEndpoint' hdl $ Api.Withdraw act'amount act'asset
    Types.FlashLoanAct                      -> pure ()
    Types.LiquidationCallAct{..}            ->
      case act'debt of
        Types.BadBorrow (Types.UserId pkh) asset  -> callEndpoint' hdl $ Api.LiquidationCall act'collateral pkh asset act'debtToCover act'receiveAToken
        _                                         -> throwError $ GenericError "Bad borrow has wrong settings"

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
    Types.AddReserveAct cfg       -> callEndpoint @"add-reserve" hdl $ Api.AddReserve cfg
    Types.QueryAllLendexesAct spm -> callEndpoint @"query-all-lendexes" hdl $ Api.QueryAllLendexes spm

-- | Calls initialisation of state for Lending pool
callStartLendex :: Types.LendexId -> Emulator.Wallet -> Api.StartLendex -> EmulatorTrace ()
callStartLendex lid wal sl = do
  hdl <- activateContractWallet wal (adminEndpoints lid)
  void $ callEndpoint @"start-lendex" hdl sl

