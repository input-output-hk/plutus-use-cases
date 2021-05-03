-- | State transitions for Lending app
module Mlabs.Lending.Logic.State(
    react
  , Error
) where

import Prelude

import Control.Monad.State.Strict

import Data.Text
import Mlabs.Lending.Logic.Types

type Error = Text

type St = StateT LendingPool (Either Error)

react :: Act -> St ()
react = \case
  LpAct     act -> lpAct act
  PriceAct  act -> priceAct act
  GovernAct act -> governAct act
  where
    lpAct = \case
      DepositAct{..}                    -> depositAct act'amount act'asset act'onBehalfOf
      BorrowAct{..}                     -> borrowAct  act'asset act'amount act'rate act'onBehalfOf
      RepayAct{..}                      -> repayAct   act'asset act'amount act'rate act'onBehalfOf
      SwapBorrowRateModelAct{..}        -> swapBorrowRateModelAct act'asset act'rate
      SetUserReserveAsCollateralAct{..} -> setUserReserveAsCollateralAct act'asset act'useAsCollateral
      WithdrawAct{..}                   -> withdrawAct act'to act'amount act'asset
      FlashLoanAct                      -> flashLoanAct
      LiquidationCallAct{..}            -> liquidationCallAct act'collateral act'debt act'user act'debtToCover act'receiveAToken

    depositAct _ _ _ = todo
    borrowAct _ _ _ _ = todo
    repayAct _ _ _ _ = todo
    swapBorrowRateModelAct _ _ = todo
    setUserReserveAsCollateralAct _ _ = todo
    withdrawAct _ _ _ = todo
    flashLoanAct = todo
    liquidationCallAct _ _ _ _ _ = todo

    priceAct = \case
      SetAssetPrice coin rate -> setAssetPrice coin rate
      SetOracleAddr coin addr -> setOracleAddr coin addr

    setAssetPrice _ _ = todo
    setOracleAddr _ _ = todo

    governAct = \case
      AddReserve coin val -> addReserve coin val

    addReserve _ _ = todo

    todo = return ()

