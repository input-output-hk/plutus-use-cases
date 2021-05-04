-- | State transitions for Aave-like application
module Mlabs.Lending.Logic.React(
  react
) where

import Control.Monad.Except
import Control.Monad.State.Strict

import qualified Data.Map.Strict as M

import Mlabs.Lending.Logic.Emulator
import Mlabs.Lending.Logic.State
import Mlabs.Lending.Logic.Types

-- | State transition for lending pool.
-- For a given action we update internal state of Lending pool and produce
-- list of responses to simulate change of the balances
react :: Act -> St [Resp]
react = \case
  UserAct uid act -> userAct uid act
  PriceAct    act -> priceAct act
  GovernAct   act -> governAct act
  where
    userAct uid = \case
      DepositAct{..}                    -> depositAct uid act'amount act'asset
      BorrowAct{..}                     -> borrowAct  uid act'asset act'amount act'rate
      RepayAct{..}                      -> repayAct   uid act'asset act'amount act'rate
      SwapBorrowRateModelAct{..}        -> swapBorrowRateModelAct uid act'asset act'rate
      SetUserReserveAsCollateralAct{..} -> setUserReserveAsCollateralAct uid act'asset act'useAsCollateral (min act'portion 1)
      WithdrawAct{..}                   -> withdrawAct uid act'amount act'asset
      FlashLoanAct                      -> flashLoanAct uid
      LiquidationCallAct{..}            -> liquidationCallAct uid act'collateral act'debt act'user act'debtToCover act'receiveAToken

    -- TODO: ignores ratio of liquidity to borrowed totals
    depositAct uid amount asset = do
      modifyWalletAndReserve uid asset (Right . depositUser)
      let move a b = MoveTo $ Move uid a b
      pure
        [ move asset (negate amount)
        , move (aToken asset) amount
        ]
      where
        depositUser    w@Wallet{..}  = w { wallet'deposit  = amount + wallet'deposit }

    -- TODO: ignores rate strategy (stable vs variable), ratio of liquidity to borrowed totals, health-check
    -- For borrowing to be valid we check that
    --  * reserve has enough liquidity
    --  * user does not use collateral reserve to borrow (it's meaningless for the user)
    --  * user has enough collateral for the borrow
    borrowAct uid asset amount _rate = do
      hasEnoughLiquidity asset amount
      collateralNonBorrow uid asset
      hasEnoughCollateral uid asset amount
      updateOnBorrow
      pure [ MoveTo $ Move uid asset amount  ]
      where
        updateOnBorrow = modifyWalletAndReserve uid asset $ \w -> Right $ w
          { wallet'deposit = wallet'deposit w - amount
          , wallet'borrow  = wallet'borrow  w + amount
          }

    hasEnoughLiquidity asset amount = do
      liquidity <- getsReserve asset (wallet'deposit . reserve'wallet)
      guardError ("Not enough liquidity for asset " <> showt asset)
        (liquidity >= amount)

    collateralNonBorrow uid asset = do
      col <- getsWallet uid asset wallet'collateral
      guardError ("Collateral can not be used as borrow for user " <> showt uid <> " for asset " <> showt asset)
        (col == 0)

    hasEnoughCollateral uid asset amount = do
      bor <- toAda asset amount
      isOk <- getHealthCheck bor asset =<< getUser uid
      guardError msg isOk
      where
        msg = mconcat [ "Not enough collateral to borrow ", showt amount, " ", showt asset, " for user ", showt uid]


    repayAct _ _ _ _ = todo
    swapBorrowRateModelAct _ _ _ = todo

    setUserReserveAsCollateralAct uid asset useAsCollateral portion
      | useAsCollateral = setAsCollateral uid asset portion
      | otherwise       = setAsDeposit    uid asset portion

    setAsCollateral uid asset portion
      | portion <= 0 = pure []
      | otherwise    = do
          amount <- getAmountBy wallet'deposit uid asset portion
          modifyWalletAndReserve uid asset $ \w -> Right $ w
            { wallet'deposit    = wallet'deposit w    - amount
            , wallet'collateral = wallet'collateral w + amount
            }
          pure [ MoveTo $ Move uid (aToken asset) (negate amount) ]

    setAsDeposit uid asset portion
      | portion <= 0 = pure []
      | otherwise    = do
          amount <- getAmountBy wallet'collateral uid asset portion
          modifyWalletAndReserve uid asset $ \w -> Right $ w
            { wallet'deposit    = wallet'deposit w    + amount
            , wallet'collateral = wallet'collateral w - amount
            }
          pure [ MoveTo $ Move uid (aToken asset) amount ]

    getAmountBy extract uid asset portion = do
      val <- getsWallet uid asset extract
      pure $ floor $ portion * fromInteger val


    withdrawAct _ _ _ = todo
    flashLoanAct _ = todo
    liquidationCallAct _ _ _ _ _ _ = todo

    priceAct = \case
      SetAssetPrice coin rate -> setAssetPrice coin rate
      SetOracleAddr coin addr -> setOracleAddr coin addr

    setAssetPrice _ _ = todo
    setOracleAddr _ _ = todo

    governAct = \case
      AddReserve coin val -> addReserve coin val

    addReserve coin val = do
      LendingPool reserves users <- get
      if M.member coin reserves
        then throwError "Reserve is already present"
        else do
          put $ LendingPool (M.insert coin (initReserve val) reserves) users
          return []

    todo = return []

