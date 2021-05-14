{-# OPTIONS_GHC -fno-specialize #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-specialize #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fobject-code #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
-- | State transitions for Aave-like application
module Mlabs.Lending.Logic.React(
    react
) where

import qualified Prelude as Hask

import qualified PlutusTx.Ratio as R
import qualified PlutusTx.Numeric as N
import PlutusTx.Prelude
import qualified PlutusTx.AssocMap as M

import Control.Monad.Except
import Control.Monad.State.Strict

import Mlabs.Lending.Logic.Emulator.Blockchain
import Mlabs.Lending.Logic.InterestRate (addDeposit)
import Mlabs.Lending.Logic.State
import Mlabs.Lending.Logic.Types

{-# INLINABLE react #-}
-- | State transitions for lending pool.
-- For a given action we update internal state of Lending pool and produce
-- list of responses to simulate change of the balances on blockchain.
react :: Act -> St [Resp]
react input = do
  checkInput input
  case input of
    UserAct t uid act -> userAct t uid act
    PriceAct      act -> priceAct act
    GovernAct     act -> governAct act
  where
    -- | User acts
    userAct time uid = \case
      DepositAct{..}                    -> depositAct time uid act'amount act'asset
      BorrowAct{..}                     -> borrowAct time uid act'asset act'amount act'rate
      RepayAct{..}                      -> repayAct time uid act'asset act'amount act'rate
      SwapBorrowRateModelAct{..}        -> swapBorrowRateModelAct uid act'asset act'rate
      SetUserReserveAsCollateralAct{..} -> setUserReserveAsCollateralAct uid act'asset act'useAsCollateral (min act'portion (R.fromInteger 1))
      WithdrawAct{..}                   -> withdrawAct time uid act'amount act'asset
      FlashLoanAct                      -> flashLoanAct uid
      LiquidationCallAct{..}            -> liquidationCallAct uid act'collateral act'debt act'user act'debtToCover act'receiveAToken

    ---------------------------------------------------
    -- deposit

    depositAct currentTime uid amount asset = do
      ni <- getNormalisedIncome asset
      modifyWalletAndReserve' uid asset (addDeposit ni amount)
      aCoin <- aToken asset
      updateReserveState currentTime asset
      pure $ mconcat
        [ [Mint aCoin amount]
        , moveFromTo Self uid aCoin amount
        , moveFromTo uid Self asset          amount
        ]

    ---------------------------------------------------
    -- borrow

    -- TODO: ignores rate strategy (stable vs variable), ratio of liquidity to borrowed totals, health-check
    -- For borrowing to be valid we check that
    --  * reserve has enough liquidity
    --  * user does not use collateral reserve to borrow (it's meaningless for the user)
    --  * user has enough collateral for the borrow
    borrowAct currentTime uid asset amount _rate = do
      hasEnoughLiquidityToBorrow asset amount
      collateralNonBorrow uid asset
      hasEnoughCollateral uid asset amount
      updateOnBorrow
      updateReserveState currentTime asset
      pure $ moveFromTo Self uid asset amount
      where
        updateOnBorrow = do
          ni <- getNormalisedIncome asset
          modifyWallet uid asset $ \w -> w { wallet'borrow  = wallet'borrow w + amount }
          modifyReserveWallet' asset $ addDeposit ni (negate amount)

    hasEnoughLiquidityToBorrow asset amount = do
      liquidity <- getsReserve asset (wallet'deposit . reserve'wallet)
      guardError "Not enough liquidity for asset" (liquidity >= amount)

    collateralNonBorrow uid asset = do
      col <- getsWallet uid asset wallet'collateral
      guardError "Collateral can not be used as borrow for user"
        (col == 0)

    hasEnoughCollateral uid asset amount = do
      bor <- toAda asset amount
      isOk <- getHealthCheck bor asset =<< getUser uid
      guardError msg isOk
      where
        msg = "Not enough collateral to borrow"

    ---------------------------------------------------
    -- repay (also called redeem in whitepaper)

    repayAct currentTime uid asset amount _rate = do
      ni <- getNormalisedIncome asset
      bor <- getsWallet uid asset wallet'borrow
      let newBor = bor - amount
      if newBor >= 0
        then modifyWallet uid asset $ \w -> w { wallet'borrow = newBor }
        else modifyWallet' uid asset $ \w -> do
                w1 <- addDeposit ni (negate newBor) w
                pure $ w1 { wallet'borrow = 0 }
      modifyReserveWallet' asset $ addDeposit ni amount
      updateReserveState currentTime asset
      pure $ moveFromTo uid Self asset amount

    ---------------------------------------------------
    -- swap borrow model

    swapBorrowRateModelAct _ _ _ = todo

    ---------------------------------------------------
    -- set user reserve as collateral

    setUserReserveAsCollateralAct uid asset useAsCollateral portion
      | useAsCollateral = setAsCollateral uid asset portion
      | otherwise       = setAsDeposit    uid asset portion

    setAsCollateral uid asset portion
      | portion <= R.fromInteger 0 || portion > R.fromInteger 1 = pure []
      | otherwise                  = do
          ni <- getNormalisedIncome asset
          amount <- getAmountBy wallet'deposit uid asset portion
          modifyWalletAndReserve' uid asset $ \w -> do
            w1 <- addDeposit ni (negate amount) w
            pure $ w1 { wallet'collateral = wallet'collateral w + amount }
          aCoin <- aToken asset
          pure $ moveFromTo uid Self aCoin amount

    setAsDeposit uid asset portion
      | portion <= R.fromInteger 0 = pure []
      | otherwise                  = do
          amount <- getAmountBy wallet'collateral uid asset portion
          ni <- getNormalisedIncome asset
          modifyWalletAndReserve' uid asset $ \w -> do
            w1 <- addDeposit ni amount w
            pure $ w1 { wallet'collateral = wallet'collateral w - amount }
          aCoin <- aToken asset
          pure $ moveFromTo Self uid aCoin amount

    getAmountBy extract uid asset portion = do
      val <- getsWallet uid asset extract
      pure $ R.round $ portion N.* R.fromInteger val

    ---------------------------------------------------
    -- withdraw

    withdrawAct currentTime uid amount asset = do
      -- validate withdraw
      hasEnoughDepositToWithdraw uid amount asset
      -- update state on withdraw
      ni <- getNormalisedIncome asset
      modifyWalletAndReserve' uid asset $ addDeposit ni (negate amount)
      aCoin <- aToken asset
      updateReserveState currentTime asset
      pure $ mconcat
        [ moveFromTo Self uid asset amount
        , moveFromTo uid Self aCoin amount
        , Hask.pure $ Burn aCoin amount
        ]

    hasEnoughDepositToWithdraw uid amount asset = do
      dep <- getCumulativeBalance uid asset
      guardError "Not enough deposit to withdraw" (dep >= R.fromInteger amount)

    ---------------------------------------------------
    -- flash loan

    flashLoanAct _ = todo

    ---------------------------------------------------
    -- liquidation call

    liquidationCallAct _ _ _ _ _ _ = todo

    ---------------------------------------------------
    priceAct = \case
      SetAssetPrice coin rate -> setAssetPrice coin rate
      SetOracleAddr coin addr -> setOracleAddr coin addr

    ---------------------------------------------------
    -- update on market price change
    setAssetPrice _ _ = todo

    ---------------------------------------------------
    -- set oracle address
    --
    setOracleAddr _ _ = todo

    ---------------------------------------------------
    -- Govern acts

    governAct = \case
      AddReserve cfg -> addReserve cfg

    ---------------------------------------------------
    -- Adds new reserve (new coin/asset)

    addReserve cfg@CoinCfg{..} = do
      LendingPool reserves users curSym coinMap <- get
      if M.member coinCfg'coin reserves
        then throwError "Reserve is already present"
        else do
          let newReserves = M.insert coinCfg'coin (initReserve cfg) reserves
              newCoinMap  = M.insert coinCfg'aToken coinCfg'coin coinMap
          put $ LendingPool newReserves users curSym newCoinMap
          return []

    todo = return []

{-# INLINABLE checkInput #-}
-- | Check if input is valid
checkInput :: Act -> St ()
checkInput = \case
  UserAct time _uid act -> do
    isNonNegative "timestamp" time
    checkUserAct act
  PriceAct act  -> checkPriceAct act
  GovernAct act -> checkGovernAct act
  where
    checkUserAct = \case
      DepositAct amount asset -> do
        isPositive "deposit" amount
        isAsset asset
      BorrowAct asset amount _rate -> do
        isPositive "borrow" amount
        isAsset asset
      RepayAct asset amount _rate -> do
        isPositive "repay" amount
        isAsset asset
      SwapBorrowRateModelAct asset _rate -> isAsset asset
      SetUserReserveAsCollateralAct asset _useAsCollateral portion -> do
        isAsset asset
        isUnitRange "deposit portion" portion
      WithdrawAct amount asset -> do
        isPositive "withdraw" amount
        isAsset asset
      FlashLoanAct -> pure ()
      LiquidationCallAct _collateral _debt _user debtToCover _receiveAToken ->
        isPositive "Debt to cover" debtToCover

    checkPriceAct = \case
      SetAssetPrice asset price -> do
        isPositiveRational "price" price
        isAsset asset
      SetOracleAddr asset _uid ->
        isAsset asset

    checkGovernAct = \case
      AddReserve cfg -> checkCoinCfg cfg

    checkCoinCfg CoinCfg{..} = do
      isPositiveRational "coin price" coinCfg'rate
      checkInterestModel coinCfg'interestModel

    checkInterestModel InterestModel{..} = do
      isUnitRange "optimal utilisation" im'optimalUtilisation
      isPositiveRational "slope 1" im'slope1
      isPositiveRational "slope 2" im'slope2

