-- | Calculate interest rate parameters
module Mlabs.Lending.Logic.InterestRate (
  updateReserveInterestRates,
  getLiquidityRate,
  getNormalisedIncome,
  getCumulatedLiquidityIndex,
  addDeposit,
  getCumulativeBalance,
) where

import PlutusTx.Prelude
-- import Prelude qualified as Hask (String)

import Mlabs.Lending.Logic.Types (Reserve (..), ReserveInterest (..), Wallet (..))
import Mlabs.Lending.Logic.Types qualified as Types
import PlutusTx.Ratio qualified as R

{-# INLINEABLE updateReserveInterestRates #-}
updateReserveInterestRates :: Integer -> Types.Reserve -> Types.Reserve
updateReserveInterestRates currentTime reserve = reserve {reserve'interest = nextInterest reserve}
  where
    nextInterest Types.Reserve {..} =
      reserve'interest
        { ri'liquidityRate = liquidityRate
        , ri'liquidityIndex = getCumulatedLiquidityIndex liquidityRate yearDelta $ reserve'interest.ri'liquidityIndex
        , ri'normalisedIncome = getNormalisedIncome liquidityRate yearDelta $ reserve'interest.ri'liquidityIndex
        , ri'lastUpdateTime = currentTime
        }
      where
        yearDelta = getYearDelta lastUpdateTime currentTime
        liquidityRate = getLiquidityRate reserve
        lastUpdateTime = reserve'interest.ri'lastUpdateTime

{-# INLINEABLE getYearDelta #-}
getYearDelta :: Integer -> Integer -> Rational
getYearDelta t0 t1 = R.fromInteger (max 0 $ t1 - t0) * secondsPerSlot * R.recip secondsPerYear
  where
    secondsPerSlot = R.fromInteger 1
    secondsPerYear = R.fromInteger 31622400

{-# INLINEABLE getCumulatedLiquidityIndex #-}
getCumulatedLiquidityIndex :: Rational -> Rational -> Rational -> Rational
getCumulatedLiquidityIndex liquidityRate yearDelta prevLiquidityIndex =
  (liquidityRate * yearDelta + R.fromInteger 1) * prevLiquidityIndex

{-# INLINEABLE getNormalisedIncome #-}
getNormalisedIncome :: Rational -> Rational -> Rational -> Rational
getNormalisedIncome liquidityRate yearDelta prevLiquidityIndex =
  (liquidityRate * yearDelta + R.fromInteger 1) * prevLiquidityIndex

{-# INLINEABLE getLiquidityRate #-}
getLiquidityRate :: Types.Reserve -> Rational
getLiquidityRate Types.Reserve {..} = r * u
  where
    u = getUtilisation reserve'wallet
    r = getBorrowRate (ri'interestModel reserve'interest) u

{-# INLINEABLE getUtilisation #-}
getUtilisation :: Types.Wallet -> Rational
getUtilisation Types.Wallet {..} = wallet'borrow R.% liquidity
  where
    liquidity = wallet'deposit + wallet'borrow

{-# INLINEABLE getBorrowRate #-}
getBorrowRate :: Types.InterestModel -> Rational -> Rational
getBorrowRate Types.InterestModel {..} u
  | u <= uOptimal = im'base + im'slope1 * (u * R.recip uOptimal)
  | otherwise = im'base + im'slope2 * (u - uOptimal) * R.recip (R.fromInteger 1 - uOptimal)
  where
    uOptimal = im'optimalUtilisation

{-# INLINEABLE addDeposit #-}
addDeposit :: Rational -> Integer -> Types.Wallet -> Either BuiltinByteString Types.Wallet
addDeposit normalisedIncome amount wallet
  | newDeposit >= 0 =
    Right
      wallet
        { wallet'deposit = max 0 newDeposit
        , wallet'scaledBalance = max (R.fromInteger 0) $ wallet'scaledBalance wallet + R.fromInteger amount * R.recip normalisedIncome
        }
  | otherwise = Left "Negative deposit"
  where
    newDeposit = wallet'deposit wallet + amount

{-# INLINEABLE getCumulativeBalance #-}
getCumulativeBalance :: Rational -> Types.Wallet -> Rational
getCumulativeBalance normalisedIncome Types.Wallet {..} =
  wallet'scaledBalance * normalisedIncome
