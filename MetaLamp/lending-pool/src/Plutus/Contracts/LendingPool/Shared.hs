{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fobject-code #-}

module Plutus.Contracts.LendingPool.Shared where

import           Plutus.Abstract.IncentivizedAmount               (IncentivizedAmount (..),
                                                                   accrue)
import qualified Plutus.Contracts.LendingPool.InterestRate        as InterestRate
import           Plutus.Contracts.LendingPool.OnChain.Core.Script (Reserve (..),
                                                                   UserConfig (..))
import           Plutus.V1.Ledger.Slot                            (Slot (..))
import           PlutusTx.Prelude
import           PlutusTx.Ratio                                   (Rational,
                                                                   denominator,
                                                                   numerator,
                                                                   reduce)
import qualified Prelude

-- refer to getCompoundedBorrowBalance and calculateCompoundedInterest
{-# INLINABLE accrueDebt #-}
accrueDebt :: Slot -> Rational -> IncentivizedAmount -> IncentivizedAmount
accrueDebt slot newRate IncentivizedAmount {..} =
    if iaAmount == fromInteger 0
        then IncentivizedAmount slot newRate iaAmount
        else IncentivizedAmount slot newRate (reduce (numerator newAmount) (denominator newAmount))
    where
        timeDifference = getSlot $ slot - iaSlot
        ratePerSecond = iaRate `InterestRate.divideRatio` (fromInteger . getSlot $ InterestRate.slotsPerYear)
        newAmount = iaAmount * (fromInteger 1 + powRatio ratePerSecond timeDifference)

{-# INLINABLE powRatio #-}
powRatio :: Rational -> Integer -> Rational
powRatio a n = reduce (numerator a `somePow` n) (denominator a `somePow` n)

-- can't use Prelude - also need to figure out what to do with n < 0 case
{-# INLINABLE somePow #-}
somePow :: Integer -> Integer -> Integer
somePow a n
    | n == 0 = 1
    | n == 1 = a
    | otherwise = a * somePow a (n - 1)

-- refer to AToken's module balanceOf and calculateCumulatedBalanceInternal
{-# INLINABLE accrueInterest #-}
accrueInterest :: Slot -> Slot -> Reserve -> IncentivizedAmount -> IncentivizedAmount
accrueInterest previousReserveUpdated slot reserve IncentivizedAmount {..} =
    if iaRate == fromInteger 0 || iaAmount == fromInteger 0
              then IncentivizedAmount slot newIncome iaAmount
              else
                IncentivizedAmount
                  slot
                  newIncome
                  (reduce (numerator newAmount) (denominator newAmount))

    where
        newIncome = InterestRate.getNormalizedIncome reserve previousReserveUpdated slot
        newAmount = (iaAmount * (newIncome + fromInteger 1)) `InterestRate.divideRatio` (iaRate + fromInteger 1)

data UpdateConfigParams =
    UpdateConfigParams
    {
        ucpUpdatedReserve         :: Reserve,
        ucpPreviousReserveUpdated :: Slot,
        ucpCurrentSlot            :: Slot
    }

{-# INLINABLE updateConfigAmounts #-}
updateConfigAmounts :: UpdateConfigParams -> UserConfig -> UserConfig
updateConfigAmounts UpdateConfigParams {..} UserConfig{..} =
    UserConfig {
        ucDebt = accrueDebt curSlot (rCurrentStableBorrowRate ucpUpdatedReserve) ucDebt,
        ucCollateralizedInvestment = accrueInterest ucpPreviousReserveUpdated curSlot ucpUpdatedReserve ucCollateralizedInvestment
    }
        where
            curSlot = Slot . (*10) . getSlot $ ucpCurrentSlot

{-# INLINABLE getAverageStableBorrowRate #-}
getAverageStableBorrowRate :: [UserConfig] -> Rational
getAverageStableBorrowRate userConfigs = getAverageStableBorrow userConfigs * InterestRate.getTotalBorrows userConfigs

{-# INLINABLE getAverageStableBorrow #-}
getAverageStableBorrow :: [UserConfig] -> Rational
getAverageStableBorrow userConfigs =
    if rateDivisor == 0
        then fromInteger 0
        else rateSum `InterestRate.divideRatio` (fromInteger rateDivisor)
    where
        rateSum = foldr
            (\cur acc -> if ((== fromInteger 0) . iaAmount . ucDebt $ cur) then acc else acc + (iaRate . ucDebt $ cur)) (fromInteger 0) userConfigs
        rateDivisor = foldr (\cur acc -> if ((== fromInteger 0) . iaAmount . ucDebt $ cur) then acc else acc + 1) 0 userConfigs

-- refer to updateReserveInterestRatesAndTimestampInternal
updateReserveOnLiquidityChange :: [UserConfig] -> Integer -> Slot -> Reserve -> Reserve
updateReserveOnLiquidityChange userConfigs newAvailableLiquidity slot reserve =
    InterestRate.updateReserveInterestRates rateParams slot averageStableBorrowRate $ InterestRate.updateCumulativeIndices reserve userConfigs slot
        where
            totalBorrows = InterestRate.getTotalBorrows userConfigs
            rateParams = InterestRate.RateParams newAvailableLiquidity totalBorrows
            averageStableBorrowRate = getAverageStableBorrowRate userConfigs

-- refer to increaseTotalBorrowsStableAndUpdateAverageRate
{-# INLINABLE updateReserveOnBorrow #-}
updateReserveOnBorrow :: [UserConfig] -> Integer -> Integer -> Slot -> Reserve -> Reserve
updateReserveOnBorrow userConfigs availableLiquidity borrowAmount slot reserve@Reserve{..} =
    InterestRate.updateReserveInterestRates rateParams slot averageStableBorrowRate $ InterestRate.updateCumulativeIndices reserve userConfigs slot
        where
            previousAverageStableBorrowRate = getAverageStableBorrowRate userConfigs
            previousTotalBorrows = InterestRate.getTotalBorrows userConfigs
            totalBorrows = previousTotalBorrows + fromInteger borrowAmount
            weightedLastBorrow = fromInteger borrowAmount * rCurrentStableBorrowRate
            weightedPreviousTotalBorrows = previousTotalBorrows * previousAverageStableBorrowRate
            averageStableBorrowRate = (weightedLastBorrow + weightedPreviousTotalBorrows) `InterestRate.divideRatio` totalBorrows
            rateParams = InterestRate.RateParams availableLiquidity totalBorrows

-- refer to decreaseTotalBorrowsStableAndUpdateAverageRate
{-# INLINABLE updateReserveOnRepay #-}
updateReserveOnRepay :: [UserConfig] -> Integer -> Integer -> Slot -> Reserve -> Reserve
updateReserveOnRepay userConfigs availableLiquidity repayAmount slot reserve@Reserve{..} =
    InterestRate.updateReserveInterestRates rateParams slot averageStableBorrowRate $ InterestRate.updateCumulativeIndices reserve userConfigs slot
        where
            previousAverageStableBorrowRate = getAverageStableBorrowRate userConfigs
            previousTotalBorrows = InterestRate.getTotalBorrows userConfigs
            totalBorrows = previousTotalBorrows - fromInteger repayAmount
            weightedLastBorrow = fromInteger repayAmount * rCurrentStableBorrowRate
            weightedPreviousTotalBorrows = previousTotalBorrows * previousAverageStableBorrowRate
            averageStableBorrowRate = (weightedPreviousTotalBorrows - weightedLastBorrow) `InterestRate.divideRatio` totalBorrows
            rateParams = InterestRate.RateParams availableLiquidity totalBorrows

{-
Regarding reserve updates:

rLiquidityRate + rLastLiquidityCumulativeIndex are used to calculate normalizedIncome,
which is the final param used to accrue interest with IncentivizedToken.

Update flow:

1. Available liquidity changes(provide/revoke) or total borrow changes (borrow/repay)

2. updateCumulativeIndices recalculates rLastLiquidityCumulativeIndex, which is then used to update user config collateral. 
This is done before each operation.

3. updateReserveInterestRates - this should also be done on each action, since availableLiquidity and totalBorrows change on every action
and they are used to calculate the current rates.

-}

