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

module Plutus.Contracts.LendingPool.InterestRate where

import           Plutus.Abstract.IncentivizedAmount               (IncentivizedAmount (..))
import           Plutus.Contracts.LendingPool.OnChain.Core.Script (InterestRateModel (..),
                                                                   Reserve (..),
                                                                   UserConfig (..))
import           Plutus.V1.Ledger.Slot                            (Slot (..))
import           Plutus.V1.Ledger.Value                           (AssetClass)
import           PlutusTx.Prelude
import           PlutusTx.Ratio                                   (Ratio,
                                                                   Rational,
                                                                   denominator,
                                                                   numerator,
                                                                   reduce)
import qualified Prelude

{-# INLINABLE updateCumulativeIndices #-}
updateCumulativeIndices :: Reserve -> [UserConfig] -> Slot -> Reserve
updateCumulativeIndices reserve@Reserve{..} userConfigs currentSlot =
    if totalBorrows > (fromInteger 0)
        then
            if rLastLiquidityCumulativeIndex == fromInteger 0
                then
                    reserve {
                    rLastLiquidityCumulativeIndex = cumulatedLiquidityInterest,
                    rLastUpdated = currentSlot
                    }
                else
                    reserve {
                    rLastLiquidityCumulativeIndex = rLastLiquidityCumulativeIndex * cumulatedLiquidityInterest,
                    rLastUpdated = currentSlot
                    }
        else reserve
    where
        totalBorrows = getTotalBorrows userConfigs
        cumulatedLiquidityInterest = calculateLinearInterest rLastUpdated currentSlot rLiquidityRate

{-# INLINABLE getTotalBorrows #-}
getTotalBorrows :: [UserConfig] -> Rational
getTotalBorrows = foldr (\acc cur -> cur + (iaAmount . ucDebt $ acc)) (fromInteger 0)

{-# INLINABLE calculateLinearInterest #-}
calculateLinearInterest :: Slot -> Slot -> Rational -> Rational
calculateLinearInterest last current rate = rate * timeDelta
    where
        timeDifference = current - last
        timeDelta = getSlot timeDifference % getSlot slotsPerYear

slotsPerYear :: Slot
slotsPerYear = Slot 31536000

data RateParams = RateParams {
    rpAvailableLiquidity :: Integer,
    rpTotalBorrows       :: Rational
}

{-# INLINABLE updateReserveInterestRates #-}
updateReserveInterestRates :: RateParams -> Slot -> Rational -> Reserve -> Reserve
updateReserveInterestRates rateParams currentSlot averageStableBorrowRate reserve@Reserve{..} =
    reserve {
        rLiquidityRate = getCurrentLiqudityRate rateParams averageStableBorrowRate,
        rCurrentStableBorrowRate = getCurrentStableBorrowRate rInterestRateModel rateParams,
        rLastUpdated = currentSlot }

{-# INLINABLE getCurrentLiqudityRate #-}
getCurrentLiqudityRate :: RateParams -> Rational -> Rational
getCurrentLiqudityRate rateParams averageStableBorrowRate =
    if utilizationRate == fromInteger 0
        then fromInteger 0
        else borrowRate `divideRatio` utilizationRate
    where
        utilizationRate = getUtilizationRate rateParams
        borrowRate = if (rpTotalBorrows rateParams) == (fromInteger 0) then (fromInteger 0) else averageStableBorrowRate

defaultRateModel :: InterestRateModel
defaultRateModel = InterestRateModel {
    irmOptimalUtilizationRate = 8 % 10,
    irmExcessUtilizationRate = 2 % 10,
    irmStableRateSlope1 = 4 % 100,
    irmStableRateSlope2 = 1 % 1,
    irmMarketBorrowRate = 4 % 100
}

-- TODO: figure out the right way to do it
{-# INLINABLE divideRatio #-}
divideRatio :: Rational -> Rational -> Rational
divideRatio a b = reduce (numerator a * denominator b) (denominator a * numerator b)

{-# INLINABLE getCurrentStableBorrowRate #-}
getCurrentStableBorrowRate :: InterestRateModel -> RateParams -> Rational
getCurrentStableBorrowRate InterestRateModel{..} rateParams =
    if utilizationRate > irmOptimalUtilizationRate
        then
            let excessUtilizationRateRatio = (utilizationRate - irmOptimalUtilizationRate) `divideRatio` irmExcessUtilizationRate
                in irmMarketBorrowRate + irmStableRateSlope1 + irmStableRateSlope2 * excessUtilizationRateRatio
        else
            irmMarketBorrowRate + irmStableRateSlope1 * utilizationRate `divideRatio` irmOptimalUtilizationRate
    where
        utilizationRate = getUtilizationRate rateParams

{-# INLINABLE getUtilizationRate #-}
getUtilizationRate :: RateParams -> Rational
getUtilizationRate RateParams{..} =
    if rpTotalBorrows == (fromInteger 0) || rpAvailableLiquidity == 0
        then fromInteger 0
        else rpTotalBorrows `divideRatio` (rpTotalBorrows + fromInteger rpAvailableLiquidity)

{-# INLINABLE getNormalizedIncome #-}
getNormalizedIncome :: Reserve -> Slot -> Slot -> Rational
getNormalizedIncome Reserve{..} previous current =
    rLastLiquidityCumulativeIndex * calculateLinearInterest previous current rLiquidityRate
