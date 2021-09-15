{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Dex.LiquidityPool.PoolParts where
import           Control.Monad
import           Data.Text     (Text)
import           Dex.Types
import           Prelude


data NormalizedParts
  = NormalizedParts
      { normalizedPartsA        :: [Double]
      , normalizedVirtualPartsA :: [Double]
      , normalizedPartsB        :: [Double]
      , normalizedVirtualPartsB :: [Double]
      } deriving (Show)


generateNormalizedParts :: PoolPartsParams -> Either Text NormalizedParts
generateNormalizedParts PriceChangeParams {..} = do
    when (numberOfParts < 2) $ Left "number of parts must be at least 2"

    let coinAPriceChangeDouble = toDouble coinAPriceChange
    let (normalizedCoreA : normalizedPartsA) =
          [ 1 / (1 + (coinAPriceChangeDouble * x / (fromIntegral numberOfParts - 1)))
          | x <- [0..fromIntegral numberOfParts- 1 ]
          ]
    let normalizedVirtualPartsA =
          [ 1 / (1 - (coinAPriceChangeDouble * x / (fromIntegral numberOfParts - 1)))
          | x <- [1..fromIntegral numberOfParts - 1]
          ]

    let coinBPriceChangeDouble = toDouble coinBPriceChange
    let (normalizedCoreB : normalizedPartsB) =
          [ 1 / (1 + (coinBPriceChangeDouble * x / (fromIntegral numberOfParts - 1)))
          | x <- [0..fromIntegral numberOfParts - 1]
          ]
    let normalizedVirtualPartsB =
          [ 1 / (1 - (coinBPriceChangeDouble * x / (fromIntegral numberOfParts - 1)))
          | x <- [1..fromIntegral numberOfParts - 1]
          ]

    Right $ NormalizedParts
                { normalizedPartsA = (normalizedCoreA / 2) : normalizedPartsA
                , normalizedPartsB = (normalizedCoreB / 2) : normalizedPartsB
                , normalizedVirtualPartsA = normalizedVirtualPartsA
                , normalizedVirtualPartsB = normalizedVirtualPartsB
                }
