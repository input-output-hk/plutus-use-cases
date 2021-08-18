{- Once GHC 8.10 is supported in Obelisk (currently only supports GHC 8.6)
 - , we will be able to reference plutus and plutus-starter functions directly.
 - For now, they will come from this module. This module is not necessary
 - for creating smart contracts, view at your own discretion.

 - This modules is full of functions used to provide estimates for swaps, liquidity redemption, etc.
-}

{-# LANGUAGE DerivingStrategies #-}

module Common.Plutus.Contracts.Uniswap.Estimates where

import Data.Ratio

traceIfFalse :: String -> Bool -> Bool
traceIfFalse _ a = if a then True else False

checkSwap
  :: Integer -- Amount A
  -> Integer -- Amount B
  -> Integer -- Amount A
  -> Integer -- Amount B
  -> Bool
checkSwap oldA oldB newA newB =
    traceIfFalse "expected positive oldA" (oldA > 0) &&
    traceIfFalse "expected positive oldB" (oldB > 0) &&
    traceIfFalse "expected positive-newA" (newA > 0) &&
    traceIfFalse "expected positive-newB" (newB > 0) &&
    traceIfFalse "expected product to increase"
        ((((newA * feeDen) - (inA * feeNum)) * ((newB * feeDen) - (inB * feeNum)))
         >= (feeDen * feeDen * oldA * oldB))
  where
    -- use max 0 just in case calculation result is less than 0
    inA = max 0 $ newA - oldA
    inB = max 0 $ newB - oldB
    -- The uniswap fee is 0.3%; here it is multiplied by 1000, so that the
    -- on-chain code deals only in integers.
    -- See: <https://uniswap.org/whitepaper.pdf> Eq (11) (Page 7.)
    feeNum, feeDen :: Integer
    feeNum = 3
    feeDen = 1000

findSwapA
  :: Integer -- original total of first coin in liquidity
  -> Integer -- original total of second coin in liquidity
  -> Integer -- amount of first coin being swapped for second coin
  -> Integer -- amount of second coin recieved
findSwapA oldA oldB inA
    | ub' <= 1   = 0
    | otherwise  = go 1 ub'
  where
    -- check if swap is valid and fee is computed correctly
    cs :: Integer -> Bool
    cs outB = checkSwap oldA oldB (oldA + inA) (oldB - outB)

    -- find the first integer where checkswap would be invalid
    ub' :: Integer
    ub' = head $ dropWhile cs [2 ^ i | i <- [0 :: Int ..]]

    -- determine amount to swap for
    go :: Integer -> Integer -> Integer
    go lb ub
        | ub == (lb + 1) = lb
        | otherwise      =
      let
        m = div (ub + lb) 2
      in
        if cs m then go m ub else go lb m

calculateRemoval
  :: Integer -- amount of first coin of token pool
  -> Integer -- amount of second coin of token pool
  -> Integer -- total amount of liquidity in token pool
  -> Integer -- amount of liquidity being redeemed from token pool
  -> (Integer, Integer) -- (amount of first coin liquidity remaining, amount of second coin liquidity remaining)
calculateRemoval inA inB liquidity diff = (g inA, g inB)
  where
    g :: Integer -> Integer
    g x = x - div (x * diff) liquidity

-- | Integer square-root representation, discarding imaginary integers.
data Sqrt
  -- | The number was negative, so we don't even attempt to compute it;
  -- just note that the result would be imaginary.
  = Imaginary
  -- | An exact integer result. The 'rsqrt' of 4 is 'Exactly 2'.
  | Exactly Integer
  -- | The Integer component (i.e. the floor) of a non-integral result. The
  -- 'rsqrt 2' is 'Approximately 1'.
  | Approximately Integer
  deriving stock (Show, Eq)

{-# INLINABLE rsqrt #-}
-- | Calculates the sqrt of a ratio of integers. As x / 0 is undefined,
-- calling this function with `d=0` results in an error.
rsqrt :: Rational -> Sqrt
rsqrt r
    | n * d < 0 = Imaginary
    | n == 0    = Exactly 0
    | n == d    = Exactly 1
    | n < d     = Approximately 0
    | n < 0     = rsqrt $ negate n % negate d
    | otherwise = go 1 $ 1 + div n d
  where
    n = numerator r
    d = denominator r
    go :: Integer -> Integer -> Sqrt
    go l u
        | l * l * d == n = Exactly l
        | u == (l + 1)   = Approximately l
        | otherwise      =
              let
                m = div (l + u) 2
              in
                if m * m * d <= n then go m u
                                  else go l m

calculateAdditionalLiquidity
  :: Integer -- Liquidity Pool Balance for first coin of token pool
  -> Integer -- Liquidity Pool Balance for second coin of token pool
  -> Integer -- Liquidity Pool Balance
  -> Integer -- Amount of first coin being added to token pool
  -> Integer -- Amount of second coin being added to token pool
  -> Integer -- Amount of Liquidity to be recieved
calculateAdditionalLiquidity oldA oldB liquidity delA delB =
  case rsqrt ratio of
    Imaginary       -> (-1)
    Exactly x       -> x - liquidity
    Approximately x -> x - liquidity
  where
    ratio = (liquidity * liquidity * newProd) % oldProd

    oldProd, newProd :: Integer
    oldProd = oldA * oldB
    newProd = (oldA + delA) * (oldB + delB)

