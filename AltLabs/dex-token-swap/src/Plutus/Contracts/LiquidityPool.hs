{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Plutus.Contracts.LiquidityPool
  ( calculateAdditionalLiquidity
  , calculateInitialLiquidity
  , calculateRemoval
  ) where
  
import           Ledger.Value                   (TokenName (..), unAssetClass, unCurrencySymbol)
import           Plutus.Contracts.Data
import           PlutusTx.Prelude
import           PlutusTx.Sqrt


{-# INLINABLE calculateInitialLiquidity #-}
calculateInitialLiquidity :: Integer -> Integer -> Integer
calculateInitialLiquidity outA outB = case isqrt (outA * outB) of
    Exactly l
        | l > 0 -> l
    Approximately l
        | l > 0 -> l + 1
    _           -> traceError "insufficient liquidity"

-- helper for if you have an existing Pool, and someone adds liquidity to it
-- eg. A (n) tokens, B (n) tokens
-- someone adds (n) A or B, computes how many
--  liquidity tokens the person should receive
{-# INLINABLE calculateAdditionalLiquidity #-}
calculateAdditionalLiquidity :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer
calculateAdditionalLiquidity oldA oldB liquidity delA delB =
  case rsqrt ((liquidity * liquidity * newProd) % oldProd) of
    Imaginary    -> traceError "insufficient liquidity"
    Exactly x      -> x - liquidity
    Approximately x -> x - liquidity
  where
    oldProd, newProd :: Integer
    oldProd = oldA * oldB
    newProd = (oldA + delA) * (oldB + delB)

-- same as above just opposite
{-# INLINABLE calculateRemoval #-}
calculateRemoval :: Integer -> Integer -> Integer -> Integer -> (Integer, Integer)
calculateRemoval inA inB liquidity diff = (f inA, f inB)
  where
    f :: Integer -> Integer
    f x = x - divide (x * diff) liquidity


