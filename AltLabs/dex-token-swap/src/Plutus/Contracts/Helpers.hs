{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE StandaloneDeriving         #-}
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
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# options_ghc -fno-warn-orphans       #-}

-- | A decentralized exchange for arbitrary token pairs following the
-- [Uniswap protocol](https://uniswap.org/whitepaper.pdf).
--
module Plutus.Contracts.Helpers
    (
      poolStateCoin,
      coinValueOf,
      mkCoin,
      findOwnInput, findOwnInput',
      valueWithin,
      liquidityPolicy, liquidityCurrency,
      checkSwap,
      coin,
      findSwapA, findSwapB,
      funds
    ) where

import           Control.Monad                    hiding (fmap)
import qualified Data.Map                         as Map
import           Data.Monoid                      (Last (..))
import           Data.Proxy                       (Proxy (..))
import           Data.Text                        (Text, pack)
import           Data.Void                        (Void)
import           Ledger                           hiding (singleton)
import           Ledger.Constraints               as Constraints
import           Ledger.Constraints.OnChain       as Constraints
import           Ledger.Constraints.TxConstraints as Constraints
import qualified Ledger.Typed.Scripts             as Scripts
import           Ledger.Value                     (AssetClass (..), assetClass, assetClassValue, assetClassValueOf,
                                                   symbols, unCurrencySymbol, unTokenName, flattenValue)
import           Playground.Contract
import           Plutus.Contract                  hiding (when)
import qualified Plutus.Contracts.Currency        as Currency
import qualified PlutusTx
import           PlutusTx.Prelude                 hiding (Semigroup (..), unless)
import           PlutusTx.Sqrt
import           Prelude                          (Semigroup (..))
import qualified Prelude
import           Text.Printf               
import qualified Prelude                           as Haskell (Int, Semigroup(..), String, div, dropWhile, flip, show, (^))     

import Plutus.Contracts.Data
import Plutus.Contracts.LiquidityPool

-- helper function to construct Values
-- Value (Cardano) is a "bag" of coins (1 ADA, 3 NFT etc...)
-- Dictionary of Dictionary of Int { CurrencySymbol: { TokenName: Int }}
{-# INLINABLE coin #-}
coin :: AssetClass -> Integer -> Value
coin = assetClassValue

-- How many pieces of this Coin are contained in Value
{-# INLINABLE coinValueOf #-}
coinValueOf :: Value -> AssetClass -> Integer
coinValueOf = assetClassValueOf

-- CurrencySymbol = hash of the pliutus script (which is run when you mint/burn token)
-- TokenName = bytestring (aka "Uniswap")
{-# INLINABLE mkCoin #-}
mkCoin:: CurrencySymbol -> TokenName -> AssetClass
mkCoin = assetClass

{-# INLINABLE findOwnInput' #-}
findOwnInput' :: ScriptContext -> TxInInfo
findOwnInput' ctx = fromMaybe (error ()) (findOwnInput ctx)

{-# INLINABLE valueWithin #-}
valueWithin :: TxInInfo -> Value
valueWithin = txOutValue . txInInfoResolved


-- GETTERS
-- | Gets the caller's funds.
funds :: forall w s. Contract w s Text Value
funds = do
    pkh <- pubKeyHash <$> ownPubKey
    os  <- map snd . Map.toList <$> utxoAt (pubKeyHashAddress pkh)
    let v = mconcat [txOutValue $ txOutTxOut o | o <- os]
    logInfo @Haskell.String $ "own funds: " ++ Haskell.show (Ledger.Value.flattenValue v)
    return v

-- Checks if swap is possible
-- oldA, oldB = existing amount
-- newA, newB = amount after the swap
-- calculates that the product does NOT decrease
{-# INLINABLE checkSwap #-}
checkSwap :: Integer -> Integer -> Integer -> Integer -> Bool
checkSwap oldA oldB newA newB =
    traceIfFalse "expected positive oldA" (oldA > 0) &&
    traceIfFalse "expected positive oldB" (oldB > 0) &&
    traceIfFalse "expected positive-newA" (newA > 0) &&
    traceIfFalse "expected positive-newB" (newB > 0) &&
    traceIfFalse "expected product to increase"
        ((((newA * feeDen) - (inA * feeNum)) * ((newB * feeDen) - (inB * feeNum)))
         >= (feeDen * feeDen * oldA * oldB))
  where
    inA, inB :: Integer
    inA = max 0 $ newA - oldA
    inB = max 0 $ newB - oldB
    -- The uniswap fee is 0.3%; here it is multiplied by 1000, so that the
    -- on-chain code deals only in integers.
    -- See: <https://uniswap.org/whitepaper.pdf> Eq (11) (Page 7.)
    feeNum, feeDen :: Integer
    feeNum = 3
    feeDen = 1000

findSwapA :: Integer -> Integer -> Integer -> Integer
findSwapA oldA oldB inA
    | ub' <= 1   = 0
    | otherwise  = go 1 ub'
  where
    cs :: Integer -> Bool
    cs outB = checkSwap oldA oldB (oldA + inA) (oldB - outB)

    ub' :: Integer
    ub' = head $ dropWhile cs [2 Haskell.^ i | i <- [0 :: Haskell.Int ..]]

    go :: Integer -> Integer -> Integer
    go lb ub
        | ub == (lb + 1) = lb
        | otherwise      =
      let
        m = Haskell.div (ub + lb) 2
      in
        if cs m then go m ub else go lb m

findSwapB :: Integer -> Integer -> Integer -> Integer
findSwapB oldA oldB = findSwapA oldB oldA

{-# INLINABLE validateLiquidityForging #-}
validateLiquidityForging :: Uniswap -> TokenName -> () -> ScriptContext -> Bool
validateLiquidityForging us tn _ ctx = case [ i
                                          | i <- txInfoInputs $ scriptContextTxInfo ctx
                                          , let v = valueWithin i
                                          , (coinValueOf v usC == 1) ||
                                            (coinValueOf v lpC == 1)
                                          ] of
    [_]    -> True
    [_, _] -> True
    _      -> traceError "pool state forging without Uniswap input"
  where
    usC, lpC :: Coin
    usC = usCoin us
    lpC = mkCoin (ownCurrencySymbol ctx) tn

liquidityPolicy :: Uniswap -> MintingPolicy
liquidityPolicy us = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \u t -> Scripts.wrapMintingPolicy (validateLiquidityForging u t) ||])
        `PlutusTx.applyCode` PlutusTx.liftCode us
        `PlutusTx.applyCode` PlutusTx.liftCode poolStateTokenName

liquidityCurrency :: Uniswap -> CurrencySymbol
liquidityCurrency = scriptCurrencySymbol . liquidityPolicy

poolStateCoin :: Uniswap -> Coin
poolStateCoin = flip mkCoin poolStateTokenName . liquidityCurrency
