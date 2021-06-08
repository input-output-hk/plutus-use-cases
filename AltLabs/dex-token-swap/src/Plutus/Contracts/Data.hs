{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# options_ghc -Wno-redundant-constraints #-}
{-# options_ghc -fno-strictness            #-}
{-# options_ghc -fno-specialise            #-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
module Plutus.Contracts.Data
  where

import           Ledger
import           Ledger.Value        (AssetClass (..), assetClass, assetClassValue, assetClassValueOf)
import           Playground.Contract (FromJSON, Generic, ToJSON, ToSchema)
import qualified PlutusTx
import           PlutusTx.Prelude                 hiding (Semigroup (..), unless)
import qualified Prelude             as Haskell
import           Text.Printf         (PrintfArg)

-- uniswapTokenName and poolStateTokenName share the same minting policy
-- | A handy alias to put things in the language of "Coins" instead of
-- "AssetClass".
type Coin = AssetClass

-- Note: An orphan instance here because of the alias above.
deriving anyclass instance ToSchema AssetClass

-- 2 token liquidity Pool
-- Order does not matter A/B == B/A pool
data LiquidityPool = LiquidityPool
    { lpCoinA :: Coin
    , lpCoinB :: Coin
    }
    deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

PlutusTx.unstableMakeIsData ''LiquidityPool
PlutusTx.makeLift ''LiquidityPool

instance Eq LiquidityPool where
    {-# INLINABLE (==) #-}
    x == y = (lpCoinA x == lpCoinA y && lpCoinB x == lpCoinB y) ||
             (lpCoinA x == lpCoinB y && lpCoinB x == lpCoinA y)