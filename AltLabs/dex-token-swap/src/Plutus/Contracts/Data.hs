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
  -- (
  --   poolStateTokenName,
  --   uniswapTokenName,
  --   usCoin,
  --   Coin,
  --   Uniswap, Uniswapping,
  --   UniswapDatum,
  --   LiquidityPool,
  --   CreateParams,
  --   AddParams,
  --   RemoveParams,
  --   CloseParams,
  --   SwapParams,
  --   UniswapAction,
  --   UserContractState,
  -- ) where

import           Ledger
import           Ledger.Value        (AssetClass (..), assetClass, assetClassValue, assetClassValueOf)
import           Playground.Contract (FromJSON, Generic, ToJSON, ToSchema)
import qualified Ledger.Typed.Scripts             as Scripts
import qualified PlutusTx
import           PlutusTx.Prelude                 hiding (Semigroup (..), unless)
import           PlutusTx.Sqrt
import           Prelude                          (Semigroup (..))
import qualified Prelude
import           Text.Printf         (PrintfArg)
import qualified Data.Aeson.Encode.Pretty                as JSON
import           Data.Text.Prettyprint.Doc

uniswapTokenName, poolStateTokenName :: TokenName
-- state token for the "factory" (unique token)
-- "factory" is first created, this token gets minted
-- and put to the UTXO of the "factory" and stays there forever
uniswapTokenName = "Uniswap"

-- state token of the liquidity pools (each pool has same token)
poolStateTokenName = "Pool State"

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

instance Eq LiquidityPool where
    {-# INLINABLE (==) #-}
    x == y = (lpCoinA x == lpCoinA y && lpCoinB x == lpCoinB y) ||
             (lpCoinA x == lpCoinB y && lpCoinB x == lpCoinA y)

-- | Type of the Uniswap user contract state.
data UserContractState =
      Pools [((Coin, Integer), (Coin, Integer))]
    | Funds Value
    | Created
    | Swapped
    | Added
    | Removed
    | Closed
    | Stopped
    deriving (Show, Generic, FromJSON, ToJSON)
  
-- DATUM that each UTXO carries
--
data UniswapDatum =
      Factory [LiquidityPool] -- list of existing liquidity pools
    | Pool LiquidityPool Integer -- Integer is the amount of tokens that have been minted for this LP
    deriving stock (Show)

-- Redeemer
-- possible actions AFTER the "factory" is setup
data UniswapAction = Create LiquidityPool | Close | Swap | Remove | Add
    deriving Show

-- just a wrapper around the "Coin" type (CurrencySymbol + TokenNAme)
newtype Uniswap = Uniswap
    { usCoin :: Coin
    } deriving stock    (Show, Generic)
      deriving anyclass (ToJSON, FromJSON, ToSchema)
      deriving newtype  (Prelude.Eq, Prelude.Ord)

data Uniswapping
instance Scripts.ScriptType Uniswapping where
    type instance RedeemerType Uniswapping = UniswapAction
    type instance DatumType Uniswapping = UniswapDatum

data UniswapContracts =
      Init
    | UniswapStart
    | UniswapUser Uniswap
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

instance Pretty UniswapContracts where
    pretty = viaShow

-- | Parameters for the @create@-endpoint, which creates a new liquidity pool.
data CreateParams = CreateParams
    { cpCoinA   :: Coin    -- ^ One 'Coin' of the liquidity pair.
    , cpCoinB   :: Coin    -- ^ The other 'Coin'.
    , cpAmountA :: Integer -- ^ Amount of liquidity for the first 'Coin'.
    , cpAmountB :: Integer -- ^ Amount of liquidity for the second 'Coin'.
    } deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

-- | Parameters for the @swap@-endpoint, which allows swaps between the two different coins in a liquidity pool.
-- One of the provided amounts must be positive, the other must be zero.
data SwapParams = SwapParams
    { spCoinA   :: Coin           -- ^ One 'Coin' of the liquidity pair.
    , spCoinB   :: Coin           -- ^ The other 'Coin'.
    , spAmountA :: Integer        -- ^ The amount the first 'Coin' that should be swapped.
    , spAmountB :: Integer        -- ^ The amount of the second 'Coin' that should be swapped.
    } deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

-- | Parameters for the @close@-endpoint, which closes a liquidity pool.
data CloseParams = CloseParams
    { clpCoinA :: Coin           -- ^ One 'Coin' of the liquidity pair.
    , clpCoinB :: Coin           -- ^ The other 'Coin' of the liquidity pair.
    } deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

-- | Parameters for the @remove@-endpoint, which removes some liquidity from a liquidity pool.
data RemoveParams = RemoveParams
    { rpCoinA :: Coin           -- ^ One 'Coin' of the liquidity pair.
    , rpCoinB :: Coin           -- ^ The other 'Coin' of the liquidity pair.
    , rpDiff  :: Integer        -- ^ The amount of liquidity tokens to burn in exchange for liquidity from the pool.
    } deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

-- | Parameters for the @add@-endpoint, which adds liquidity to a liquidity pool in exchange for liquidity tokens.
data AddParams = AddParams
    { apCoinA   :: Coin           -- ^ One 'Coin' of the liquidity pair.
    , apCoinB   :: Coin           -- ^ The other 'Coin' of the liquidity pair.
    , apAmountA :: Integer        -- ^ The amount of coins of the first kind to add to the pool.
    , apAmountB :: Integer        -- ^ The amount of coins of the second kind to add to the pool.
    } deriving (Show, Generic, ToJSON, FromJSON, ToSchema)
  
PlutusTx.unstableMakeIsData ''LiquidityPool
PlutusTx.makeLift ''LiquidityPool

PlutusTx.makeLift ''Uniswap

PlutusTx.unstableMakeIsData ''UniswapAction
PlutusTx.makeLift ''UniswapAction

PlutusTx.unstableMakeIsData ''UniswapDatum
PlutusTx.makeLift ''UniswapDatum