{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Common.Plutus.Contracts.Uniswap.Types where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics

newtype Coin a = Coin { unCoin :: Text }
  deriving (Generic, Show, ToJSON, FromJSON)

newtype Amount  a = Amount { unAmount :: Integer }
  deriving (Generic, Show, ToJSON, FromJSON)

data SwapParams = SwapParams            
    { spCoinA   :: Coin Text       -- ^ One 'Coin' of the liquidity pair.
    , spCoinB   :: Coin Text      -- ^ The other 'Coin'.          
    , spAmountA :: Amount Integer     -- ^ The amount the first 'Coin' that should be swapped.
    , spAmountB :: Amount Integer    -- ^ The amount of the second 'Coin' that should be swapped.
    } deriving (Generic, Show)

instance ToJSON SwapParams
instance FromJSON SwapParams
