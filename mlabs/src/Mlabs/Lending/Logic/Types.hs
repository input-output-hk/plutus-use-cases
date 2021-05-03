-- | Types for lending app
--
-- inspired by aave spec. See
--
-- * https://docs.aave.com/developers/v/2.0/the-core-protocol/lendingpool
module Mlabs.Lending.Logic.Types(
    LendingPool(..)
  , Reserve(..)
  , Act(..)
  , LpAct(..)
  , PriceQuery(..)
  , PriceAct(..)
  , GovernAct(..)
  , LpAddressesProvider(..)
  , LpAddressesProviderRegistry(..)
  , Coin(..)
  , AToken(..)
  , LpCollateralManager(..)
  , LpConfigurator(..)
  , PriceOracleProvider(..)
  , InterestRateStrategy(..)
  , Collateral(..)
  , Deposit(..)
) where

import Prelude
import Data.Map.Strict (Map)
import Data.ByteString (ByteString)

-- | Address that can hold values of assets
newtype Addr = Addr Integer
  deriving (Show)

-- | Lending pool is a list of reserves
data LendingPool = LendingPool (Map Coin Reserve)
  deriving (Show)

-- | Reserve of give coin in the pool.
-- It holds all info on individual collaterals and deposits.
data Reserve = Reserve
  { reserve'liquidity   :: !Integer       -- ^ total amount of coins available in reserve
  , reserve'borrow      :: !Integer       -- ^ how much was already borrowed
  , reserve'collaterals :: ![Collateral]  -- ^ list of collaterals
  , reserve'deposits    :: ![Deposit]     -- ^ list of deposits
  , reserve'value       :: !Rational      -- ^ ratio of reserve's coin to base currency
  }
  deriving (Show)

-- | Colateral
data Collateral = Collateral
  { collateral'amount   :: Integer
  , collateral'health   :: Rational
  , collateral'addr     :: Addr
  }
  deriving (Show)

-- | Deposit
data Deposit = Deposit
  { deposit'amount      :: Integer
  , deposit'addr        :: Addr
  }
  deriving (Show)

data Act = LpAct LpAct | PriceAct PriceAct | GovernAct GovernAct
  deriving (Show)

-- | Lending pool action
data LpAct
  = DepositAct
      { act'amount          :: Integer
      , act'asset           :: Coin
      , act'onBehalfOf      :: Addr
      }
  | BorrowAct
      { act'asset           :: Coin
      , act'amount          :: Integer
      , act'rate            :: InterestRate
      , act'onBehalfOf      :: Addr
      }
  | RepayAct
      { act'asset           :: Coin
      , act'amount          :: Integer
      , act'rate            :: InterestRate
      , act'onBehalfOf      :: Addr
      }
  | SwapBorrowRateModelAct
      { act'asset           :: Coin
      , act'rate            :: InterestRate
      }
  | SetUserReserveAsCollateralAct
      { act'asset           :: Coin
      , act'useAsCollateral :: Bool
      }
  | WithdrawAct
      { act'to             :: Addr
      , act'amount         :: Integer
      , act'asset          :: Coin
      }
  | FlashLoanAct  -- TODO
  | LiquidationCallAct
      { act'collateral     :: Addr  -- ^ collateral address
      , act'debt           :: Addr
      , act'user           :: Addr
      , act'debtToCover    :: Integer
      , act'receiveAToken  :: Bool
      }
  deriving (Show)

data PriceQuery
  = GetAssetPrice Coin
  | GetAssetPrices [Coin]
  | GetOracleAddr Coin
  deriving (Show)

data GovernAct
  = AddReserve Coin Rational
  deriving (Show)

data PriceAct
  = SetAssetPrice Coin Rational
  | SetOracleAddr Coin Addr
  deriving (Show)

data LpAddressesProvider = LpAddressesProvider

newtype LpAddressesProviderRegistry
  = LpAddressesProviderRegistry [LpAddressesProvider]

newtype Coin = Coin ByteString
  deriving (Show, Eq, Ord)

newtype AToken = AToken Coin
  deriving (Show)

data LpCollateralManager = LpCollateralManager

data LpConfigurator = LpConfigurator

data PriceOracleProvider = PriceOracleProvider

data InterestRateStrategy = InterestRateStrategy

data InterestRate = StableRate | VariableRate
  deriving (Show)

