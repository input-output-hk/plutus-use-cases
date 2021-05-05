-- | Types for lending app
--
-- inspired by aave spec. See
--
-- * https://docs.aave.com/developers/v/2.0/the-core-protocol/lendingpool
module Mlabs.Lending.Logic.Types(
    LendingPool(..)
  , Wallet(..)
  , defaultWallet
  , User(..)
  , defaultUser
  , UserId(..)
  , Reserve(..)
  , initReserve
  , Act(..)
  , UserAct(..)
  , PriceQuery(..)
  , PriceAct(..)
  , GovernAct(..)
  , LpAddressesProvider(..)
  , LpAddressesProviderRegistry(..)
  , Coin(..)
  , aToken
  , Addr(..)
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
  deriving (Show, Eq, Ord)

data UserId
  = UserId Integer  -- user address
  | Self            -- addres of the lending platform
  deriving (Show, Eq, Ord)

-- | Lending pool is a list of reserves
data LendingPool = LendingPool
  { lp'reserves :: !(Map Coin Reserve)
  , lp'users    :: !(Map UserId User)
  }
  deriving (Show)

-- | Reserve of give coin in the pool.
-- It holds all info on individual collaterals and deposits.
data Reserve = Reserve
  { reserve'wallet               :: !Wallet     -- ^ total amounts of coins deposited to reserve
  , reserve'rate                 :: !Rational   -- ^ ratio of reserve's coin to base currency
  , reserve'liquidationThreshold :: !Rational   -- ^ ratio at which liquidation of collaterals can happen for this coin
  }
  deriving (Show)

-- | Initialise empty reserve with given ratio of its coin to ada
initReserve :: Rational -> Reserve
initReserve rate = Reserve
  { reserve'wallet = Wallet
      { wallet'deposit    = 0
      , wallet'borrow     = 0
      , wallet'collateral = 0
      }
  , reserve'rate                 = rate
  , reserve'liquidationThreshold = 0.8
  }

data User = User
  { user'wallets         :: !(Map Coin Wallet)
  }
  deriving (Show)

defaultUser :: User
defaultUser = User mempty

data Wallet = Wallet
  { wallet'deposit       :: !Integer
  , wallet'collateral    :: !Integer
  , wallet'borrow        :: !Integer
  }
  deriving (Show)

defaultWallet :: Wallet
defaultWallet = Wallet 0 0 0

data UserConfig = UserConfig
  { userConfig'collaterals :: [Addr]
  , userConfig'borrows     :: [Borrow]
  }
  deriving (Show)

data Borrow = Borrow
  { borrow'amount   :: Integer
  , borrow'health   :: Rational
  }
  deriving (Show)

-- | Colateral
data Collateral = Collateral
  { collateral'amount   :: Integer
  }
  deriving (Show)

-- | Deposit
data Deposit = Deposit
  { deposit'amount      :: Integer
  }
  deriving (Show)

data Act = UserAct UserId UserAct | PriceAct PriceAct | GovernAct GovernAct
  deriving (Show)

-- | Lending pool action
data UserAct
  = DepositAct
      { act'amount          :: Integer
      , act'asset           :: Coin
      }
  | BorrowAct
      { act'asset           :: Coin
      , act'amount          :: Integer
      , act'rate            :: InterestRate
      }
  | RepayAct
      { act'asset           :: Coin
      , act'amount          :: Integer
      , act'rate            :: InterestRate
      }
  | SwapBorrowRateModelAct
      { act'asset           :: Coin
      , act'rate            :: InterestRate
      }
  | SetUserReserveAsCollateralAct
      { act'asset           :: Coin       -- ^ which asset to use as collateral or not
      , act'useAsCollateral :: Bool       -- ^ should we use as collateral (True) or use as deposit (False)
      , act'portion         :: Rational   -- ^ poriton of deposit/collateral to change status (0, 1)
      }
  | WithdrawAct
      { act'amount         :: Integer
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

-- | Appends a prefix to all coins
aToken :: Coin -> Coin
aToken (Coin bs) = Coin $ "a" <> bs

data LpCollateralManager = LpCollateralManager

data LpConfigurator = LpConfigurator

data PriceOracleProvider = PriceOracleProvider

data InterestRateStrategy = InterestRateStrategy

data InterestRate = StableRate | VariableRate
  deriving (Show)

