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
  , InterestRate(..)
  , initReserve
  , Act(..)
  , UserAct(..)
  , PriceAct(..)
  , GovernAct(..)
  , LpAddressesProvider(..)
  , LpAddressesProviderRegistry(..)
  , Coin(..)
  , aToken
  , LpCollateralManager(..)
  , LpConfigurator(..)
  , PriceOracleProvider(..)
  , InterestRateStrategy(..)
  , showt
) where

import qualified Prelude as P
import qualified PlutusTx as PlutusTx
import PlutusTx.Prelude
import PlutusTx.AssocMap (Map)
import qualified PlutusTx.AssocMap as M
import GHC.Generics

import Data.String

{-# INLINABLE showt #-}
-- | Helper to print @Text@ values
showt :: Show a => a -> String
showt = fromString . show

-- | Address of the wallet that can hold values of assets
data UserId
  = UserId Integer  -- user address
  | Self            -- addres of the lending platform
  deriving (Show, Generic, P.Eq, P.Ord)

instance Eq UserId where
  {-# INLINABLE (==) #-}
  Self == Self = True
  UserId a == UserId b = a == b
  _ == _ = False

-- | Lending pool is a list of reserves
data LendingPool = LendingPool
  { lp'reserves :: !(Map Coin Reserve)   -- ^ list of reserves
  , lp'users    :: !(Map UserId User)    -- ^ internal user wallets on the app
  }
  deriving (Show, Generic)

-- | Reserve of give coin in the pool.
-- It holds all info on individual collaterals and deposits.
data Reserve = Reserve
  { reserve'wallet               :: !Wallet     -- ^ total amounts of coins deposited to reserve
  , reserve'rate                 :: !Rational   -- ^ ratio of reserve's coin to base currency
  , reserve'liquidationThreshold :: !Rational   -- ^ ratio at which liquidation of collaterals can happen for this coin
  }
  deriving (Show, Generic)

{-# INLINABLE initReserve #-}
-- | Initialise empty reserve with given ratio of its coin to ada
initReserve :: Rational -> Reserve
initReserve rate = Reserve
  { reserve'wallet = Wallet
      { wallet'deposit    = 0
      , wallet'borrow     = 0
      , wallet'collateral = 0
      }
  , reserve'rate                 = rate
  , reserve'liquidationThreshold = 8 % 10
  }

-- | User is a set of wallets per currency
data User = User
  { user'wallets         :: !(Map Coin Wallet)
  }
  deriving (Show, Generic)

{-# INLINABLE defaultUser #-}
-- | Default user with no wallets.
defaultUser :: User
defaultUser = User { user'wallets = M.empty }

-- | Internal walet of the lending app
--
-- All amounts are provided in the currency of the wallet
data Wallet = Wallet
  { wallet'deposit       :: !Integer   -- ^ amount of deposit
  , wallet'collateral    :: !Integer   -- ^ amount of collateral
  , wallet'borrow        :: !Integer   -- ^ amount of borrow
  }
  deriving (Show, Generic)

{-# INLINABLE defaultWallet #-}
defaultWallet :: Wallet
defaultWallet = Wallet 0 0 0

-- | Acts for lending platform
data Act
  = UserAct UserId UserAct   -- ^ user's actions
  | PriceAct PriceAct        -- ^ price oracle's actions
  | GovernAct GovernAct      -- ^ app admin's actions
  deriving (Show, Generic)

-- | Lending pool action
data UserAct
  = DepositAct
      { act'amount          :: Integer
      , act'asset           :: Coin
      }
  -- ^ deposit funds
  | BorrowAct
      { act'asset           :: Coin
      , act'amount          :: Integer
      , act'rate            :: InterestRate
      }
  -- ^ borrow funds. We have to allocate collateral to be able to borrow
  | RepayAct
      { act'asset           :: Coin
      , act'amount          :: Integer
      , act'rate            :: InterestRate
      }
  -- ^ repay part of the borrow
  | SwapBorrowRateModelAct
      { act'asset           :: Coin
      , act'rate            :: InterestRate
      }
  -- ^ swap borrow interest rate strategy (stable to variable)
  | SetUserReserveAsCollateralAct
      { act'asset           :: Coin       -- ^ which asset to use as collateral or not
      , act'useAsCollateral :: Bool       -- ^ should we use as collateral (True) or use as deposit (False)
      , act'portion         :: Rational   -- ^ poriton of deposit/collateral to change status (0, 1)
      }
  -- ^ set some portion of deposit as collateral or some portion of collateral as deposit
  | WithdrawAct
      { act'amount         :: Integer
      , act'asset          :: Coin
      }
  -- ^ withdraw funds from deposit
  | FlashLoanAct  -- TODO
  -- ^ flash loans happen within the single block of transactions
  | LiquidationCallAct
      { act'collateral     :: UserId  -- ^ collateral address
      , act'debt           :: UserId
      , act'user           :: UserId
      , act'debtToCover    :: Integer
      , act'receiveAToken  :: Bool
      }
  -- ^ call to liquidate borrows that are unsafe due to health check
  deriving (Show, Generic)

-- | Acts that can be done by admin users.
data GovernAct
  = AddReserve Coin Rational  -- ^ Adds new reserve
  deriving (Show, Generic)

-- | Updates for the prices of the currencies on the markets
data PriceAct
  = SetAssetPrice Coin Rational   -- ^ Set asset price
  | SetOracleAddr Coin UserId     -- ^ Provide address of the oracle
  deriving (Show, Generic)

-- | Custom currency
newtype Coin = Coin ByteString
  deriving newtype (Show, P.Eq, P.Ord)

instance Eq Coin where
  {-# INLINABLE (==) #-}
  Coin a == Coin b = a == b

{-# INLINABLE aToken #-}
-- | Appends a prefix to all coins
aToken :: Coin -> Coin
aToken (Coin bs) = Coin $ "a" <> bs

----------------------------------------------------
-- some types specific to aave
--

data LpAddressesProvider = LpAddressesProvider

newtype LpAddressesProviderRegistry
  = LpAddressesProviderRegistry [LpAddressesProvider]

data LpCollateralManager = LpCollateralManager

data LpConfigurator = LpConfigurator

data PriceOracleProvider = PriceOracleProvider

data InterestRateStrategy = InterestRateStrategy

data InterestRate = StableRate | VariableRate
  deriving (Show)

------------------------------------------

PlutusTx.unstableMakeIsData ''InterestRate
PlutusTx.unstableMakeIsData ''Coin
PlutusTx.unstableMakeIsData ''UserAct
PlutusTx.unstableMakeIsData ''PriceAct
PlutusTx.unstableMakeIsData ''GovernAct
PlutusTx.unstableMakeIsData ''UserId
PlutusTx.unstableMakeIsData ''User
PlutusTx.unstableMakeIsData ''Wallet
PlutusTx.unstableMakeIsData ''Reserve
PlutusTx.unstableMakeIsData ''LendingPool
PlutusTx.unstableMakeIsData ''Act

