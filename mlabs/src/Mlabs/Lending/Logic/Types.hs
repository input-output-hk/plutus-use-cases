{-# OPTIONS_GHC -fno-specialize #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-specialize #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fobject-code #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
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
  , ReserveInterest(..)
  , InterestRate(..)
  , InterestModel(..)
  , defaultInterestModel
  , CoinCfg(..)
  , initReserve
  , initLendingPool
  , Act(..)
  , UserAct(..)
  , PriceAct(..)
  , GovernAct(..)
  , LpAddressesProvider(..)
  , LpAddressesProviderRegistry(..)
  , Coin
  , toLendingToken
  , fromLendingToken
  , fromAToken
  , LpCollateralManager(..)
  , LpConfigurator(..)
  , PriceOracleProvider(..)
  , InterestRateStrategy(..)
  , Showt(..)
) where


import Data.Aeson (FromJSON, ToJSON)

import qualified PlutusTx.Ratio as R
import qualified Prelude as P
import qualified PlutusTx as PlutusTx
import PlutusTx.Prelude
import Plutus.V1.Ledger.Value (AssetClass(..), TokenName(..), CurrencySymbol(..))
import PlutusTx.AssocMap (Map)
import qualified PlutusTx.AssocMap as M
import GHC.Generics
import Plutus.V1.Ledger.Crypto (PubKeyHash(..))

-- | Class that converts to inlinable builtin string
class Showt a where
  showt :: a -> String

-- | Address of the wallet that can hold values of assets
data UserId
  = UserId PubKeyHash  -- user address
  | Self               -- addres of the lending platform
  deriving stock (Show, Generic, P.Eq, P.Ord)
  deriving anyclass (FromJSON, ToJSON)

instance Eq UserId where
  {-# INLINABLE (==) #-}
  Self == Self = True
  UserId a == UserId b = a == b
  _ == _ = False

-- | Lending pool is a list of reserves
data LendingPool = LendingPool
  { lp'reserves :: !(Map Coin Reserve)   -- ^ list of reserves
  , lp'users    :: !(Map UserId User)    -- ^ internal user wallets on the app
  , lp'currency :: !CurrencySymbol       -- ^ main currencySymbol of the app
  , lp'coinMap  :: !(Map TokenName Coin) -- ^ maps aTokenNames to actual coins
  }
  deriving (Show, Generic)

-- | Reserve of give coin in the pool.
-- It holds all info on individual collaterals and deposits.
data Reserve = Reserve
  { reserve'wallet               :: !Wallet     -- ^ total amounts of coins deposited to reserve
  , reserve'rate                 :: !Rational   -- ^ ratio of reserve's coin to base currency
  , reserve'liquidationThreshold :: !Rational   -- ^ ratio at which liquidation of collaterals can happen for this coin
  , reserve'aToken               :: !TokenName  -- ^ aToken corresponding to the coin of the reserve
  , reserve'interest             :: !ReserveInterest -- ^ reserve liquidity params
  }
  deriving (Show, Generic)

-- | Parameters for calculation of interest rates.
data ReserveInterest = ReserveInterest
  { ri'interestModel      :: !InterestModel
  , ri'liquidityRate      :: !Rational
  , ri'liquidityIndex     :: !Rational
  , ri'normalisedIncome   :: !Rational
  , ri'lastUpdateTime     :: !Integer
  }
  deriving (Show, Generic)

data InterestModel = InterestModel
  { im'optimalUtilisation  :: !Rational
  , im'slope1              :: !Rational
  , im'slope2              :: !Rational
  , im'base                :: !Rational
  }
  deriving (Show, Generic, P.Eq)
  deriving anyclass (FromJSON, ToJSON)

defaultInterestModel :: InterestModel
defaultInterestModel = InterestModel
  { im'base   = R.fromInteger 0
  , im'slope1 = 1 % 5
  , im'slope2 = R.fromInteger 4
  , im'optimalUtilisation = 8 % 10
  }

-- | Coin configuration
data CoinCfg = CoinCfg
  { coinCfg'coin          :: Coin
  , coinCfg'rate          :: Rational
  , coinCfg'aToken        :: TokenName
  , coinCfg'interestModel :: InterestModel
  }
  deriving stock (Show, Generic, P.Eq)
  deriving anyclass (FromJSON, ToJSON)

{-# INLINABLE initLendingPool #-}
initLendingPool :: CurrencySymbol -> [CoinCfg] -> LendingPool
initLendingPool curSym coinCfgs = LendingPool reserves M.empty curSym coinMap
  where
    reserves = M.fromList $ fmap (\cfg -> (coinCfg'coin cfg, initReserve cfg)) coinCfgs
    coinMap  = M.fromList $ fmap (\(CoinCfg coin _ aToken _) -> (aToken, coin)) coinCfgs

{-# INLINABLE initReserve #-}
-- | Initialise empty reserve with given ratio of its coin to ada
initReserve :: CoinCfg -> Reserve
initReserve CoinCfg{..} = Reserve
  { reserve'wallet = Wallet
      { wallet'deposit       = 0
      , wallet'borrow        = 0
      , wallet'collateral    = 0
      , wallet'scaledBalance = R.fromInteger 0
      }
  , reserve'rate                 = coinCfg'rate
  , reserve'liquidationThreshold = 8 % 10
  , reserve'aToken               = coinCfg'aToken
  , reserve'interest             = initInterest coinCfg'interestModel
  }
  where
    initInterest interestModel = ReserveInterest
      { ri'interestModel    = interestModel
      , ri'liquidityRate    = R.fromInteger 0
      , ri'liquidityIndex   = R.fromInteger 1
      , ri'normalisedIncome = R.fromInteger 1
      , ri'lastUpdateTime   = 0
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
  , wallet'scaledBalance :: !Rational  -- ^ scaled balance
  }
  deriving (Show, Generic)


{-# INLINABLE defaultWallet #-}
defaultWallet :: Wallet
defaultWallet = Wallet 0 0 0 (R.fromInteger 0)

-- | Acts for lending platform
data Act
  = UserAct
      { userAct'time        :: Integer
      , userAct'userId      :: UserId
      , userAct'act         :: UserAct
      }                              -- ^ user's actions
  | PriceAct PriceAct                -- ^ price oracle's actions
  | GovernAct GovernAct              -- ^ app admin's actions
  deriving stock (Show, Generic, P.Eq)
  deriving anyclass (FromJSON, ToJSON)

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
  deriving stock (Show, Generic, P.Eq)
  deriving anyclass (FromJSON, ToJSON)

-- | Acts that can be done by admin users.
data GovernAct
  = AddReserve CoinCfg  -- ^ Adds new reserve
  deriving stock (Show, Generic, P.Eq)
  deriving anyclass (FromJSON, ToJSON)

-- | Updates for the prices of the currencies on the markets
data PriceAct
  = SetAssetPrice Coin Rational   -- ^ Set asset price
  | SetOracleAddr Coin UserId     -- ^ Provide address of the oracle
  deriving stock (Show, Generic, P.Eq)
  deriving anyclass (FromJSON, ToJSON)

-- | Custom currency
type Coin = AssetClass

{-# INLINABLE toLendingToken #-}
toLendingToken :: LendingPool -> Coin -> Maybe Coin
toLendingToken LendingPool{..} coin =
  flip fmap (M.lookup coin lp'reserves) $ \Reserve{..} -> AssetClass (lp'currency, reserve'aToken)

{-# INLINABLE fromAToken #-}
fromAToken :: LendingPool -> TokenName -> Maybe Coin
fromAToken LendingPool{..} tn = M.lookup tn lp'coinMap

{-# INLINABLE fromLendingToken #-}
fromLendingToken :: LendingPool -> Coin -> Maybe Coin
fromLendingToken lp (AssetClass (_ ,tn)) = fromAToken lp tn

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
  deriving stock (Show, Generic, P.Eq)
  deriving anyclass (FromJSON, ToJSON)

---------------------------------------------------------------
-- boilerplate instances

PlutusTx.unstableMakeIsData ''CoinCfg
PlutusTx.unstableMakeIsData ''InterestModel
PlutusTx.unstableMakeIsData ''InterestRate
PlutusTx.unstableMakeIsData ''ReserveInterest
PlutusTx.unstableMakeIsData ''UserAct
PlutusTx.unstableMakeIsData ''PriceAct
PlutusTx.unstableMakeIsData ''GovernAct
PlutusTx.unstableMakeIsData ''UserId
PlutusTx.unstableMakeIsData ''User
PlutusTx.unstableMakeIsData ''Wallet
PlutusTx.unstableMakeIsData ''Reserve
PlutusTx.unstableMakeIsData ''LendingPool
PlutusTx.unstableMakeIsData ''Act

