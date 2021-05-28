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
  , LendexId(..)
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
  , CoinRate(..)
  , adaCoin
  , initReserve
  , initLendingPool
  , Act(..)
  , UserAct(..)
  , HealthReport
  , BadBorrow(..)
  , PriceAct(..)
  , GovernAct(..)
  , Coin
  , toLendingToken
  , fromLendingToken
  , fromAToken
) where

import Data.Aeson (FromJSON, ToJSON)

import qualified PlutusTx.Ratio as R
import qualified Prelude as Hask
import qualified PlutusTx as PlutusTx
import PlutusTx.Prelude
import Plutus.V1.Ledger.Value (AssetClass(..), TokenName(..), CurrencySymbol(..))
import PlutusTx.AssocMap (Map)
import qualified PlutusTx.AssocMap as M
import GHC.Generics

import Mlabs.Emulator.Types

-- | Unique identifier of the lending pool state.
newtype LendexId = LendexId ByteString
  deriving stock (Show, Generic)
  deriving newtype (Eq)
  deriving anyclass (ToJSON, FromJSON)

-- | Lending pool is a list of reserves
data LendingPool = LendingPool
  { lp'reserves       :: !(Map Coin Reserve)     -- ^ list of reserves
  , lp'users          :: !(Map UserId User)      -- ^ internal user wallets on the app
  , lp'currency       :: !CurrencySymbol         -- ^ main currencySymbol of the app
  , lp'coinMap        :: !(Map TokenName Coin)   -- ^ maps aTokenNames to actual coins
  , lp'healthReport   :: !HealthReport           -- ^ map of unhealthy borrows
  , lp'trustedOracles :: ![UserId]               -- ^ we accept price changes only for those users
  }
  deriving (Show, Generic)

-- | Reserve of give coin in the pool.
-- It holds all info on individual collaterals and deposits.
data Reserve = Reserve
  { reserve'wallet               :: !Wallet     -- ^ total amounts of coins deposited to reserve
  , reserve'rate                 :: !CoinRate   -- ^ ratio of reserve's coin to base currency
  , reserve'liquidationThreshold :: !Rational   -- ^ ratio at which liquidation of collaterals can happen for this coin
  , reserve'liquidationBonus     :: !Rational   -- ^ ratio of bonus for liquidation of the borrow in collateral of this asset
  , reserve'aToken               :: !TokenName  -- ^ aToken corresponding to the coin of the reserve
  , reserve'interest             :: !ReserveInterest -- ^ reserve liquidity params
  }
  deriving (Show, Generic)

type HealthReport = Map BadBorrow Rational

-- | Borrow that don't has enough collateral.
-- It has health check ration below one.
data BadBorrow = BadBorrow
  { badBorrow'userId :: !UserId   -- ^ user identifier
  , badBorrow'asset  :: !Coin     -- ^ asset of the borrow
  }
  deriving stock (Show, Generic, Hask.Eq)
  deriving anyclass (ToJSON, FromJSON)

instance Eq BadBorrow where
  {-# INLINABLE (==) #-}
  BadBorrow a1 b1 == BadBorrow a2 b2 = a1 == a2 && b1 == b2

-- | Price of the given currency to Ada.
data CoinRate = CoinRate
  { coinRate'value          :: !Rational -- ^ ratio to ada
  , coinRate'lastUpdateTime :: !Integer  -- ^ last time price was updated
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
  deriving (Show, Generic, Hask.Eq)
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
  { coinCfg'coin             :: Coin
  , coinCfg'rate             :: Rational
  , coinCfg'aToken           :: TokenName
  , coinCfg'interestModel    :: InterestModel
  , coinCfg'liquidationBonus :: Rational
  }
  deriving stock (Show, Generic, Hask.Eq)
  deriving anyclass (FromJSON, ToJSON)

{-# INLINABLE initLendingPool #-}
initLendingPool :: CurrencySymbol -> [CoinCfg] -> [UserId] -> LendingPool
initLendingPool curSym coinCfgs oracles =
  LendingPool
    { lp'reserves       = reserves
    , lp'users          = M.empty
    , lp'currency       = curSym
    , lp'coinMap        = coinMap
    , lp'healthReport   = M.empty
    , lp'trustedOracles = oracles
    }
  where
    reserves = M.fromList $ fmap (\cfg -> (coinCfg'coin cfg, initReserve cfg)) coinCfgs
    coinMap  = M.fromList $ fmap (\(CoinCfg coin _ aToken _ _) -> (aToken, coin)) coinCfgs

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
  , reserve'rate                 = CoinRate
                                    { coinRate'value          = coinCfg'rate
                                    , coinRate'lastUpdateTime = 0
                                    }
  , reserve'liquidationThreshold = 8 % 10
  , reserve'liquidationBonus     = coinCfg'liquidationBonus
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
  , user'lastUpdateTime  :: !Integer
  , user'health          :: !Health
  }
  deriving (Show, Generic)

-- | Health ratio for user per borrow
type Health = Map Coin Rational

{-# INLINABLE defaultUser #-}
-- | Default user with no wallets.
defaultUser :: User
defaultUser = User
  { user'wallets        = M.empty
  , user'lastUpdateTime = 0
  , user'health         = M.empty
  }

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
  | PriceAct
      { priceAct'time       :: Integer
      , priceAct'userId     :: UserId
      , priceAct'act        :: PriceAct
      }                              -- ^ price oracle's actions
  | GovernAct GovernAct              -- ^ app admin's actions
  deriving stock (Show, Generic, Hask.Eq)
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
      { act'collateral     :: Coin        -- ^ which collateral do we take for borrow repay
      , act'debt           :: BadBorrow   -- ^ identifier of the unhealthy borrow
      , act'debtToCover    :: Integer     -- ^ how much of the debt we cover
      , act'receiveAToken  :: Bool        -- ^ if true, the user receives the aTokens equivalent
                                          --   of the purchased collateral. If false, the user receives
                                          --   the underlying asset directly.
      }
  -- ^ call to liquidate borrows that are unsafe due to health check
  -- (see <https://docs.aave.com/faq/liquidations> for description)
  deriving stock (Show, Generic, Hask.Eq)
  deriving anyclass (FromJSON, ToJSON)

-- | Acts that can be done by admin users.
data GovernAct
  = AddReserve CoinCfg  -- ^ Adds new reserve
  deriving stock (Show, Generic, Hask.Eq)
  deriving anyclass (FromJSON, ToJSON)

-- | Updates for the prices of the currencies on the markets
data PriceAct
  = SetAssetPrice Coin Rational   -- ^ Set asset price
  deriving stock (Show, Generic, Hask.Eq)
  deriving anyclass (FromJSON, ToJSON)

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

data InterestRate = StableRate | VariableRate
  deriving stock (Show, Generic, Hask.Eq)
  deriving anyclass (FromJSON, ToJSON)

---------------------------------------------------------------
-- boilerplate instances

PlutusTx.unstableMakeIsData ''CoinCfg
PlutusTx.unstableMakeIsData ''CoinRate
PlutusTx.unstableMakeIsData ''InterestModel
PlutusTx.unstableMakeIsData ''InterestRate
PlutusTx.unstableMakeIsData ''ReserveInterest
PlutusTx.unstableMakeIsData ''UserAct
PlutusTx.unstableMakeIsData ''PriceAct
PlutusTx.unstableMakeIsData ''GovernAct
PlutusTx.unstableMakeIsData ''User
PlutusTx.unstableMakeIsData ''Wallet
PlutusTx.unstableMakeIsData ''Reserve
PlutusTx.unstableMakeIsData ''BadBorrow
PlutusTx.unstableMakeIsData ''LendingPool
PlutusTx.unstableMakeIsData ''Act

PlutusTx.unstableMakeIsData ''LendexId
PlutusTx.makeLift ''LendexId
