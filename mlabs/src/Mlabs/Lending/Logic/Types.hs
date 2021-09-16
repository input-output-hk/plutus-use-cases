{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-specialize #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fobject-code #-}

{- | Types for lending app

 inspired by aave spec. See

 * https://docs.aave.com/developers/v/2.0/the-core-protocol/lendingpool
-}
module Mlabs.Lending.Logic.Types (
  LendingPool (..),
  LendexId (..),
  Wallet (..),
  defaultWallet,
  User (..),
  defaultUser,
  UserId (..),
  Reserve (..),
  ReserveInterest (..),
  InterestRate (..),
  InterestModel (..),
  defaultInterestModel,
  CoinCfg (..),
  CoinRate (..),
  adaCoin,
  initReserve,
  initLendingPool,
  Act (..),
  QueryAct (..),
  UserAct (..),
  StartParams (..),
  HealthReport,
  BadBorrow (..),
  PriceAct (..),
  GovernAct (..),
  Coin,
  toLendingToken,
  fromLendingToken,
  fromAToken,
  QueryRes (..),
  SupportedCurrency (..),
  UserBalance (..),
) where

import PlutusTx.Prelude hiding ((%))

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Playground.Contract (ToSchema)
import Plutus.V1.Ledger.Crypto (PubKeyHash)
import Plutus.V1.Ledger.Tx (Address)
import Plutus.V1.Ledger.Value (AssetClass (..), CurrencySymbol (..), TokenName (..), Value)
import PlutusTx qualified
import PlutusTx.AssocMap (Map)
import PlutusTx.AssocMap qualified as M
import Prelude qualified as Hask (Eq, Show)

import Mlabs.Emulator.Types (Coin, UserId (..), adaCoin)
import PlutusTx.Ratio qualified as R

-- | Unique identifier of the lending pool state.
newtype LendexId = LendexId ByteString
  deriving stock (Hask.Show, Generic)
  deriving newtype (Eq)
  deriving anyclass (ToJSON, FromJSON)

-- | Lending pool is a list of reserves
data LendingPool = LendingPool
  { -- | list of reserves
    lp'reserves :: !(Map Coin Reserve)
  , -- | internal user wallets on the app
    lp'users :: !(Map UserId User)
  , -- | main currencySymbol of the app
    lp'currency :: !CurrencySymbol
  , -- | maps aTokenNames to actual coins
    lp'coinMap :: !(Map TokenName Coin)
  , -- | map of unhealthy borrows
    lp'healthReport :: !HealthReport
  , -- | we accept govern acts only for those users
    lp'admins :: ![UserId]
  , -- | we accept price changes only for those users
    lp'trustedOracles :: ![UserId]
  }
  deriving (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (FromJSON, ToJSON)

{- | Reserve of give coin in the pool.
 It holds all info on individual collaterals and deposits.
-}
data Reserve = Reserve
  { -- | total amounts of coins deposited to reserve
    reserve'wallet :: !Wallet
  , -- | ratio of reserve's coin to base currency
    reserve'rate :: !CoinRate
  , -- | ratio at which liquidation of collaterals can happen for this coin
    reserve'liquidationThreshold :: !Rational
  , -- | ratio of bonus for liquidation of the borrow in collateral of this asset
    reserve'liquidationBonus :: !Rational
  , -- | aToken corresponding to the coin of the reserve
    reserve'aToken :: !TokenName
  , -- | reserve liquidity params
    reserve'interest :: !ReserveInterest
  }
  deriving (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (FromJSON, ToJSON)

data StartParams = StartParams
  { -- | supported coins with ratios to ADA
    sp'coins :: [CoinCfg]
  , -- | init value deposited to the lending app
    sp'initValue :: Value
  , -- | admins
    sp'admins :: [PubKeyHash]
  , -- | trusted oracles
    sp'oracles :: [PubKeyHash]
  }
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

type HealthReport = Map BadBorrow Rational

{- | Borrow that doesn't have enough collateral.
 It has health check ration below one.
-}
data BadBorrow = BadBorrow
  { -- | user identifier
    badBorrow'userId :: !UserId
  , -- | asset of the borrow
    badBorrow'asset :: !Coin
  }
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (ToJSON, FromJSON)

instance Eq BadBorrow where
  {-# INLINEABLE (==) #-}
  BadBorrow a1 b1 == BadBorrow a2 b2 = a1 == a2 && b1 == b2

-- | Price of the given currency to Ada.
data CoinRate = CoinRate
  { -- | ratio to ada
    coinRate'value :: !Rational
  , -- | last time price was updated
    coinRate'lastUpdateTime :: !Integer
  }
  deriving (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (FromJSON, ToJSON)

-- | Parameters for calculation of interest rates.
data ReserveInterest = ReserveInterest
  { ri'interestModel :: !InterestModel
  , ri'liquidityRate :: !Rational
  , ri'liquidityIndex :: !Rational
  , ri'normalisedIncome :: !Rational
  , ri'lastUpdateTime :: !Integer
  }
  deriving (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (FromJSON, ToJSON)

data InterestModel = InterestModel
  { im'optimalUtilisation :: !Rational
  , im'slope1 :: !Rational
  , im'slope2 :: !Rational
  , im'base :: !Rational
  }
  deriving (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

defaultInterestModel :: InterestModel
defaultInterestModel =
  InterestModel
    { im'base = R.fromInteger 0
    , im'slope1 = 1 R.% 5
    , im'slope2 = R.fromInteger 4
    , im'optimalUtilisation = 8 R.% 10
    }

-- | Coin configuration
data CoinCfg = CoinCfg
  { coinCfg'coin :: Coin
  , coinCfg'rate :: Rational
  , coinCfg'aToken :: TokenName
  , coinCfg'interestModel :: InterestModel
  , coinCfg'liquidationBonus :: Rational
  }
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

{-# INLINEABLE initLendingPool #-}
initLendingPool :: CurrencySymbol -> [CoinCfg] -> [UserId] -> [UserId] -> LendingPool
initLendingPool curSym coinCfgs admins oracles =
  LendingPool
    { lp'reserves = reserves
    , lp'users = M.empty
    , lp'currency = curSym
    , lp'coinMap = coinMap
    , lp'healthReport = M.empty
    , lp'admins = admins
    , lp'trustedOracles = oracles
    }
  where
    reserves = M.fromList $ fmap (\cfg -> (cfg.coinCfg'coin, initReserve cfg)) coinCfgs
    coinMap = M.fromList $ fmap (\(CoinCfg coin _ aToken _ _) -> (aToken, coin)) coinCfgs

{-# INLINEABLE initReserve #-}

-- | Initialise empty reserve with given ratio of its coin to ada
initReserve :: CoinCfg -> Reserve
initReserve CoinCfg {..} =
  Reserve
    { reserve'wallet =
        Wallet
          { wallet'deposit = 0
          , wallet'borrow = 0
          , wallet'collateral = 0
          , wallet'scaledBalance = R.fromInteger 0
          }
    , reserve'rate =
        CoinRate
          { coinRate'value = coinCfg'rate
          , coinRate'lastUpdateTime = 0
          }
    , reserve'liquidationThreshold = 8 R.% 10
    , reserve'liquidationBonus = coinCfg'liquidationBonus
    , reserve'aToken = coinCfg'aToken
    , reserve'interest = initInterest coinCfg'interestModel
    }
  where
    initInterest interestModel =
      ReserveInterest
        { ri'interestModel = interestModel
        , ri'liquidityRate = R.fromInteger 0
        , ri'liquidityIndex = R.fromInteger 1
        , ri'normalisedIncome = R.fromInteger 1
        , ri'lastUpdateTime = 0
        }

-- | User is a set of wallets per currency
data User = User
  { user'wallets :: !(Map Coin Wallet)
  , user'lastUpdateTime :: !Integer
  , user'health :: !Health
  }
  deriving (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (FromJSON, ToJSON)

-- | Health ratio for user per borrow
type Health = Map Coin Rational

{-# INLINEABLE defaultUser #-}

-- | Default user with no wallets.
defaultUser :: User
defaultUser =
  User
    { user'wallets = M.empty
    , user'lastUpdateTime = 0
    , user'health = M.empty
    }

{- | Internal walet of the lending app

 All amounts are provided in the currency of the wallet
-}
data Wallet = Wallet
  { -- | amount of deposit
    wallet'deposit :: !Integer
  , -- | amount of collateral
    wallet'collateral :: !Integer
  , -- | amount of borrow
    wallet'borrow :: !Integer
  , -- | scaled balance
    wallet'scaledBalance :: !Rational
  }
  deriving (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (FromJSON, ToJSON)

{-# INLINEABLE defaultWallet #-}
defaultWallet :: Wallet
defaultWallet = Wallet 0 0 0 (R.fromInteger 0)

-- | Acts for lending platform
data Act
  = -- | user's actions
    UserAct
      { userAct'time :: Integer
      , userAct'userId :: UserId
      , userAct'act :: UserAct
      }
  | -- | price oracle's actions
    PriceAct
      { priceAct'time :: Integer
      , priceAct'userId :: UserId
      , priceAct'act :: PriceAct
      }
  | -- | app admin's actions
    GovernAct
      { governAct'userd :: UserId
      , goverAct'act :: GovernAct
      }
  | -- | app query actions
    QueryAct
      { queryAct'userId :: UserId
      , queryAct'time :: Integer
      , queryAct'act :: QueryAct
      }
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (FromJSON, ToJSON)

-- | Lending pool action
data UserAct
  = -- | deposit funds
    DepositAct
      { act'amount :: Integer
      , act'asset :: Coin
      }
  | -- | borrow funds. We have to allocate collateral to be able to borrow
    BorrowAct
      { act'amount :: Integer
      , act'asset :: Coin
      , act'rate :: InterestRate
      }
  | -- | repay part of the borrow
    RepayAct
      { act'amount :: Integer
      , act'asset :: Coin
      , act'rate :: InterestRate
      }
  | -- | swap borrow interest rate strategy (stable to variable)
    SwapBorrowRateModelAct
      { act'asset :: Coin
      , act'rate :: InterestRate
      }
  | -- | transfer amount of Asset from the user's Wallet to the Contract, locked as the user's Collateral
    AddCollateralAct
      { add'asset :: Coin
      , add'amount :: Integer
      }
  | -- | transfer amount of Asset from user's Collateral locked in Contract to user's Wallet
    RemoveCollateralAct
      { remove'asset :: Coin
      , remove'amount :: Integer
      }
  | -- | withdraw funds from deposit
    WithdrawAct
      { act'asset :: Coin
      , act'amount :: Integer
      }
  | -- | flash loans happen within the single block of transactions
    FlashLoanAct -- TODO
  | -- | call to liquidate borrows that are unsafe due to health check
    -- (see <https://docs.aave.com/faq/liquidations> for description)
    LiquidationCallAct
      { -- | which collateral do we take for borrow repay
        act'collateral :: Coin
      , -- | identifier of the unhealthy borrow
        act'debt :: BadBorrow
      , -- | how much of the debt we cover
        act'debtToCover :: Integer
      , -- | if true, the user receives the aTokens equivalent
        --   of the purchased collateral. If false, the user receives
        --   the underlying asset directly.
        act'receiveAToken :: Bool
      }
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (FromJSON, ToJSON)

-- | Query Actions.
newtype QueryAct
  = -- | Query current balance
    QueryCurrentBalanceAct ()
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (FromJSON, ToJSON)

-- | Acts that can be done by admin users.
newtype GovernAct
  = -- | Adds new reserve
    AddReserveAct CoinCfg
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (FromJSON, ToJSON)

-- | Updates for the prices of the currencies on the markets
data PriceAct
  = -- | Set asset price
    SetAssetPriceAct Coin Rational
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (FromJSON, ToJSON)

{-# INLINEABLE toLendingToken #-}
toLendingToken :: LendingPool -> Coin -> Maybe Coin
toLendingToken LendingPool {..} coin =
  flip fmap (M.lookup coin lp'reserves) $ \Reserve {..} -> AssetClass (lp'currency, reserve'aToken)

{-# INLINEABLE fromAToken #-}
fromAToken :: LendingPool -> TokenName -> Maybe Coin
fromAToken LendingPool {..} tn = M.lookup tn lp'coinMap

{-# INLINEABLE fromLendingToken #-}
fromLendingToken :: LendingPool -> Coin -> Maybe Coin
fromLendingToken lp (AssetClass (_, tn)) = fromAToken lp tn

data InterestRate = StableRate | VariableRate
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (FromJSON, ToJSON)

-- | Supported currency of `Reserve` in `LendingPool`
data SupportedCurrency = SupportedCurrency
  { -- | underlying
    sc'underlying :: !Coin
  , -- | aToken
    sc'aToken :: !TokenName
  , -- | exchange rate
    sc'exchangeRate :: !CoinRate
  }
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (FromJSON, ToJSON)

{- | Query returns the user's funds currently locked in the current Lendex,
 including both underlying tokens and aTokens of multiple kinds. Also returns
 the user's current borrow amount and advances interest.
-}
data UserBalance = UserBalance
  { -- | User Id
    ub'id :: !UserId
  , -- | Total Deposit for User,
    ub'totalDeposit :: !Integer
  , -- | Total Collateral for User,
    ub'totalCollateral :: !Integer
  , -- | Total Borrow for User,
    ub'totalBorrow :: !Integer
  , -- | Normalised Income for User,
    ub'cumulativeBalance :: Map Coin Rational
  , -- | User Funds
    ub'funds :: Map Coin Wallet
  }
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (FromJSON, ToJSON)

-- data Funds = Funds
--   { -- | Coin
--     funds'coin :: Coin
--   , -- | Deposit Balance
--     funds'deposit :: Integer
--   , -- | Collateral Balance
--     funds'collateral :: Integer
--   , -- | Borrow Balance
--     funds'borrow :: Integer
--   }
--   deriving stock (Hask.Show, Generic, Hask.Eq)
--   deriving anyclass (FromJSON, ToJSON)

-- If another query is added, extend this data type

-- | Results of query endpoints calls on `QueryContract`
data QueryRes
  = QueryResAllLendexes [(Address, LendingPool)]
  | QueryResSupportedCurrencies {getSupported :: [SupportedCurrency]}
  | QueryResCurrentBalance UserBalance
  deriving stock (Hask.Show, Generic, Hask.Eq)
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
PlutusTx.unstableMakeIsData ''QueryAct
PlutusTx.unstableMakeIsData ''User
PlutusTx.unstableMakeIsData ''Wallet
PlutusTx.unstableMakeIsData ''Reserve
PlutusTx.unstableMakeIsData ''StartParams
PlutusTx.unstableMakeIsData ''BadBorrow
PlutusTx.unstableMakeIsData ''LendingPool
PlutusTx.unstableMakeIsData ''Act
PlutusTx.unstableMakeIsData ''LendexId
PlutusTx.makeLift ''LendexId
