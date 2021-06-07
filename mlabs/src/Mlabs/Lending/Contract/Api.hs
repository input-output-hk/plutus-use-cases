-- | Contract API for Lendex application
module Mlabs.Lending.Contract.Api(
  -- * Actions
  -- ** User actions
    Deposit(..)
  , Borrow(..)
  , Repay(..)
  , SwapBorrowRateModel(..)
  , SetUserReserveAsCollateral(..)
  , Withdraw(..)
  , LiquidationCall(..)
  , InterestRateFlag(..)
  , toInterestRateFlag
  , fromInterestRateFlag
  -- ** Admin actions
  , AddReserve(..)
  , StartParams(..)
  -- ** Price oracle actions
  , SetAssetPrice(..)
  -- ** Action conversions
  , IsUserAct(..)
  , IsPriceAct(..)
  , IsGovernAct(..)
  -- * Schemas
  , UserSchema
  , OracleSchema
  , AdminSchema
) where


import qualified Prelude as Hask
import PlutusTx.Prelude

import GHC.Generics

import Plutus.Contract
import Playground.Contract
import Plutus.V1.Ledger.Crypto
import Plutus.V1.Ledger.Value

import Mlabs.Plutus.Contract
import Mlabs.Emulator.Types
import Mlabs.Data.Ray (Ray)
import Mlabs.Lending.Logic.Types

-----------------------------------------------------------------------
-- lending pool actions

-- user actions

-- | Deposit funds to app
data Deposit = Deposit
  { deposit'amount         :: Integer
  , deposit'asset          :: Coin
  }
  deriving stock (Show, Generic, Hask.Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

-- | Borrow funds. We have to allocate collateral to be able to borrow
data Borrow = Borrow
  { borrow'amount         :: Integer
  , borrow'asset          :: Coin
  , borrow'rate           :: InterestRateFlag
  }
  deriving stock (Show, Generic, Hask.Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

-- | Repay part of the borrow
data Repay = Repay
  { repay'amount          :: Integer
  , repay'asset           :: Coin
  , repay'rate            :: InterestRateFlag
  }
  deriving stock (Show, Generic, Hask.Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

-- | Swap borrow interest rate strategy (stable to variable)
data SwapBorrowRateModel = SwapBorrowRateModel
  { swapRate'asset        :: Coin
  , swapRate'rate         :: InterestRateFlag
  }
  deriving stock (Show, Generic, Hask.Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

-- | Set some portion of deposit as collateral or some portion of collateral as deposit
data SetUserReserveAsCollateral = SetUserReserveAsCollateral
  { setCollateral'asset           :: Coin       -- ^ which asset to use as collateral or not
  , setCollateral'useAsCollateral :: Bool       -- ^ should we use as collateral (True) or use as deposit (False)
  , setCollateral'portion         :: Ray        -- ^ poriton of deposit/collateral to change status (0, 1)
  }
  deriving stock (Show, Generic, Hask.Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

-- | Withdraw funds from deposit
data Withdraw = Withdraw
  { withdraw'amount         :: Integer
  , withdraw'asset          :: Coin
  }
  deriving stock (Show, Generic, Hask.Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

-- | Call to liquidate borrows that are unsafe due to health check
-- (see <https://docs.aave.com/faq/liquidations> for description)
data LiquidationCall = LiquidationCall
  { liquidationCall'collateral     :: Coin        -- ^ which collateral do we take for borrow repay
  , liquidationCall'debtUser       :: PubKeyHash  -- ^ identifier of the unhealthy borrow user
  , liquidationCall'debtAsset      :: Coin        -- ^ identifier of the unhealthy borrow asset
  , liquidationCall'debtToCover    :: Integer     -- ^ how much of the debt we cover
  , liquidationCall'receiveAToken  :: Bool        -- ^ if true, the user receives the aTokens equivalent
                                                  --   of the purchased collateral. If false, the user receives
                                                  --   the underlying asset directly.
  }
  deriving stock (Show, Generic, Hask.Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

-- deriving stock (Show, Generic, Hask.Eq)
-- deriving anyclass (FromJSON, ToJSON)

-- admin actions

-- | Adds new reserve
data AddReserve = AddReserve CoinCfg
  deriving stock (Show, Generic, Hask.Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data StartParams = StartParams
  { sp'coins     :: [CoinCfg]     -- ^ supported coins with ratios to ADA
  , sp'initValue :: Value         -- ^ init value deposited to the lending app
  , sp'admins    :: [PubKeyHash]  -- ^ admins
  , sp'oracles   :: [PubKeyHash]  -- ^ trusted oracles
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

-- price oracle actions

-- | Updates for the prices of the currencies on the markets
data SetAssetPrice = SetAssetPrice Coin Ray
  deriving stock (Show, Generic, Hask.Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

----------------------------------------------------------
-- schemas

-- | User actions
type UserSchema =
  BlockchainActions
    .\/ Call Deposit
    .\/ Call Borrow
    .\/ Call Repay
    .\/ Call SwapBorrowRateModel
    .\/ Call SetUserReserveAsCollateral
    .\/ Call Withdraw
    .\/ Call LiquidationCall


-- | Oracle schema
type OracleSchema =
  BlockchainActions
    .\/ Call SetAssetPrice

-- | Admin schema
type AdminSchema =
  BlockchainActions
    .\/ Call AddReserve
    .\/ Call StartParams

----------------------------------------------------------
-- proxy types for ToSchema instance

-- | Interest rate flag.
--
-- * 0 is stable rate
-- * everything else is variable rate
newtype InterestRateFlag = InterestRateFlag Integer
  deriving newtype (Show, Hask.Eq, FromJSON, ToJSON, ToSchema)

fromInterestRateFlag :: InterestRateFlag -> InterestRate
fromInterestRateFlag (InterestRateFlag n)
  | n == 0    = StableRate
  | otherwise = VariableRate

toInterestRateFlag :: InterestRate -> InterestRateFlag
toInterestRateFlag = InterestRateFlag . \case
  StableRate   -> 0
  VariableRate -> 1

----------------------------------------------------------
-- boilerplate to logic-act coversions

class IsEndpoint a => IsUserAct a where
  toUserAct :: a -> UserAct

class IsEndpoint a => IsPriceAct a where
  toPriceAct :: a -> PriceAct

class IsEndpoint a => IsGovernAct a where
  toGovernAct :: a -> GovernAct

-- user acts

instance IsUserAct Deposit                    where { toUserAct Deposit{..} = DepositAct deposit'amount deposit'asset }
instance IsUserAct Borrow                     where { toUserAct Borrow{..} = BorrowAct borrow'amount borrow'asset (fromInterestRateFlag borrow'rate) }
instance IsUserAct Repay                      where { toUserAct Repay{..} = RepayAct repay'amount repay'asset (fromInterestRateFlag repay'rate) }
instance IsUserAct SwapBorrowRateModel        where { toUserAct SwapBorrowRateModel{..} = SwapBorrowRateModelAct swapRate'asset (fromInterestRateFlag swapRate'rate) }
instance IsUserAct SetUserReserveAsCollateral where { toUserAct SetUserReserveAsCollateral{..} = SetUserReserveAsCollateralAct setCollateral'asset setCollateral'useAsCollateral setCollateral'portion }
instance IsUserAct Withdraw                   where { toUserAct Withdraw{..} = WithdrawAct withdraw'amount withdraw'asset }
instance IsUserAct LiquidationCall            where { toUserAct LiquidationCall{..} = LiquidationCallAct liquidationCall'collateral (BadBorrow (UserId liquidationCall'debtUser) liquidationCall'debtAsset) liquidationCall'debtToCover liquidationCall'receiveAToken }

-- price acts

instance IsPriceAct SetAssetPrice             where { toPriceAct (SetAssetPrice asset rate) = SetAssetPriceAct asset rate }

-- govern acts

instance IsGovernAct AddReserve               where { toGovernAct (AddReserve cfg) = AddReserveAct cfg }

-- endpoint names

instance IsEndpoint Deposit where
  type EndpointSymbol Deposit = "deposit"

instance IsEndpoint Borrow where
  type EndpointSymbol Borrow = "borrow"

instance IsEndpoint Repay where
  type EndpointSymbol Repay = "repay"

instance IsEndpoint SwapBorrowRateModel where
  type EndpointSymbol SwapBorrowRateModel = "swap-borrow-rate-model"

instance IsEndpoint SetUserReserveAsCollateral where
  type EndpointSymbol SetUserReserveAsCollateral = "set-user-reserve-as-collateral"

instance IsEndpoint Withdraw where
  type EndpointSymbol Withdraw = "withdraw"

instance IsEndpoint LiquidationCall where
  type EndpointSymbol LiquidationCall = "liquidation-call"

instance IsEndpoint SetAssetPrice where
  type EndpointSymbol SetAssetPrice = "set-asset-price"

instance IsEndpoint AddReserve where
  type EndpointSymbol AddReserve = "add-reserve"

instance IsEndpoint StartParams where
  type EndpointSymbol StartParams = "start-lendex"

