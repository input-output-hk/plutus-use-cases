{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | Contract API for Lendex application
module Mlabs.Lending.Contract.Api(
  -- * Actions
  -- ** User actions
    Deposit(..)
  , Borrow(..)
  , Repay(..)
  , SwapBorrowRateModel(..)
  , AddCollateral(..)
  , RemoveCollateral(..)
  , Withdraw(..)
  , LiquidationCall(..)
  , InterestRateFlag(..)
  , toInterestRateFlag
  , fromInterestRateFlag
  -- ** Admin actions
  , AddReserve(..)
  , StartLendex(..)
  -- ** Query actions
  , QueryAllLendexes(..)
  , QuerySupportedCurrencies(..)
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
  , QuerySchema
) where


import PlutusTx.Prelude

import GHC.Generics (Generic)
import Playground.Contract (FromJSON, ToJSON, ToSchema)
import Plutus.Contract ( type (.\/) )
import Plutus.V1.Ledger.Crypto (PubKeyHash)
import Prelude qualified as Hask (Show, Eq)

import Mlabs.Lending.Logic.Types qualified as Types
import Mlabs.Plutus.Contract ( Call, IsEndpoint(..) )

-----------------------------------------------------------------------
-- lending pool actions

-- user actions

-- | Deposit funds to app
data Deposit = Deposit
  { deposit'amount         :: Integer
  , deposit'asset          :: Types.Coin
  }
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

-- | Borrow funds. We have to allocate collateral to be able to borrow
data Borrow = Borrow
  { borrow'amount         :: Integer
  , borrow'asset          :: Types.Coin
  , borrow'rate           :: InterestRateFlag
  }
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

-- | Repay part of the borrow
data Repay = Repay
  { repay'amount          :: Integer
  , repay'asset           :: Types.Coin
  , repay'rate            :: InterestRateFlag
  }
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

-- | Swap borrow interest rate strategy (stable to variable)
data SwapBorrowRateModel = SwapBorrowRateModel
  { swapRate'asset        :: Types.Coin
  , swapRate'rate         :: InterestRateFlag
  }
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

-- | Transfer portion of asset from the user's wallet to the contract, locked as the user's Collateral
data AddCollateral = AddCollateral
  { addCollateral'asset           :: Types.Coin  -- ^ which Asset to use as collateral
  , addCollateral'amount         :: Integer      -- ^ amount of Asset to take from Wallet and use as Collateral
  }
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

-- | Transfer portion of asset from locked user's Collateral to user's wallet
data RemoveCollateral = RemoveCollateral
  { removeCollateral'asset           :: Types.Coin  -- ^ which Asset to use as collateral or not
  , removeCollateral'amount         :: Integer      -- ^ amount of Asset to remove from Collateral and put back to Wallet
  }
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

-- | Withdraw funds from deposit
data Withdraw = Withdraw
  { withdraw'amount         :: Integer
  , withdraw'asset          :: Types.Coin
  }
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

-- | Call to liquidate borrows that are unsafe due to health check
-- (see <https://docs.aave.com/faq/liquidations> for description)
data LiquidationCall = LiquidationCall
  { liquidationCall'collateral     :: Types.Coin  -- ^ which collateral do we take for borrow repay
  , liquidationCall'debtUser       :: PubKeyHash  -- ^ identifier of the unhealthy borrow user
  , liquidationCall'debtAsset      :: Types.Coin  -- ^ identifier of the unhealthy borrow asset
  , liquidationCall'debtToCover    :: Integer     -- ^ how much of the debt we cover
  , liquidationCall'receiveAToken  :: Bool        -- ^ if true, the user receives the aTokens equivalent
                                                  --   of the purchased collateral. If false, the user receives
                                                  --   the underlying asset directly.
  }
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

-- deriving stock (Show, Generic, Hask.Eq)
-- deriving anyclass (FromJSON, ToJSON)

-- admin actions

-- | Adds new reserve
newtype AddReserve = AddReserve Types.CoinCfg
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

newtype StartLendex = StartLendex Types.StartParams 
  deriving newtype (Hask.Show, Generic, Hask.Eq, FromJSON, ToJSON, ToSchema)

-- query actions

newtype QueryAllLendexes = QueryAllLendexes Types.StartParams
  deriving newtype (Hask.Show, Generic, Hask.Eq, FromJSON, ToJSON, ToSchema)

newtype QuerySupportedCurrencies = QuerySupportedCurrencies ()
  deriving stock (Hask.Show, Generic)
  deriving newtype (FromJSON, ToJSON, ToSchema)

-- price oracle actions

-- | Updates for the prices of the currencies on the markets
data SetAssetPrice = SetAssetPrice Types.Coin Rational
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

----------------------------------------------------------
-- schemas

-- | User actions
type UserSchema =
  Call Deposit
  .\/ Call Borrow
  .\/ Call Repay
  .\/ Call SwapBorrowRateModel
  -- .\/ Call SetUserReserveAsCollateral
  .\/ Call AddCollateral
  .\/ Call RemoveCollateral
  .\/ Call Withdraw
  .\/ Call LiquidationCall


-- | Oracle schema
type OracleSchema =
  Call SetAssetPrice

-- | Admin schema
type AdminSchema =
  Call AddReserve
  .\/ Call StartLendex

-- | Query schema
type QuerySchema =
  Call QueryAllLendexes
  .\/ Call QuerySupportedCurrencies

----------------------------------------------------------
-- proxy types for ToSchema instance

-- | Interest rate flag.
--
-- * 0 is stable rate
-- * everything else is variable rate
newtype InterestRateFlag = InterestRateFlag Integer
  deriving newtype (Hask.Show, Hask.Eq, FromJSON, ToJSON, ToSchema)

fromInterestRateFlag :: InterestRateFlag -> Types.InterestRate
fromInterestRateFlag (InterestRateFlag n)
  | n == 0    = Types.StableRate
  | otherwise = Types.VariableRate

toInterestRateFlag :: Types.InterestRate -> InterestRateFlag
toInterestRateFlag = InterestRateFlag . \case
  Types.StableRate   -> 0
  Types.VariableRate -> 1

----------------------------------------------------------
-- boilerplate to logic-act conversions

class IsEndpoint a => IsUserAct a where
  toUserAct :: a -> Types.UserAct

class IsEndpoint a => IsPriceAct a where
  toPriceAct :: a -> Types.PriceAct

class IsEndpoint a => IsGovernAct a where
  toGovernAct :: a -> Types.GovernAct

-- user acts

instance IsUserAct Deposit                    where { toUserAct Deposit{..} = Types.DepositAct deposit'amount deposit'asset }
instance IsUserAct Borrow                     where { toUserAct Borrow{..} = Types.BorrowAct borrow'amount borrow'asset (fromInterestRateFlag borrow'rate) }
instance IsUserAct Repay                      where { toUserAct Repay{..} = Types.RepayAct repay'amount repay'asset (fromInterestRateFlag repay'rate) }
instance IsUserAct SwapBorrowRateModel        where { toUserAct SwapBorrowRateModel{..} = Types.SwapBorrowRateModelAct swapRate'asset (fromInterestRateFlag swapRate'rate) }
instance IsUserAct AddCollateral              where { toUserAct AddCollateral{..} = Types.AddCollateralAct addCollateral'asset addCollateral'amount }
instance IsUserAct RemoveCollateral           where { toUserAct RemoveCollateral{..} = Types.RemoveCollateralAct removeCollateral'asset removeCollateral'amount }
instance IsUserAct Withdraw                   where { toUserAct Withdraw{..} = Types.WithdrawAct withdraw'asset withdraw'amount }
instance IsUserAct LiquidationCall            where { toUserAct LiquidationCall{..} = Types.LiquidationCallAct liquidationCall'collateral (Types.BadBorrow (Types.UserId liquidationCall'debtUser) liquidationCall'debtAsset) liquidationCall'debtToCover liquidationCall'receiveAToken }

-- price acts

instance IsPriceAct SetAssetPrice             where { toPriceAct (SetAssetPrice asset rate) = Types.SetAssetPriceAct asset rate }

-- govern acts

instance IsGovernAct AddReserve               where { toGovernAct (AddReserve cfg) = Types.AddReserveAct cfg }

-- endpoint names

instance IsEndpoint Deposit where
  type EndpointSymbol Deposit = "deposit"

instance IsEndpoint Borrow where
  type EndpointSymbol Borrow = "borrow"

instance IsEndpoint Repay where
  type EndpointSymbol Repay = "repay"

instance IsEndpoint SwapBorrowRateModel where
  type EndpointSymbol SwapBorrowRateModel = "swap-borrow-rate-model"

instance IsEndpoint AddCollateral where
  type EndpointSymbol AddCollateral = "add-collateral"

instance IsEndpoint RemoveCollateral where
  type EndpointSymbol RemoveCollateral = "remove-collateral"

instance IsEndpoint Withdraw where
  type EndpointSymbol Withdraw = "withdraw"

instance IsEndpoint LiquidationCall where
  type EndpointSymbol LiquidationCall = "liquidation-call"

instance IsEndpoint SetAssetPrice where
  type EndpointSymbol SetAssetPrice = "set-asset-price"

instance IsEndpoint AddReserve where
  type EndpointSymbol AddReserve = "add-reserve"

instance IsEndpoint StartLendex where
  type EndpointSymbol StartLendex = "start-lendex"

instance IsEndpoint QueryAllLendexes where
  type EndpointSymbol QueryAllLendexes = "query-all-lendexes"

instance IsEndpoint QuerySupportedCurrencies where
  type EndpointSymbol QuerySupportedCurrencies = "query-supported-currencies"
