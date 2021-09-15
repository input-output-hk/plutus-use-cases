{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
-- |

module Middleware.Dex.Types where

import           Data.Aeson.Types (FromJSON, ToJSON, toJSON)
import           Data.Text        (Text)
import           Dex.Types        (CancelOrderParams (CancelOrderParams),
                                   OrderInfo (..), PayoutSummary, fromNat)
import           GHC.Generics     (Generic)
import           Ledger           (AssetClass, CurrencySymbol, TokenName,
                                   TxOutRef)
import           Ledger.Value     (assetClass, unAssetClass)

newtype Error = Error
  { errorMessage :: Text
  } deriving (Generic, ToJSON)

-- | Data type to represent wallet founds.
data FundView = FundView
  { coin   :: Coin
  , amount :: Integer
  } deriving (Show, Generic, ToJSON)

mkFundView :: AssetClass -> Integer -> FundView
mkFundView = FundView . coinFromAssetClass

data Coin = Coin
  { currencySymbol :: CurrencySymbol
  , tokenName      :: TokenName
  } deriving (Show, Generic, FromJSON)

data CreateSellOrderParams
  = CreateSellOrderParams
      { lockedCoin     :: Coin
      , expectedCoin   :: Coin
      , lockedAmount   :: Integer
      , expectedAmount :: Integer
      }
  deriving (Show, Generic, ToJSON, FromJSON)

coinFromAssetClass :: AssetClass -> Coin
coinFromAssetClass = uncurry Coin . unAssetClass

data CreateLiquidityPoolParams
  = CreateLiquidityPoolParams
      { coinA           :: Coin
      , coinB           :: Coin
      , amountA         :: Integer
      , poolPartsParams :: PoolPartsParams
      , swapFee         :: (Integer, Integer)
      , exchangeRate    :: (Integer, Integer)
      }
  deriving (Generic, FromJSON, ToJSON, Show)

data CreateLiquidityOrderParams
  = CreateLiquidityOrderParams
      { lockedCoin     :: Coin
      , expectedCoin   :: Coin
      , lockedAmount   :: Integer
      , expectedAmount :: Integer
      , swapFee        :: (Integer, Integer)
      }
  deriving (Generic, FromJSON, ToJSON, Show)

data PoolPartsParams
  = PriceChangeParams
      { coinAPriceChange :: (Integer, Integer)
      , coinBPriceChange :: (Integer, Integer)
      , numberOfParts    :: Integer
      }
  deriving (FromJSON, Generic, Show, ToJSON)

instance ToJSON Coin where
 toJSON (Coin cs tn) = toJSON $ assetClass cs tn

data DexOrder
  = DexOrder
      { orderHash    :: TxOutRef
      , lockedCoin   :: FundView
      , expectedCoin :: FundView
      , orderType    :: Text
      }
  deriving (Generic, Show, ToJSON)

dexOrder :: OrderInfo -> DexOrder
dexOrder OrderInfo { orderHash      = oh
                   , lockedCoin     = lc
                   , lockedAmount   = la
                   , expectedCoin   = ec
                   , expectedAmount = ea
                   , orderType      = ot } =
    DexOrder oh (fv lc la) (fv ec ea) ot
  where
    fv coin amount = mkFundView coin (fromNat amount)

newtype MidCancelOrder = MidCancelOrder TxOutRef
  deriving (FromJSON, Generic, Show, ToJSON)

toCancelOrderParams :: MidCancelOrder -> CancelOrderParams
toCancelOrderParams (MidCancelOrder ref) = CancelOrderParams ref

data PayoutView = PayoutView
   { coin   :: Coin
   , amount :: Integer
   } deriving (Show, Generic, ToJSON)

mkPayoutView :: AssetClass -> Integer -> PayoutView
mkPayoutView = PayoutView . coinFromAssetClass
