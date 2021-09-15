{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
-- {-# LANGUAGE StrictData                 #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:profile-all #-}
module Dex.Types
  where

import           Data.Aeson          (FromJSON (parseJSON), ToJSON)

import           Data.Text           (Text)
import           Dex.WalletHistory
import           Ledger              (AssetClass, PubKeyHash, TxOutRef)
import           Ledger.Value        (Value, assetClassValue)
import           Playground.Contract (Generic, ToSchema)
import qualified PlutusTx
import           PlutusTx.Prelude    (AdditiveGroup, AdditiveMonoid,
                                      AdditiveSemigroup, BuiltinByteString, Eq,
                                      Integer, MultiplicativeMonoid,
                                      MultiplicativeSemigroup, Ord, return, ($),
                                      (&&), (<), (==))
import           Prelude             (Double, Show, fromIntegral, (/))
import qualified Prelude

newtype Nat
  = Nat Integer
  deriving stock (Generic)
  deriving newtype
    ( AdditiveGroup
    , AdditiveMonoid
    , AdditiveSemigroup
    , Eq
    , MultiplicativeMonoid
    , MultiplicativeSemigroup
    , Ord
    , Prelude.Enum
    , Prelude.Eq
    , Prelude.Integral
    , Prelude.Num
    , Prelude.Ord
    , Prelude.Real
    , Show
    , ToJSON
    , ToSchema
    )

fromNat :: Nat -> Integer
fromNat (Nat x) = x

PlutusTx.makeIsDataIndexed ''Nat [('Nat,0)]
PlutusTx.makeLift ''Nat


instance FromJSON Nat where
  parseJSON value = do
    integer <- parseJSON @Integer value
    if integer < 0 then
        Prelude.fail "parsing Natural failed, unexpected negative number "
    else
        return $ Nat integer

toDouble :: (Nat,Nat) -> Double
toDouble (a,b) = fromIntegral a / fromIntegral b

singleton :: AssetClass -> Nat -> Value
singleton assetClass amount = assetClassValue assetClass (fromNat amount)

data DexAction = Swap | CancelOrder | CollectCoins deriving (Show)

PlutusTx.makeIsDataIndexed
  ''DexAction
  [ ('Swap, 0)
  , ('CancelOrder, 1)
  , ('CollectCoins, 2)
  ]
PlutusTx.makeLift ''DexAction


data SellOrderParams
  = SellOrderParams
      { lockedCoin     :: !AssetClass
      , expectedCoin   :: !AssetClass
      , lockedAmount   :: !Nat
      , expectedAmount :: !Nat
      }
  deriving (FromJSON, Generic, Show, ToJSON, ToSchema)

PlutusTx.unstableMakeIsData ''SellOrderParams
PlutusTx.makeLift ''SellOrderParams

data LiquidityOrderParams
  = LiquidityOrderParams
      { lockedCoin     :: AssetClass
      , expectedCoin   :: AssetClass
      , lockedAmount   :: Nat
      , expectedAmount :: Nat
      , swapFee        :: (Nat, Nat)
      }
  deriving (FromJSON, Generic, Show, ToJSON, ToSchema)

PlutusTx.unstableMakeIsData ''LiquidityOrderParams
PlutusTx.makeLift ''LiquidityOrderParams


data PoolPartsParams
  = PriceChangeParams
      { coinAPriceChange :: (Nat, Nat)
      , coinBPriceChange :: (Nat, Nat)
      , numberOfParts    :: Nat
      }
  deriving (FromJSON, Generic, Show, ToJSON, ToSchema)
PlutusTx.unstableMakeIsData ''PoolPartsParams
PlutusTx.makeLift ''PoolPartsParams


data LiquidityPoolParams
  = LiquidityPoolParams
      { coinA           :: AssetClass
      , coinB           :: AssetClass
      , amountA         :: Nat
      , poolPartsParams :: PoolPartsParams
      , swapFee         :: (Nat, Nat)
      , exchangeRate    :: (Nat, Nat)
      }
  deriving (FromJSON, Generic, Show, ToJSON, ToSchema)

PlutusTx.unstableMakeIsData ''LiquidityPoolParams
PlutusTx.makeLift ''LiquidityPoolParams

data SellOrderInfo
  = SellOrderInfo
      { lockedCoin     :: AssetClass
      , expectedCoin   :: AssetClass
      , expectedAmount :: Nat
      , ownerHash      :: PubKeyHash
      , orderId        :: BuiltinByteString
      }
  deriving (FromJSON, Generic, Show, ToJSON)
PlutusTx.makeIsDataIndexed ''SellOrderInfo [('SellOrderInfo,0)]
PlutusTx.makeLift ''SellOrderInfo

newtype CancelOrderParams
  = CancelOrderParams { orderHash :: TxOutRef }
  deriving (Generic, Show)
  deriving anyclass (FromJSON, ToJSON, ToSchema)
PlutusTx.makeIsDataIndexed ''CancelOrderParams [('CancelOrderParams, 0)]
PlutusTx.makeLift ''CancelOrderParams

data PayoutInfo
  = PayoutInfo
      { ownerHash :: PubKeyHash
      , orderId   :: BuiltinByteString
      }
  deriving (FromJSON, Generic, Show, ToJSON)
PlutusTx.makeIsDataIndexed ''PayoutInfo [('PayoutInfo,0)]
PlutusTx.makeLift ''PayoutInfo

data LiquidityOrderInfo
  = LiquidityOrderInfo
      { lockedCoin     :: AssetClass
      , expectedCoin   :: AssetClass
      , expectedAmount :: Nat
      , swapFee        :: (Nat, Nat)
      , ownerHash      :: PubKeyHash
      , orderId        :: BuiltinByteString
      }
  deriving (FromJSON, Generic, Show, ToJSON)

instance Eq LiquidityOrderInfo where
  {-# INLINEABLE (==) #-}
  (LiquidityOrderInfo lC eC eA sF oH oId) == (LiquidityOrderInfo lC' eC' eA' sF' oH' oId') =
    lC == lC' && eC == eC' && eA == eA' && sF == sF' && oH == oH' && oId == oId'


{-# INLINEABLE reversedLiquidityOrder #-}
reversedLiquidityOrder :: Integer -> LiquidityOrderInfo -> LiquidityOrderInfo
reversedLiquidityOrder liquidity LiquidityOrderInfo {..} =
  LiquidityOrderInfo
  { lockedCoin = expectedCoin
  , expectedCoin = lockedCoin
  , expectedAmount = Nat liquidity
  , swapFee = swapFee
  , ownerHash = ownerHash
  , orderId = orderId
  }

PlutusTx.unstableMakeIsData ''LiquidityOrderInfo
PlutusTx.makeLift ''LiquidityOrderInfo


data Order
  = SellOrder SellOrderInfo
  | LiquidityOrder LiquidityOrderInfo
  deriving (FromJSON, Generic, Show, ToJSON)

PlutusTx.makeIsDataIndexed
 ''Order
 [ ('SellOrder, 0)
 , ('LiquidityOrder, 1)
 ]
PlutusTx.makeLift ''Order

data DexDatum
  = Order Order
  | Payout PayoutInfo
  deriving (FromJSON, Generic, Show, ToJSON)


PlutusTx.makeIsDataIndexed
  ''DexDatum
  [ ('Order, 0)
  , ('Payout, 1)
  ]
PlutusTx.makeLift ''DexDatum


data Request a
  = Request
      { historyId  :: HistoryId
      , randomSeed :: Integer
      , content    :: a
      }
  deriving (FromJSON, Generic, Show, ToJSON, ToSchema)


data OrderInfo
  = OrderInfo
      { orderHash      :: TxOutRef
      , lockedCoin     :: AssetClass
      , expectedCoin   :: AssetClass
      , expectedAmount :: Nat
      , lockedAmount   :: Nat
      , ownerHash      :: PubKeyHash
      , orderType      :: Text
      }
  deriving (FromJSON, Generic, Show, ToJSON)

newtype PayoutSummary
  = PayoutSummary
      { payoutValue :: [(AssetClass, Integer)]
      }
  deriving newtype (FromJSON, ToJSON)
  deriving stock (Generic, Show)

data DexContractState
  = AllOrders [OrderInfo]
  | OrderCreated
  | PoolCreated
  | Performed
  | Stopped
  | Funds [(AssetClass, Integer)]
  | MyOrders [OrderInfo]
  | Canceled
  | Collected
  | MyPayouts PayoutSummary
  deriving (FromJSON, Generic, Show, ToJSON)
