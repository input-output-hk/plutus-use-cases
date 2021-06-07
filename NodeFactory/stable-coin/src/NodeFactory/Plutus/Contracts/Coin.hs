{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module NodeFactory.Plutus.Contracts.Coin
    (
        Coin (..),
        coin,
        coinValueOf,
        hashCoin,
        coinAssetClass
    ) where

import           Control.Monad                    hiding (fmap)
import qualified Data.Map                         as Map
import           Data.Monoid                      (Last (..))
import           Data.Proxy                       (Proxy (..))
import           Data.Text                        (Text, pack)
import           Data.Void                        (Void)
import           Ledger                           hiding (singleton)
import           Ledger.Contexts
import           Ledger.Constraints               as Constraints
import           Ledger.Constraints.OnChain       as Constraints
import           Ledger.Constraints.TxConstraints as Constraints
import qualified Ledger.Scripts                   as Scripts
import qualified Ledger.Typed.Scripts             as Scripts
import           Ledger.Value                     as Value
import           Playground.Contract
import           Plutus.Contract                  hiding (when)
import qualified Plutus.Contracts.Currency        as Currency
import qualified PlutusTx
import           PlutusTx.Prelude                 hiding (Semigroup (..), unless)
import           Prelude                          (Semigroup (..))
import qualified Prelude
import           Text.Printf                      (printf)


-- | A pair consisting of a 'CurrencySymbol' and a 'TokenName'.
-- Coins are the entities that can be swapped in the exchange.
data Coin = Coin
    { cCurrency :: CurrencySymbol
    , cToken    :: TokenName
    } deriving (Show, Generic, ToJSON, FromJSON)

PlutusTx.unstableMakeIsData ''Coin
PlutusTx.makeLift ''Coin

instance Eq Coin where
    {-# INLINABLE (==) #-}
    c == d = cCurrency c == cCurrency d && cToken c == cToken d

instance Prelude.Eq Coin where
    (==) = (==)

{-# INLINABLE compareCoins #-}
compareCoins :: Coin -> Coin -> Ordering
compareCoins c d = case compare (cCurrency c) (cCurrency d) of
    LT -> LT
    GT -> GT
    EQ -> compare (cToken c) (cToken d)

instance Prelude.Ord Coin where
    compare = compareCoins

{-# INLINABLE coinLT #-}
coinLT :: Coin -> Coin -> Bool
coinLT c d = case compareCoins c d of
    LT -> True
    _  -> False

{-# INLINABLE coin #-}
-- | @'coin' c n@ denotes the value given by @n@ units of @'Coin'@ @c@.
coin :: Coin    -- ^ The 'Coin'.
     -> Integer -- ^ The desired number coins.
     -> Value   -- ^ The 'Value' consisting of the given number of units of the given 'Coin'.
coin Coin{..} = Value.singleton cCurrency cToken

{-# INLINABLE coinValueOf #-}
-- | Calculates how many units of the specified 'Coin' are contained in the
-- given 'Value'.
coinValueOf :: Value   -- ^ The 'Value' to inspect.
            -> Coin    -- ^ The 'Coin' to look for.
            -> Integer -- ^ The number of units of the given 'Coin' contained in the given 'Value'.
coinValueOf v Coin{..} = valueOf v cCurrency cToken

{-# INLINABLE hashCoin #-}
hashCoin :: Coin -> ByteString
hashCoin Coin{..} = sha2_256 $ concatenate (unCurrencySymbol cCurrency) (unTokenName cToken)

{-# INLINABLE coinAssetClass #-}
coinAssetClass :: Coin -> AssetClass
coinAssetClass Coin{..} = AssetClass (cCurrency, cToken)