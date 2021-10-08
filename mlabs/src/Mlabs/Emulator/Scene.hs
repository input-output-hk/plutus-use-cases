{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Set of balances for tests
module Mlabs.Emulator.Scene (
  Scene (..),
  owns,
  appOwns,
  appAddress,
  checkScene,
  coinDiff,
) where

import Prelude (Semigroup, Monoid, mempty, (<>))
import PlutusTx.Prelude hiding (Semigroup, Monoid, mempty,(<>))


import Control.Applicative (Alternative (..))

import Data.List qualified as L
import Data.Map qualified as M
import Plutus.Contract.Test
    ( Wallet,
      (.&&.),
      assertNoFailedTransactions,
      valueAtAddress,
      walletFundsChange,
      TracePredicate )
import Plutus.V1.Ledger.Address (Address)
import Plutus.V1.Ledger.Value (Value)
import Plutus.V1.Ledger.Value qualified as Value

import Mlabs.Lending.Logic.Types (Coin)

{- | Scene is users with balances and value that is owned by application script.
 It can be built with Monoid instance from parts with handy functions:

 'owns', 'apOwns', 'appAddress'

 With monoid instance we can specify only differences between test stages
 and then add them app with @<>@ to the initial state of the scene.
-}
data Scene = Scene
  { -- | user balances
    scene'users :: M.Map Wallet Value
  , -- | application script balance
    scene'appValue :: Value
  , -- | address of the app
    scene'appAddress :: Maybe Address
  }

instance Semigroup Scene where
  Scene us1 e1 maddr1 <> Scene us2 e2 maddr2 =
    Scene (M.unionWith (<>) us1 us2) (e1 <> e2) (maddr1 <|> maddr2)

instance Monoid Scene where
  mempty = Scene mempty mempty Nothing

-- | Creates scene with single user in it that owns so many coins, app owns zero coins.
owns :: Wallet -> [(Coin, Integer)] -> Scene
owns wal ds = Scene {scene'users = M.singleton wal (coinDiff ds), scene'appValue = mempty, scene'appAddress = Nothing}

-- | Creates scene with no users and app owns given amount of coins.
appOwns :: [(Coin, Integer)] -> Scene
appOwns v = Scene {scene'users = mempty, scene'appValue = coinDiff v, scene'appAddress = Nothing}

-- | Creates scene with no users and app owns given amount of coins.
appAddress :: Address -> Scene
appAddress addr = Scene {scene'users = mempty, scene'appValue = mempty, scene'appAddress = Just addr}

-- | Turns scene to plutus checks. Every user ownership turns into 'walletFundsChange' check.
checkScene :: Scene -> TracePredicate
checkScene Scene {..} =
  withAddressCheck $
    concatPredicates
      (uncurry walletFundsChange <$> M.toList scene'users)
      .&&. assertNoFailedTransactions
  where
    withAddressCheck = maybe id (\addr -> (valueAtAddress addr (== scene'appValue) .&&.)) scene'appAddress

-- | Converts list of coins to value.
coinDiff :: [(Coin, Integer)] -> Value
coinDiff = foldMap (uncurry Value.assetClassValue)

concatPredicates :: [TracePredicate] -> TracePredicate
concatPredicates = L.foldl1' (.&&.)
