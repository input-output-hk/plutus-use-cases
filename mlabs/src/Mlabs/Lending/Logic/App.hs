-- | Lending app emulator
module Mlabs.Lending.Logic.App(
    App(..)
  , runApp
  , AppConfig(..)
  , defaultAppConfig
  , lookupAppWallet
) where

import Prelude

import Control.Monad.State.Strict
import Control.Arrow (second)

import Data.List (foldl')

import Mlabs.Lending.Logic.Emulator
import Mlabs.Lending.Logic.React
import Mlabs.Lending.Logic.Types
import Mlabs.Lending.Logic.State

import qualified Data.Map.Strict as M

-- | Prototype application
data App = App
  { app'pool    :: !LendingPool  -- ^ lending pool
  , app'log     :: ![Error]      -- ^ error log
  , app'wallets :: !BchState     -- ^ current state of blockchain
  }

-- | Lookup state of the blockchain-wallet for a given user-id.
lookupAppWallet :: UserId -> App -> Maybe BchWallet
lookupAppWallet uid App{..} = case app'wallets of
  BchState wals -> M.lookup uid wals

-- | Runs application with the list of actions.
-- Returns final state of the application.
runApp :: AppConfig -> [Act] -> App
runApp cfg acts = foldl' go (initApp cfg) acts
  where
    -- There are two possible sources of errors:
    --   * we can not make transition to state (react produces Left)
    --   * the transition produces action on blockchain that leads to negative balances (applyResp produces Left)
    go (App lp errs wallets) act = case runStateT (react act) lp of
      Right (resp, nextState) -> case foldM (flip applyResp) wallets resp of
        Right nextWallets -> App nextState errs nextWallets
        Left err          -> App lp (err : errs) wallets
      Left err                -> App lp (err : errs) wallets

-- Configuration paprameters for app.
data AppConfig = AppConfig
  { appConfig'reserves :: [(Coin, Rational)]
  -- ^ coins with ratios to base currencies for each reserve
  , appConfig'users    :: [(UserId, BchWallet)]
  -- ^ initial set of users with their wallets on blockchain
  -- the wallet for lending app wil be created automatically.
  -- no need to include it here
  }

-- | App is initialised with list of coins and their rates (value relative to base currency, ada for us)
initApp :: AppConfig -> App
initApp AppConfig{..} = App
  { app'pool = LendingPool (M.fromList (fmap (second initReserve) appConfig'reserves)) mempty
  , app'log  = []
  , app'wallets = BchState $ M.fromList $ (Self, defaultBchWallet) : appConfig'users
  }

-- | Default application.
-- It allocates three users nad three reserves for Dollars, Euros and Liras.
-- Each user has 100 units of only one currency. User 1 has dollars, user 2 has euros amd user 3 has liras.
defaultAppConfig :: AppConfig
defaultAppConfig = AppConfig reserves users
  where
    reserves = fmap (, 1) [coin1, coin2, coin3]

    coin1 = Coin "Dollar"
    coin2 = Coin "Euro"
    coin3 = Coin "Lira"

    users = [user1, user2, user3]

    user1 = (UserId 1, wal (coin1, 100))
    user2 = (UserId 2, wal (coin2, 100))
    user3 = (UserId 3, wal (coin3, 100))

    wal cs = BchWallet $ uncurry M.singleton cs

