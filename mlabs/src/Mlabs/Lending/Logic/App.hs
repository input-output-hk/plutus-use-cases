-- | Lending app emulator
module Mlabs.Lending.Logic.App(
    App(..)
  , runApp
  , AppConfig(..)
  , defaultAppConfig
  , lookupAppWallet
  , toCoin
) where

import PlutusTx.Prelude
import Plutus.V1.Ledger.Crypto (PubKeyHash(..))
import Plutus.V1.Ledger.Value

import Control.Monad.State.Strict hiding (Functor(..))

import Data.List (foldl')

import Mlabs.Lending.Logic.Emulator
import Mlabs.Lending.Logic.React
import Mlabs.Lending.Logic.Types
import Mlabs.Lending.Logic.State

import qualified Data.Map.Strict as M
import qualified PlutusTx.AssocMap as AM
import qualified PlutusTx.Ratio as R

-- | Prototype application
data App = App
  { app'pool    :: !LendingPool                  -- ^ lending pool
  , app'log     :: ![(Act, LendingPool, Error)]  -- ^ error log
                                                 -- ^ it reports on which act and pool state error has happened
  , app'wallets :: !BchState                     -- ^ current state of blockchain
  }

-- | Lookup state of the blockchain-wallet for a given user-id.
lookupAppWallet :: UserId -> App -> Maybe BchWallet
lookupAppWallet uid App{..} = case app'wallets of
  BchState wals -> M.lookup uid wals

-- | Runs application with the list of actions.
-- Returns final state of the application.
runApp :: AppConfig -> [Act] -> App
runApp cfg acts = foldl' go (initApp cfg) $ zip [0..] acts
  where
    -- There are two possible sources of errors:
    --   * we can not make transition to state (react produces Left)
    --   * the transition produces action on blockchain that leads to negative balances (applyResp produces Left)
    go (App lp errs wallets) (timestamp, act) = case runStateT (react timestamp act) lp of
      Right (resp, nextState) -> case foldM (flip applyResp) wallets resp of
        Right nextWallets -> App nextState errs nextWallets
        Left err          -> App lp ((act, lp, err) : errs) wallets
      Left err                -> App lp ((act, lp, err) : errs) wallets

-- Configuration paprameters for app.
data AppConfig = AppConfig
  { appConfig'reserves :: [CoinCfg]
  -- ^ coins with ratios to base currencies for each reserve
  , appConfig'users    :: [(UserId, BchWallet)]
  -- ^ initial set of users with their wallets on blockchain
  -- the wallet for lending app wil be created automatically.
  -- no need to include it here
  , appConfig'currencySymbol :: CurrencySymbol
  -- ^ lending app main currency symbol
  }

-- | App is initialised with list of coins and their rates (value relative to base currency, ada for us)
initApp :: AppConfig -> App
initApp AppConfig{..} = App
  { app'pool = LendingPool (AM.fromList (fmap (\x -> (coinCfg'coin x, initReserve x)) appConfig'reserves)) AM.empty appConfig'currencySymbol coinMap
  , app'log  = []
  , app'wallets = BchState $ M.fromList $ (Self, defaultBchWallet) : appConfig'users
  }
  where
    coinMap = AM.fromList $ fmap (\CoinCfg{..} -> (coinCfg'aToken, coinCfg'coin)) $ appConfig'reserves

-- | Default application.
-- It allocates three users nad three reserves for Dollars, Euros and Liras.
-- Each user has 100 units of only one currency. User 1 has dollars, user 2 has euros amd user 3 has liras.
defaultAppConfig :: AppConfig
defaultAppConfig = AppConfig reserves users curSym
  where
    curSym = currencySymbol "lending-app"
    userNames = ["1", "2", "3"]
    coinNames = ["Dollar", "Euro", "Lira"]

    reserves = fmap (\name -> CoinCfg (toCoin name) (R.fromInteger 1) (toAToken name) defaultInterestModel)  coinNames

    users = zipWith (\coinName userName -> (UserId (PubKeyHash userName), wal (toCoin coinName, 100))) coinNames userNames
    wal cs = BchWallet $ uncurry M.singleton cs

    toAToken name = tokenName $ "a" <> name

toCoin :: ByteString -> Coin
toCoin str = AssetClass (currencySymbol str, tokenName str)
