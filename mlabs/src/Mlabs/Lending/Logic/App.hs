{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Inits logic test suite app emulator
module Mlabs.Lending.Logic.App (
  -- * Application
  LendingApp,
  runLendingApp,
  initApp,
  AppConfig (..),
  defaultAppConfig,
  toCoin,

  -- * Script actions
  Script,
  userAct,
  priceAct,
  governAct,
  queryAct,
) where

import PlutusTx.Prelude hiding ((%))
import Prelude qualified as Hask (uncurry)

import Data.Map.Strict qualified as M
import Plutus.V1.Ledger.Crypto (PubKeyHash (..))
import Plutus.V1.Ledger.Value qualified as Value
import PlutusTx.AssocMap qualified as AM

import Mlabs.Emulator.App (App (..), runApp)
import Mlabs.Emulator.Blockchain (BchState (BchState), BchWallet (..), defaultBchWallet)
import Mlabs.Emulator.Script qualified as Script
import Mlabs.Lending.Logic.React (react)
import Mlabs.Lending.Logic.Types qualified as Types
import PlutusTx.Ratio qualified as R

type LendingApp = App Types.LendingPool Types.Act

runLendingApp :: AppConfig -> Script -> LendingApp
runLendingApp cfg = runApp react (initApp cfg)

-- Configuration parameters for app.
data AppConfig = AppConfig
  { -- | coins with ratios to base currencies for each reserve
    appConfig'reserves :: [Types.CoinCfg]
  , -- | initial set of users with their wallets on blockchain
    -- the wallet for lending app wil be created automatically.
    -- no need to include it here
    appConfig'users :: [(Types.UserId, BchWallet)]
  , -- | lending app main currency symbol
    appConfig'currencySymbol :: Value.CurrencySymbol
  , -- | users that can do govern actions
    appConfig'admins :: [Types.UserId]
  , -- | users that can submit price changes
    appConfig'oracles :: [Types.UserId]
  }

-- | App is initialised with list of coins and their rates (value relative to base currency, ada for us)
initApp :: AppConfig -> LendingApp
initApp AppConfig {..} =
  App
    { app'st =
        Types.LendingPool
          { lp'reserves = AM.fromList (fmap (\x -> (x.coinCfg'coin, Types.initReserve x)) appConfig'reserves)
          , lp'users = AM.empty
          , lp'currency = appConfig'currencySymbol
          , lp'coinMap = coinMap
          , lp'healthReport = AM.empty
          , lp'admins = appConfig'admins
          , lp'trustedOracles = appConfig'oracles
          }
    , app'log = []
    , app'wallets = BchState $ M.fromList $ (Types.Self, defaultBchWallet) : appConfig'users
    }
  where
    coinMap =
      AM.fromList
        . fmap
          (\Types.CoinCfg {..} -> (coinCfg'aToken, coinCfg'coin))
        $ appConfig'reserves

{- | Default application.
 It allocates three users and three reserves for Dollars, Euros and Liras.
 Each user has 100 units of only one currency. User 1 has dollars, user 2 has euros amd user 3 has liras.
-}
defaultAppConfig :: AppConfig
defaultAppConfig = AppConfig reserves users curSym admins oracles
  where
    admins = [user1]
    oracles = [user1]
    user1 = Types.UserId $ PubKeyHash "1" -- only user 1 can set the price and be admin
    curSym = Value.currencySymbol "lending-app"
    userNames = ["1", "2", "3"]
    coinNames = ["Dollar", "Euro", "Lira"]

    reserves =
      fmap
        ( \name ->
            Types.CoinCfg
              { coinCfg'coin = toCoin name
              , coinCfg'rate = R.fromInteger 1
              , coinCfg'aToken = toAToken name
              , coinCfg'interestModel = Types.defaultInterestModel
              , coinCfg'liquidationBonus = 5 R.% 100
              }
        )
        coinNames

    users = zipWith (\coinName userName -> (Types.UserId (PubKeyHash userName), wal (toCoin coinName, 100))) coinNames userNames
    wal cs = BchWallet $ Hask.uncurry M.singleton cs

    toAToken name = Value.TokenName $ "a" <> name

toCoin :: BuiltinByteString -> Types.Coin
toCoin str = Value.AssetClass (Value.CurrencySymbol str, Value.TokenName str)

----------------------------------------------------------
-- scripts

type Script = Script.Script Types.Act

-- | Make user act
userAct :: Types.UserId -> Types.UserAct -> Script
userAct uid act = do
  time <- Script.getCurrentTime
  Script.putAct $ Types.UserAct time uid act

-- | Make price act
priceAct :: Types.UserId -> Types.PriceAct -> Script
priceAct uid arg = do
  t <- Script.getCurrentTime
  Script.putAct $ Types.PriceAct t uid arg

-- | Make govern act
governAct :: Types.UserId -> Types.GovernAct -> Script
governAct uid arg = Script.putAct $ Types.GovernAct uid arg

-- | Make query act
queryAct :: Types.UserId -> Types.QueryAct -> Script
queryAct uid arg = do
  t <- Script.getCurrentTime
  Script.putAct $ Types.QueryAct uid t arg
