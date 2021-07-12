{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | Inits logic test suite app emulator
module Mlabs.Lending.Logic.App(
  -- * Application
    LendingApp
  , runLendingApp
  , initApp
  , AppConfig(..)
  , defaultAppConfig
  , toCoin
  -- * Script actions
  , Script
  , userAct
  , priceAct
  , governAct
) where

import           PlutusTx.Prelude          hiding ((%))

import           Plutus.V1.Ledger.Crypto   (PubKeyHash(..))
import qualified Data.Map.Strict           as M
import qualified Plutus.V1.Ledger.Value    as Value
import qualified PlutusTx.AssocMap         as AM

import qualified Mlabs.Data.Ray            as Ray
import           Mlabs.Emulator.App        (runApp, App(..))
import           Mlabs.Emulator.Blockchain (defaultBchWallet, BchState(BchState), BchWallet(..))
import qualified Mlabs.Emulator.Script     as Script
import           Mlabs.Lending.Logic.React (react)
import qualified Mlabs.Lending.Logic.Types as Types

type LendingApp = App Types.LendingPool Types.Act

runLendingApp :: AppConfig -> Script -> LendingApp
runLendingApp cfg acts = runApp react (initApp cfg) acts

-- Configuration parameters for app.
data AppConfig = AppConfig
  { appConfig'reserves :: [Types.CoinCfg]
  -- ^ coins with ratios to base currencies for each reserve
  , appConfig'users    :: [(Types.UserId, BchWallet)]
  -- ^ initial set of users with their wallets on blockchain
  -- the wallet for lending app wil be created automatically.
  -- no need to include it here
  , appConfig'currencySymbol :: Value.CurrencySymbol
  -- ^ lending app main currency symbol
  , appConfig'admins  :: [Types.UserId]
  -- ^ users that can do govern actions
  , appConfig'oracles :: [Types.UserId]
  -- ^ users that can submit price changes
  }

-- | App is initialised with list of coins and their rates (value relative to base currency, ada for us)
initApp :: AppConfig -> LendingApp
initApp AppConfig{..} = App
  { app'st = Types.LendingPool
      { lp'reserves       = (AM.fromList (fmap (\x -> (x.coinCfg'coin, Types.initReserve x)) appConfig'reserves))
      , lp'users          = AM.empty
      , lp'currency       = appConfig'currencySymbol
      , lp'coinMap        = coinMap
      , lp'healthReport   = AM.empty
      , lp'admins         = appConfig'admins
      , lp'trustedOracles = appConfig'oracles
      }
  , app'log  = []
  , app'wallets = BchState $ M.fromList $ (Types.Self, defaultBchWallet) : appConfig'users
  }
  where
    coinMap = AM.fromList $ fmap (\Types.CoinCfg{..} -> (coinCfg'aToken, coinCfg'coin)) $ appConfig'reserves

-- | Default application.
-- It allocates three users and three reserves for Dollars, Euros and Liras.
-- Each user has 100 units of only one currency. User 1 has dollars, user 2 has euros amd user 3 has liras.
defaultAppConfig :: AppConfig
defaultAppConfig = AppConfig reserves users curSym admins oracles
  where
    admins  = [user1]
    oracles = [user1]
    user1 = Types.UserId $ PubKeyHash "1"  -- only user 1 can set the price and be admin
    curSym = Value.currencySymbol "lending-app"
    userNames = ["1", "2", "3"]
    coinNames = ["Dollar", "Euro", "Lira"]

    reserves = fmap (\name ->
        Types.CoinCfg
          { coinCfg'coin = toCoin name
          , coinCfg'rate =  Ray.fromInteger 1
          , coinCfg'aToken = toAToken name
          , coinCfg'interestModel = Types.defaultInterestModel
          , coinCfg'liquidationBonus = 5 Ray.% 100
          }) coinNames

    users = zipWith (\coinName userName -> (Types.UserId (PubKeyHash userName), wal (toCoin coinName, 100))) coinNames userNames
    wal cs = BchWallet $ uncurry M.singleton cs

    toAToken name = Value.tokenName $ "a" <> name

toCoin :: ByteString -> Types.Coin
toCoin str = Value.AssetClass (Value.currencySymbol str, Value.tokenName str)

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

