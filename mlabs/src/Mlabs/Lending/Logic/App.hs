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

import PlutusTx.Prelude hiding ((%))
import Plutus.V1.Ledger.Value
import Plutus.V1.Ledger.Crypto (PubKeyHash(..))

import Mlabs.Emulator.App
import Mlabs.Emulator.Blockchain
import qualified Mlabs.Emulator.Script as S
import Mlabs.Emulator.Types
import Mlabs.Lending.Logic.React
import Mlabs.Lending.Logic.Types

import qualified Data.Map.Strict as M
import qualified PlutusTx.AssocMap as AM
import Mlabs.Data.Ray ((%))
import qualified Mlabs.Data.Ray as R

type LendingApp = App LendingPool Act

runLendingApp :: AppConfig -> Script -> LendingApp
runLendingApp cfg acts = runApp react (initApp cfg) acts

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
  , appConfig'oracles :: [UserId]
  -- ^ users that can submit price changes
  }

-- | App is initialised with list of coins and their rates (value relative to base currency, ada for us)
initApp :: AppConfig -> LendingApp
initApp AppConfig{..} = App
  { app'st = LendingPool
      { lp'reserves       = (AM.fromList (fmap (\x -> (coinCfg'coin x, initReserve x)) appConfig'reserves))
      , lp'users          = AM.empty
      , lp'currency       = appConfig'currencySymbol
      , lp'coinMap        = coinMap
      , lp'healthReport   = AM.empty
      , lp'trustedOracles = appConfig'oracles
      }
  , app'log  = []
  , app'wallets = BchState $ M.fromList $ (Self, defaultBchWallet) : appConfig'users
  }
  where
    coinMap = AM.fromList $ fmap (\CoinCfg{..} -> (coinCfg'aToken, coinCfg'coin)) $ appConfig'reserves

-- | Default application.
-- It allocates three users nad three reserves for Dollars, Euros and Liras.
-- Each user has 100 units of only one currency. User 1 has dollars, user 2 has euros amd user 3 has liras.
defaultAppConfig :: AppConfig
defaultAppConfig = AppConfig reserves users curSym oracles
  where
    oracles = [UserId $ PubKeyHash "1"]  -- only user 1 can set the price
    curSym = currencySymbol "lending-app"
    userNames = ["1", "2", "3"]
    coinNames = ["Dollar", "Euro", "Lira"]

    reserves = fmap (\name ->
        CoinCfg
          { coinCfg'coin = toCoin name
          , coinCfg'rate =  R.fromInteger 1
          , coinCfg'aToken = toAToken name
          , coinCfg'interestModel = defaultInterestModel
          , coinCfg'liquidationBonus = 5 % 100
          }) coinNames

    users = zipWith (\coinName userName -> (UserId (PubKeyHash userName), wal (toCoin coinName, 100))) coinNames userNames
    wal cs = BchWallet $ uncurry M.singleton cs

    toAToken name = tokenName $ "a" <> name

toCoin :: ByteString -> Coin
toCoin str = AssetClass (currencySymbol str, tokenName str)

----------------------------------------------------------
-- scripts

type Script = S.Script Act

-- | Make user act
userAct :: UserId -> UserAct -> Script
userAct uid act = do
  time <- S.getCurrentTime
  S.putAct $ UserAct time uid act

-- | Make price act
priceAct :: UserId -> PriceAct -> Script
priceAct uid arg = do
  t <- S.getCurrentTime
  S.putAct $ PriceAct t uid arg

-- | Make govern act
governAct :: GovernAct -> Script
governAct arg = S.putAct $ GovernAct arg

