-- | Tests for lending application contracts.
module Test.Lending.Contract(
  test
) where

import Prelude

-- import Data.Default
import Control.Lens

import Test.Tasty

import Plutus.V1.Ledger.Value (Value, TokenName)
import qualified Plutus.V1.Ledger.Ada as Ada
import qualified Plutus.V1.Ledger.Value as Value
import qualified Data.Map as M

import Plutus.Contract.Test hiding (tx)
import qualified Plutus.Trace.Emulator as Trace
import qualified PlutusTx.Ratio as R

import Mlabs.Lending.Logic.Types (Coin, UserAct(..), InterestRate(..), CoinCfg(..))
import qualified Mlabs.Lending.Logic.App as L
import qualified Mlabs.Lending.Contract.Lendex as L
import qualified Mlabs.Lending.Contract.Forge as Forge

import Test.Utils

import Test.Lending.Scene

depositScene :: Scene
depositScene = appOwns mempty
  <> mconcat
      [ user w1 coin1 aCoin1
      , user w2 coin2 aCoin2
      , user w3 coin3 aCoin3  ]
  where
    user wal coin aCoin = wal `owns` [(coin, -50), (aCoin, 50)]

borrowScene :: Scene
borrowScene = depositScene <> borrowChange
  where
    borrowChange = w1 `owns` [(aCoin1, -50), (coin2, 30)]


test :: TestTree
test = testGroup "Contract"
  [ testDeposit
  , testBorrow
  ]
  where
    check msg scene = checkPredicateOptions checkOptions msg (checkScene scene)

    testDeposit = check "Deposit" depositScene depositScript
    testBorrow  = check "Borrow"  borrowScene  borrowScript

-- | 3 users deposit 50 coins to lending app. Each of them uses different coin.
depositScript :: Trace.EmulatorTrace ()
depositScript = do
  L.callStartLendex wAdmin $ L.StartParams
    { sp'coins = fmap (\(coin, aCoin) -> CoinCfg coin (R.fromInteger 1) aCoin) [(adaCoin, aAda), (coin1, aToken1), (coin2, aToken2), (coin3, aToken3)] }
  wait 5
  userAct1 $ DepositAct 50 coin1
  next
  userAct2 $ DepositAct 50 coin2
  next
  userAct3 $ DepositAct 50 coin3
  next

-- | 3 users deposit 50 coins to lending app
-- and first user borrows in coin2 that he does not own prior to script run.
borrowScript :: Trace.EmulatorTrace ()
borrowScript = do
  depositScript
  userAct1 SetUserReserveAsCollateralAct
        { act'asset           = coin1
        , act'useAsCollateral = True
        , act'portion         = R.fromInteger 1
        }
  next
  userAct1 $ BorrowAct
        { act'asset           = coin2
        , act'amount          = 30
        , act'rate            = StableRate
        }
  next

------------------------------------------------------------------------------------
-- init blockchain state

checkOptions :: CheckOptions
checkOptions = defaultCheckOptions & emulatorConfig . Trace.initialChainState .~ Left initialDistribution

-- | Wallets that are used for testing.
wAdmin, w1, w2, w3 :: Wallet
wAdmin = Wallet 50
w1 = Wallet 1
w2 = Wallet 2
w3 = Wallet 3

userAct1, userAct2, userAct3 :: UserAct -> Trace.EmulatorTrace ()
userAct1 = L.callUserAct w1
userAct2 = L.callUserAct w2
userAct3 = L.callUserAct w3

-- coins
adaCoin, coin1, coin2, coin3 :: Coin
coin1 = L.toCoin "Dollar"
coin2 = L.toCoin "Euro"
coin3 = L.toCoin "Lira"

aToken1, aToken2, aToken3, aAda :: TokenName
aToken1 = Value.tokenName "aDollar"
aToken2 = Value.tokenName "aEuro"
aToken3 = Value.tokenName "aLira"
aAda    = Value.tokenName "aAda"

adaCoin = Value.AssetClass (Ada.adaSymbol, Ada.adaToken)

fromToken :: TokenName -> Coin
fromToken aToken = Value.AssetClass (Forge.currencySymbol, aToken)

aCoin1, aCoin2, aCoin3 :: Coin
aCoin1 = fromToken aToken1
aCoin2 = fromToken aToken2
aCoin3 = fromToken aToken3

initialDistribution :: M.Map Wallet Value
initialDistribution = M.fromList
  [ (wAdmin, val 1000)
  , (w1, val 1000 <> v1 100)
  , (w2, val 1000 <> v2 100)
  , (w3, val 1000 <> v3 100)
  ]
  where
    val x = Value.singleton Ada.adaSymbol Ada.adaToken x

    coinVal coin = uncurry Value.singleton (Value.unAssetClass coin)
    v1 = coinVal coin1
    v2 = coinVal coin2
    v3 = coinVal coin3


