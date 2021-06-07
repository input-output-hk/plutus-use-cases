-- | Tests for logic of state transitions for aave prototype
module Test.Lending.Logic(
    test
  , testScript
) where

import Test.Tasty
import Test.Tasty.HUnit

import Plutus.V1.Ledger.Value
import Plutus.V1.Ledger.Crypto (PubKeyHash(..))

import Mlabs.Emulator.App
import Mlabs.Emulator.Blockchain
import Mlabs.Lending.Logic.App
import Mlabs.Lending.Logic.Types


import qualified Data.Map.Strict as M
import qualified Mlabs.Data.Ray as R

-- | Test suite for a logic of lending application
test :: TestTree
test = testGroup "Logic"
  [ testCase "Deposit" testDeposit
  , testCase "Borrow"  testBorrow
  , testCase "Borrow without collateral" testBorrowNoCollateral
  , testCase "Borrow with not enough collateral" testBorrowNotEnoughCollateral
  , testCase "Withdraw" testWithdraw
  , testCase "Repay" testRepay
  , testGroup "Borrow liquidation" testLiquidationCall
  , testCase "Wrong user sets the price" testWrongUserPriceSet
  ]
  where
    testBorrow = testWallets [(user1, w1)] borrowScript
      where
        w1 = BchWallet $ M.fromList [(coin1, 50), (coin2, 30), (aCoin1, 0)]

    testDeposit = testWallets [(user1, wal coin1 aToken1), (user2, wal coin2 aToken2), (user3, wal coin3 aToken3)] depositScript
      where
        wal coin aToken = BchWallet $ M.fromList [(coin, 50), (fromToken aToken, 50)]

    testBorrowNoCollateral = someErrors $ testScript borrowNoCollateralScript
    testBorrowNotEnoughCollateral = someErrors $ testScript borrowNotEnoughCollateralScript

    testWithdraw = testWallets [(user1, w1)] withdrawScript
      where
        w1 = BchWallet $ M.fromList [(coin1, 75), (aCoin1, 25)]

    -- User:
    --  * deposits 50 coin1
    --  * sets it all as collateral
    --  * borrows 30 coin2
    --  * repays 20 coin2 back
    --
    --  So we get:
    --    coin1 - 50
    --    coin2 - 10 = 30 - 20
    --    aToken - 0 = remaining from collateral
    testRepay = testWallets [(user1, w1)] repayScript
      where
        w1 = BchWallet $ M.fromList [(coin1, 50), (coin2, 10), (fromToken aToken1, 0)]

    testLiquidationCall =
      [ testCase "get aTokens for collateral" $
          testWallets [(user1, w1), (user2, w2a)] $ liquidationCallScript True
      , testCase "get underlying currency for collateral" $
          testWallets [(user1, w1), (user2, w2)]  $ liquidationCallScript False
      ]
      where
        w1 = BchWallet $ M.fromList [(coin1, 50), (coin2, 30), (fromToken aToken1, 0)]
        -- receive aTokens
        w2a = BchWallet $ M.fromList [(coin2, 40), (aCoin2, 50) , (aCoin1, 20), (adaCoin, 1)]
        -- receive underlying currency
        w2 = BchWallet $ M.fromList [(coin2, 40), (aCoin2, 50) , (coin1, 20), (adaCoin, 1)]

    testWrongUserPriceSet = someErrors $ testScript wrongUserPriceSetScript

-- | Checks that script runs without errors
testScript :: Script -> LendingApp
testScript script = runLendingApp testAppConfig script

-- | Check that we have those wallets after script was run.
testWallets :: [(UserId, BchWallet)] -> Script -> Assertion
testWallets wals script = do
  noErrors app
  checkWallets wals app
  where
    app = runLendingApp testAppConfig script

-- | 3 users deposit 50 coins to lending app
depositScript :: Script
depositScript = do
  userAct user1 $ DepositAct 50 coin1
  userAct user2 $ DepositAct 50 coin2
  userAct user3 $ DepositAct 50 coin3

-- | 3 users deposit 50 coins to lending app
-- and first user borrows in coin2 that he does not own prior to script run.
borrowScript :: Script
borrowScript = do
  depositScript
  userAct user1 $ SetUserReserveAsCollateralAct
        { act'asset           = coin1
        , act'useAsCollateral = True
        , act'portion         = R.fromInteger 1 }
  userAct user1 $ BorrowAct
        { act'asset           = coin2
        , act'amount          = 30
        , act'rate            = StableRate }

-- | Try to borrow without setting up deposit as collateral.
borrowNoCollateralScript :: Script
borrowNoCollateralScript = do
  depositScript
  userAct user1 $ BorrowAct
        { act'asset           = coin2
        , act'amount          = 30
        , act'rate            = StableRate
        }

-- | Try to borrow more than collateral permits
borrowNotEnoughCollateralScript :: Script
borrowNotEnoughCollateralScript = do
  depositScript
  userAct user1 $ SetUserReserveAsCollateralAct
        { act'asset           = coin1
        , act'useAsCollateral = True
        , act'portion         = R.fromInteger 1 }
  userAct user1 $ BorrowAct
        { act'asset           = coin2
        , act'amount          = 60
        , act'rate            = StableRate }

-- | User1 deposits 50 out of 100 and gets back 25.
-- So we check that user has 75 coins and 25 aCoins
withdrawScript :: Script
withdrawScript = do
  depositScript
  userAct user1 $ WithdrawAct
      { act'amount = 25
      , act'asset  = coin1
      }

-- | We use borrow script to deposit and borrow for user 1
-- and then repay part of the borrow.
repayScript :: Script
repayScript = do
  borrowScript
  userAct user1 $ RepayAct
      { act'asset   = coin2
      , act'amount  = 20
      , act'rate    = StableRate
      }

-- |
-- * User 1 lends in coin1 and borrows in coin2
-- * price for coin2 grows so that collateral is not enough
-- * health check for user 1 becomes bad
-- * user 2 repays part of the borrow and aquires part of the collateral of the user 1
--
-- So we should get the balances
--
-- * init           | user1 = 100 $       | user2 = 100 €
-- * after deposit  | user1 = 50 $, 50 a$ | user2 = 50 €, 50 a€
-- * after borrow   | user1 = 50 $, 30 €  | user2 = 50 €, 50 a€
-- * after liq call | user1 = 50 $, 30 €  | user2 = 40 €, 50 a€, 20 a$, 1 ada  : if flag is True
-- * after liq call | user1 = 50 $, 30 €  | user2 = 40 €, 50 a€, 20 $,  1 ada  : if flag is False
--
-- user2 pays 10 € for borrow, because at that time Euro to Dollar is 2:1 user2
-- gets 20 aDollars, and 1 ada as bonus (5% of the collateral (20) which is rounded).
-- User gets aDolars because user provides recieveATokens set to True
liquidationCallScript :: Bool -> Script
liquidationCallScript receiveAToken = do
  borrowScript
  priceAct user1 $ SetAssetPriceAct coin2 (R.fromInteger 2)
  userAct user2 $ LiquidationCallAct
      { act'collateral     = coin1
      , act'debt           = BadBorrow user1 coin2
      , act'debtToCover    = 10
      , act'receiveAToken  = receiveAToken
      }

-- oracles

wrongUserPriceSetScript :: Script
wrongUserPriceSetScript = do
  priceAct user2 $ SetAssetPriceAct coin2 (R.fromInteger 2)

---------------------------------
-- constants

-- | convert aToken to aCoin
fromToken :: TokenName -> Coin
fromToken aToken = AssetClass (lendingPoolCurrency, aToken)

-- | Base currency of lending app (it's mock for monetary policy of the lending app)
lendingPoolCurrency :: CurrencySymbol
lendingPoolCurrency = currencySymbol "lending-pool"

-- users
user1, user2, user3 :: UserId
user1 = UserId $ PubKeyHash "1"
user2 = UserId $ PubKeyHash "2"
user3 = UserId $ PubKeyHash "3"

-- coins
coin1, coin2, coin3 :: Coin
coin1 = toCoin "Dollar"
coin2 = toCoin "Euro"
coin3 = toCoin "Lira"

-- | aTokens
aToken1, aToken2, aToken3 :: TokenName
aToken1 = tokenName "aDollar"
aToken2 = tokenName "aEuro"
aToken3 = tokenName "aLira"

aCoin1, aCoin2 :: Coin
aCoin1 = fromToken aToken1
aCoin2 = fromToken aToken2
-- aCoin3 = fromToken aToken3

-- | Default application.
-- It allocates three users nad three reserves for Dollars, Euros and Liras.
-- Each user has 100 units of only one currency. User 1 has dollars, user 2 has euros amd user 3 has liras.
testAppConfig :: AppConfig
testAppConfig = AppConfig reserves users lendingPoolCurrency admins oracles
  where
    admins  = [user1]
    oracles = [user1]

    reserves = fmap (\(coin, aCoin) -> CoinCfg
                                        { coinCfg'coin             = coin
                                        , coinCfg'rate             = R.fromInteger 1
                                        , coinCfg'aToken           = aCoin
                                        , coinCfg'interestModel    = defaultInterestModel
                                        , coinCfg'liquidationBonus = 5 R.% 100
                                        })
      [(coin1, aToken1), (coin2, aToken2), (coin3, aToken3)]

    users =
      [ (Self, wal (adaCoin, 1000)) -- script starts with some ada on it
      , (user1, wal (coin1, 100))
      , (user2, wal (coin2, 100))
      , (user3, wal (coin3, 100))
      ]
    wal cs = BchWallet $ uncurry M.singleton cs

