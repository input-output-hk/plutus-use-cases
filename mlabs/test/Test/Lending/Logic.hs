-- | Tests for logic of state transitions for aave prototype
module Test.Lending.Logic(
    test
  , testScript
) where

import Test.Tasty
import Test.Tasty.HUnit

import Plutus.V1.Ledger.Value
import Plutus.V1.Ledger.Crypto (PubKeyHash(..))

import Mlabs.Lending.Logic.Emulator.App
import Mlabs.Lending.Logic.Emulator.Blockchain
import Mlabs.Lending.Logic.Types

import Text.Show.Pretty

import qualified Data.Map.Strict as M
import qualified PlutusTx.Ratio as R

noErrors :: App -> Assertion
noErrors app = case app'log app of
  [] -> assertBool "no errors" True
  xs -> do
    mapM_ printLog xs
    assertFailure "There are errors"
  where
    printLog (act, lp, msg) = do
      pPrint act
      pPrint lp
      print msg

someErrors :: App -> Assertion
someErrors app = assertBool "Script fails" $ not $ null (app'log app)

-- | Test suite for a logic of lending application
test :: TestTree
test = testGroup "Logic"
  [ testCase "Deposit" testDeposit
  , testCase "Borrow"  testBorrow
  , testCase "Borrow without collateral" testBorrowNoCollateral
  , testCase "Borrow with not enough collateral" testBorrowNotEnoughCollateral
  , testCase "Withdraw" testWithdraw
  , testCase "Repay" testRepay
  ]
  where
    testBorrow = testWallets [(user1, w1)] borrowScript
      where
        w1 = BchWallet $ M.fromList [(coin1, 50), (coin2, 30), (fromToken aToken1, 0)]

    testDeposit = testWallets [(user1, wal coin1 aToken1), (user2, wal coin2 aToken2), (user3, wal coin3 aToken3)] depositScript
      where
        wal coin aToken = BchWallet $ M.fromList [(coin, 50), (fromToken aToken, 50)]

    testBorrowNoCollateral = someErrors $ testScript borrowNoCollateralScript
    testBorrowNotEnoughCollateral = someErrors $ testScript borrowNotEnoughCollateralScript

    testWithdraw = testWallets [(user1, w1)] withdrawScript
      where
        w1 = BchWallet $ M.fromList [(coin1, 75), (fromToken aToken1, 25)]

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

-- | Checks that script runs without errors
testScript :: Script -> App
testScript script = runApp testAppConfig script

-- | Check that we have those wallets after script was run.
testWallets :: [(UserId, BchWallet)] -> Script -> Assertion
testWallets wals script = do
  noErrors app
  mapM_ (uncurry $ hasWallet app) wals
  where
    app = runApp testAppConfig script

-- | Checks that application state contains concrete wallet for a given user id.
hasWallet :: App -> UserId -> BchWallet -> Assertion
hasWallet app uid wal = lookupAppWallet uid app @=? Just wal

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

-- | Default application.
-- It allocates three users nad three reserves for Dollars, Euros and Liras.
-- Each user has 100 units of only one currency. User 1 has dollars, user 2 has euros amd user 3 has liras.
testAppConfig :: AppConfig
testAppConfig = AppConfig reserves users lendingPoolCurrency
  where
    reserves = fmap (\(coin, aCoin) -> CoinCfg
                                        { coinCfg'coin          = coin
                                        , coinCfg'rate          = R.fromInteger 1
                                        , coinCfg'aToken        = aCoin
                                        , coinCfg'interestModel = defaultInterestModel
                                        })
      [(coin1, aToken1), (coin2, aToken2), (coin3, aToken3)]

    users =
      [ (user1, wal (coin1, 100))
      , (user2, wal (coin2, 100))
      , (user3, wal (coin3, 100))
      ]
    wal cs = BchWallet $ uncurry M.singleton cs

