-- | Tests for logic of state transitions for aave prototype
module Test.Lending.Logic(
    test
  , testScript
) where

import Test.Tasty
import Test.Tasty.HUnit

import Mlabs.Lending.Logic.App
import Mlabs.Lending.Logic.Emulator
import Mlabs.Lending.Logic.Types

import qualified Data.Map.Strict as M

noErrors :: App -> Bool
noErrors app = null $ app'log app

-- | Test suite for a logic of lending application
test :: TestTree
test = testGroup "User actions"
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
        w1 = BchWallet $ M.fromList [(coin1, 50), (coin2, 30), (aToken coin1, 0)]

    testDeposit = testWallets [(user1, wal coin1), (user2, wal coin2), (user3, wal coin3)] depositScript
      where
        wal coin = BchWallet $ M.fromList [(coin, 50), (aToken coin, 50)]

    testBorrowNoCollateral = testScript borrowNoCollateralScript @=? False
    testBorrowNotEnoughCollateral = testScript borrowNotEnoughCollateralScript @=? False

    testWithdraw = testWallets [(user1, w1)] withdrawScript
      where
        w1 = BchWallet $ M.fromList [(coin1, 75), (aToken coin1, 25)]

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
        w1 = BchWallet $ M.fromList [(coin1, 50), (coin2, 10), (aToken coin1, 0)]

-- | Checks that script runs without errors
testScript :: [Act] -> Bool
testScript script = noErrors $ runApp testAppConfig script

-- | Check that we have those wallets after script was run.
testWallets :: [(UserId, BchWallet)] -> [Act] -> Assertion
testWallets wals script = do
  assertBool "Script has no errors" $ noErrors app
  mapM_ (uncurry $ hasWallet app) wals
  where
    app = runApp testAppConfig script

-- | Checks that application state contains concrete wallet for a given user id.
hasWallet :: App -> UserId -> BchWallet -> Assertion
hasWallet app uid wal = lookupAppWallet uid app @=? Just wal

-- | 3 users deposit 50 coins to lending app
depositScript :: [Act]
depositScript =
  [ UserAct user1 $ DepositAct 50 coin1
  , UserAct user2 $ DepositAct 50 coin2
  , UserAct user3 $ DepositAct 50 coin3
  ]

-- | 3 users deposit 50 coins to lending app
-- and first user borrows in coin2 that he does not own prior to script run.
borrowScript :: [Act]
borrowScript = mconcat
  [ depositScript
  , [ UserAct user1 $ SetUserReserveAsCollateralAct
        { act'asset           = coin1
        , act'useAsCollateral = True
        , act'portion         = 1
        }
    , UserAct user1 $ BorrowAct
        { act'asset           = coin2
        , act'amount          = 30
        , act'rate            = StableRate
        }
    ]
  ]

-- | Try to borrow without setting up deposit as collateral.
borrowNoCollateralScript :: [Act]
borrowNoCollateralScript = mconcat
  [ depositScript
  , pure $ UserAct user1 $ BorrowAct
        { act'asset           = coin2
        , act'amount          = 30
        , act'rate            = StableRate
        }
  ]

-- | Try to borrow more than collateral permits
borrowNotEnoughCollateralScript :: [Act]
borrowNotEnoughCollateralScript = mconcat
  [ depositScript
  , [ UserAct user1 $ SetUserReserveAsCollateralAct
        { act'asset           = coin1
        , act'useAsCollateral = True
        , act'portion         = 1
        }
    , UserAct user1 $ BorrowAct
        { act'asset           = coin2
        , act'amount          = 60
        , act'rate            = StableRate
        }
    ]
  ]

-- | User1 deposits 50 out of 100 and gets back 25.
-- So we check that user has 75 coins and 25 aCoins
withdrawScript :: [Act]
withdrawScript = mconcat
  [ depositScript
  , pure $ UserAct user1 $ WithdrawAct
      { act'amount = 25
      , act'asset  = coin1
      }
  ]

-- | We use borrow script to deposit and borrow for user 1
-- and then repay part of the borrow.
repayScript :: [Act]
repayScript = mconcat
  [ borrowScript
  , pure $ UserAct user1 $ RepayAct
      { act'asset   = coin2
      , act'amount  = 20
      , act'rate    = StableRate
      }
  ]

---------------------------------
-- constants

-- users
user1, user2, user3 :: UserId
user1 = UserId 1
user2 = UserId 2
user3 = UserId 3

-- coins
coin1, coin2, coin3 :: Coin
coin1 = Coin "Dollar"
coin2 = Coin "Euro"
coin3 = Coin "Lira"

-- | Default application.
-- It allocates three users nad three reserves for Dollars, Euros and Liras.
-- Each user has 100 units of only one currency. User 1 has dollars, user 2 has euros amd user 3 has liras.
testAppConfig :: AppConfig
testAppConfig = AppConfig reserves users
  where
    reserves = fmap (, 1) [coin1, coin2, coin3]
    users =
      [ (user1, wal (coin1, 100))
      , (user2, wal (coin2, 100))
      , (user3, wal (coin3, 100))
      ]

    wal cs = BchWallet $ uncurry M.singleton cs

