-- | Tests for lending application contracts.
module Test.Lending.Contract(
  test
) where

import Prelude

import Test.Tasty

import Plutus.Contract.Test hiding (tx)
import qualified Plutus.Trace.Emulator as Trace
import qualified PlutusTx.Ratio as R

import Mlabs.Lending.Logic.Types (UserAct(..), InterestRate(..), CoinCfg(..), defaultInterestModel)
import qualified Mlabs.Lending.Contract.Lendex as L

import Test.Utils

import Test.Lending.Init
import Test.Lending.Scene

test :: TestTree
test = testGroup "Contract"
  [ testDeposit
  , testBorrow
  , testBorrowNoCollateral
  , testBorrowNotEnoughCollateral
  , testWithdraw
  , testRepay
  ]
  where
    check msg scene = checkPredicateOptions checkOptions msg (checkScene scene)

    testDeposit = check "Deposit (can mint aTokens)" depositScene depositScript
    testBorrow  = check "Borrow"  borrowScene  borrowScript
    testBorrowNoCollateral = check "Borrow without collateral" borrowWithoutCollateralScene borrowWithoutCollateralScript
    testBorrowNotEnoughCollateral = check "Borrow with not enough collateral" borrowNotEnoughCollateralScene borrowNotEnoughCollateralScript
    testWithdraw = check "Withdraw (can burn aTokens)" withdrawScene withdrawScript
    testRepay = check "Repay" repayScene repayScript

--------------------------------------------------------------------------------
-- deposit test

-- | 3 users deposit 50 coins to lending app. Each of them uses different coin.
depositScript :: Trace.EmulatorTrace ()
depositScript = do
  L.callStartLendex wAdmin $ L.StartParams
    { sp'coins = fmap (\(coin, aCoin) -> CoinCfg
                                          { coinCfg'coin = coin
                                          , coinCfg'rate = R.fromInteger 1
                                          , coinCfg'aToken = aCoin
                                          , coinCfg'interestModel = defaultInterestModel
                                          })
          [(adaCoin, aAda), (coin1, aToken1), (coin2, aToken2), (coin3, aToken3)]
    }
  wait 5
  userAct1 $ DepositAct 50 coin1
  next
  userAct2 $ DepositAct 50 coin2
  next
  userAct3 $ DepositAct 50 coin3
  next

depositScene :: Scene
depositScene = mconcat
  [ appAddress L.lendexAddress
  , appOwns [(coin1, 50), (coin2, 50), (coin3, 50)]
  , user w1 coin1 aCoin1
  , user w2 coin2 aCoin2
  , user w3 coin3 aCoin3  ]
  where
    user wal coin aCoin = wal `owns` [(coin, -50), (aCoin, 50)]

--------------------------------------------------------------------------------
-- borrow test

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

borrowScene :: Scene
borrowScene = depositScene <> borrowChange
  where
    borrowChange = mconcat
      [ w1 `owns` [(aCoin1, -50), (coin2, 30)]
      , appOwns [(aCoin1, 50), (coin2, -30)]
      ]

--------------------------------------------------------------------------------
-- borrow without collateral test (It should fail to borrow)

-- | 3 users deposit 50 coins to lending app
-- and first user borrows in coin2 that he does not own prior to script run.
-- But it should fail because user does not set his deposit funds as collateral.
borrowWithoutCollateralScript :: Trace.EmulatorTrace ()
borrowWithoutCollateralScript = do
  depositScript
  next
  userAct1 $ BorrowAct
        { act'asset           = coin2
        , act'amount          = 30
        , act'rate            = StableRate
        }
  next

borrowWithoutCollateralScene :: Scene
borrowWithoutCollateralScene = depositScene

--------------------------------------------------------------------------------
-- borrow without not enough collateral test (It should fail to borrow)

-- | 3 users deposit 50 coins to lending app
-- and first user wants to borrow too much.
-- Only allocation of collateral succeeds for the first user but borrow step should fail.
borrowNotEnoughCollateralScript :: Trace.EmulatorTrace ()
borrowNotEnoughCollateralScript = do
  depositScript
  userAct1 SetUserReserveAsCollateralAct
        { act'asset           = coin1
        , act'useAsCollateral = True
        , act'portion         = R.fromInteger 1
        }
  next
  userAct1 BorrowAct
        { act'asset           = coin2
        , act'amount          = 60
        , act'rate            = StableRate
        }
  next

-- | Only allocation of collateral succeeds but borrow step should fail.
borrowNotEnoughCollateralScene :: Scene
borrowNotEnoughCollateralScene = depositScene <> setCollateralChange
  where
    setCollateralChange = mconcat [ w1 `owns` [(aCoin1, -50)], appOwns [(aCoin1, 50)]]

--------------------------------------------------------------------------------
-- withdraw test

-- | User1 deposits 50 out of 100 and gets back 25.
-- So we check that user has 75 coins and 25 aCoins
withdrawScript :: Trace.EmulatorTrace ()
withdrawScript = do
  depositScript
  userAct1 WithdrawAct
      { act'amount = 25
      , act'asset  = coin1
      }

withdrawScene :: Scene
withdrawScene = depositScene <> withdrawChange
  where
    withdrawChange = mconcat [ w1 `owns` [(aCoin1, -25), (coin1, 25)], appOwns [(coin1, -25)] ]

--------------------------------------------------------------------------------
-- repay test

repayScript :: Trace.EmulatorTrace ()
repayScript = do
  borrowScript
  userAct1 $ RepayAct
      { act'asset   = coin2
      , act'amount  = 20
      , act'rate    = StableRate
      }

repayScene :: Scene
repayScene = borrowScene <> repayChange
  where
    repayChange = mconcat [w1 `owns` [(coin2, -20)], appOwns [(coin2, 20)]]

