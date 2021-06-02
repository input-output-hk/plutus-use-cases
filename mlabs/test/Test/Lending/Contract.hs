-- | Tests for lending application contracts.
module Test.Lending.Contract(
  test
) where

import Prelude

import Test.Tasty

import Plutus.Contract.Test hiding (tx)
import qualified Plutus.Trace.Emulator as Trace
import qualified PlutusTx.Ratio as R

import Mlabs.Emulator.Scene
import Mlabs.Lending.Logic.Types ( UserAct(..), InterestRate(..), CoinCfg(..), defaultInterestModel
                                 , PriceAct(..), BadBorrow(..))

import qualified Mlabs.Lending.Contract.Lendex as L
import qualified Plutus.V1.Ledger.Value as Value

import Test.Utils

import Test.Lending.Init

test :: TestTree
test = testGroup "Contract"
  [ testDeposit
  , testBorrow
  , testBorrowNoCollateral
  , testBorrowNotEnoughCollateral
  , testWithdraw
  , testRepay
  , testLiquidationCall
  ]
  where
    check msg scene = checkPredicateOptions checkOptions msg (checkScene scene)

    testDeposit = check "Deposit (can mint aTokens)" depositScene depositScript
    testBorrow  = check "Borrow"  borrowScene  borrowScript
    testBorrowNoCollateral = check "Borrow without collateral" borrowWithoutCollateralScene borrowWithoutCollateralScript
    testBorrowNotEnoughCollateral = check "Borrow with not enough collateral" borrowNotEnoughCollateralScene borrowNotEnoughCollateralScript
    testWithdraw = check "Withdraw (can burn aTokens)" withdrawScene withdrawScript
    testRepay = check "Repay" repayScene repayScript
    testLiquidationCall = testGroup "Liquidation"
      [ check "Liquidation call aToken"        (liquidationCallScene True) (liquidationCallScript True)
      , check "Liquidation call real currency" (liquidationCallScene False) (liquidationCallScript False)
      ]

--------------------------------------------------------------------------------
-- deposit test

-- | 3 users deposit 50 coins to lending app. Each of them uses different coin.
depositScript :: Trace.EmulatorTrace ()
depositScript = do
  L.callStartLendex lendexId wAdmin $ L.StartParams
    { sp'coins = fmap (\(coin, aCoin) -> CoinCfg
                                          { coinCfg'coin = coin
                                          , coinCfg'rate = R.fromInteger 1
                                          , coinCfg'aToken = aCoin
                                          , coinCfg'interestModel = defaultInterestModel
                                          , coinCfg'liquidationBonus = 5 R.% 100
                                          })
          [(adaCoin, aAda), (coin1, aToken1), (coin2, aToken2), (coin3, aToken3)]
    , sp'initValue = Value.assetClassValue adaCoin 1000
    , sp'oracles   = [toUserId wAdmin]
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
  [ appAddress (L.lendexAddress lendexId)
  , appOwns [(coin1, 50), (coin2, 50), (coin3, 50), (adaCoin, 1000)]
  , user w1 coin1 aCoin1
  , user w2 coin2 aCoin2
  , user w3 coin3 aCoin3
  , wAdmin `owns` [(adaCoin, -1000)] ]
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
  next

repayScene :: Scene
repayScene = borrowScene <> repayChange
  where
    repayChange = mconcat [w1 `owns` [(coin2, -20)], appOwns [(coin2, 20)]]

--------------------------------------------------------------------------------
-- liquidation call test

liquidationCallScript :: Bool -> Trace.EmulatorTrace ()
liquidationCallScript receiveAToken = do
  borrowScript
  priceAct wAdmin $ SetAssetPrice coin2 (R.fromInteger 2)
  next
  userAct2 $ LiquidationCallAct
      { act'collateral     = coin1
      , act'debt           = BadBorrow (toUserId w1) coin2
      , act'debtToCover    = 10
      , act'receiveAToken  = receiveAToken
      }
  next

liquidationCallScene :: Bool -> Scene
liquidationCallScene receiveAToken = borrowScene <> liquidationCallChange
  where
    liquidationCallChange = mconcat
      [ w2 `owns` [(receiveCoin, 20), (coin2, -10), (adaCoin, 1)]
      , appOwns [(adaCoin, -1), (coin2, 10), (receiveCoin, -20)]
      ]

    receiveCoin
      | receiveAToken = aCoin1
      | otherwise     = coin1

--------------------------------------------------
-- names as in script test

priceAct :: Wallet -> PriceAct -> Trace.EmulatorTrace ()
priceAct wal act = L.callPriceOracleAct lendexId wal act

