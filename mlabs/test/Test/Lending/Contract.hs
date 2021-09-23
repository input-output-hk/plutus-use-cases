{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

-- | Tests for lending application contracts.
module Test.Lending.Contract (
  test,
) where

import Data.Functor (void)
import Data.Semigroup (Last (..))

import PlutusTx.Prelude --hiding (foldMap, mconcat, (<>))
--import Prelude --(foldMap, mconcat, (<>))

import Plutus.Contract.Test (Wallet, assertAccumState, checkPredicateOptions)
import Plutus.Trace.Emulator qualified as Trace
import Plutus.Trace.Emulator.Types ()
import Plutus.V1.Ledger.Value (assetClassValue)
import PlutusTx.Ratio qualified as R
import Test.Lending.Init (
  aAda,
  aCoin1,
  aCoin2,
  aCoin3,
  aToken1,
  aToken2,
  aToken3,
  adaCoin,
  checkOptions,
  coin1,
  coin2,
  coin3,
  lendexId,
  toPubKeyHash,
  toUserId,
  userAct1,
  userAct2,
  userAct3,
  w1,
  w2,
  w3,
  wAdmin,
 )
import Test.Tasty (TestTree, testGroup)
import Test.Utils (next, wait)

import Mlabs.Emulator.Scene (Scene, appAddress, appOwns, checkScene, owns)
import Mlabs.Lending.Contract qualified as L
import Mlabs.Lending.Contract.Api (StartLendex (..))
import Mlabs.Lending.Contract.Api qualified as Api
import Mlabs.Lending.Contract.Emulator.Client qualified as L
import Mlabs.Lending.Contract.Server qualified as Server
import Mlabs.Lending.Logic.Types (
  BadBorrow (..),
  CoinCfg (..),
  CoinRate (..),
  InterestRate (..),
  PriceAct (..),
  QueryRes (QueryResSupportedCurrencies),
  StartParams (..),
  SupportedCurrency (..),
  UserAct (..),
  defaultInterestModel,
 )
import Mlabs.Plutus.Contract (callEndpoint')

instance Eq a => Eq (Last a) where
  (Last x) == (Last y) = x == y

test :: TestTree
test =
  testGroup
    "Contract"
    [ testDeposit
    , testBorrow
    , testBorrowNoCollateral
    , testBorrowNotEnoughCollateral
    , testWithdraw
    , testRepay
    , testLiquidationCall
    -- , testQueryAllLendexes -- todo: fix - gets stuck in a loop
    -- , testQuerrySupportedCurrencies -- todo: fix
    --    , testQueryCurrentBalance
    -- , testQueryInsolventAccounts -- todo
    ]
  where
    check msg scene = checkPredicateOptions checkOptions msg (checkScene scene)
    testDeposit = check "Deposit (can mint aTokens)" depositScene depositScript
    testBorrow = check "Borrow" borrowScene borrowScript
    testBorrowNoCollateral = check "Borrow without collateral" borrowWithoutCollateralScene borrowWithoutCollateralScript
    testBorrowNotEnoughCollateral = check "Borrow with not enough collateral" borrowNotEnoughCollateralScene borrowNotEnoughCollateralScript
    testWithdraw = check "Withdraw (can burn aTokens)" withdrawScene withdrawScript
    testRepay = check "Repay" repayScene repayScript
    testLiquidationCall =
      testGroup
        "Liquidation"
        [ check "Liquidation call aToken" (liquidationCallScene True) (liquidationCallScript True)
        , check "Liquidation call real currency" (liquidationCallScene False) (liquidationCallScript False)
        ]
    testQueryAllLendexes = check "QueryAllLendexes works" queryAllLendexesScene queryAllLendexesScript

-- testQueryCurrentBalance = check "QeuryCurrentBalance works" queryCurrentBalanceScene queryCurrentBalanceScript
-- testQueryInsolventAccounts =
--------------------------------------------------------------------------------
-- deposit test

-- | 3 users deposit 50 coins to lending app. Each of them uses different coin.
depositScript :: Trace.EmulatorTrace ()
depositScript = do
  L.callStartLendex lendexId wAdmin . StartLendex $
    StartParams
      { sp'coins =
          fmap
            ( \(coin, aCoin) ->
                CoinCfg
                  { coinCfg'coin = coin
                  , coinCfg'rate = R.fromInteger 1
                  , coinCfg'aToken = aCoin
                  , coinCfg'interestModel = defaultInterestModel
                  , coinCfg'liquidationBonus = 5 R.% 100
                  }
            )
            [(adaCoin, aAda), (coin1, aToken1), (coin2, aToken2), (coin3, aToken3)]
      , sp'initValue = assetClassValue adaCoin 1000
      , sp'admins = [toPubKeyHash wAdmin]
      , sp'oracles = [toPubKeyHash wAdmin]
      }
  wait 5
  userAct1 $ DepositAct 50 coin1
  next
  userAct2 $ DepositAct 50 coin2
  next
  userAct3 $ DepositAct 50 coin3
  next

depositScene :: Scene
depositScene =
  mconcat
    [ appAddress (L.lendexAddress lendexId)
    , appOwns [(coin1, 50), (coin2, 50), (coin3, 50), (adaCoin, 1000)]
    , user w1 coin1 aCoin1
    , user w2 coin2 aCoin2
    , user w3 coin3 aCoin3
    , wAdmin `owns` [(adaCoin, -1000)]
    ]
  where
    user wal coin aCoin = wal `owns` [(coin, -50), (aCoin, 50)]

--------------------------------------------------------------------------------
-- borrow test

{- | 3 users deposit 50 coins to lending app
 and first user borrows in coin2 that he does not own prior to script run.
-}
borrowScript :: Trace.EmulatorTrace ()
borrowScript = do
  depositScript
  userAct1
    AddCollateralAct
      { add'asset = coin1
      , add'amount = 50
      }
  next
  userAct1 $
    BorrowAct
      { act'asset = coin2
      , act'amount = 30
      , act'rate = StableRate
      }
  next

borrowScene :: Scene
borrowScene = depositScene <> borrowChange
  where
    borrowChange =
      mconcat
        [ w1 `owns` [(aCoin1, -50), (coin2, 30)]
        , appOwns [(aCoin1, 50), (coin2, -30)]
        ]

--------------------------------------------------------------------------------
-- borrow without collateral test (It should fail to borrow)

{- | 3 users deposit 50 coins to lending app
 and first user borrows in coin2 that he does not own prior to script run.
 But it should fail because user does not set his deposit funds as collateral.
-}
borrowWithoutCollateralScript :: Trace.EmulatorTrace ()
borrowWithoutCollateralScript = do
  depositScript
  next
  userAct1 $
    BorrowAct
      { act'asset = coin2
      , act'amount = 30
      , act'rate = StableRate
      }
  next

borrowWithoutCollateralScene :: Scene
borrowWithoutCollateralScene = depositScene

--------------------------------------------------------------------------------
-- borrow without not enough collateral test (It should fail to borrow)

{- | 3 users deposit 50 coins to lending app
 and first user wants to borrow too much.
 Only allocation of collateral succeeds for the first user but borrow step should fail.
-}
borrowNotEnoughCollateralScript :: Trace.EmulatorTrace ()
borrowNotEnoughCollateralScript = do
  depositScript
  userAct1
    AddCollateralAct
      { add'asset = coin1
      , add'amount = 50
      }
  next
  userAct1
    BorrowAct
      { act'asset = coin2
      , act'amount = 60
      , act'rate = StableRate
      }
  next

-- | Only allocation of collateral succeeds but borrow step should fail.
borrowNotEnoughCollateralScene :: Scene
borrowNotEnoughCollateralScene = depositScene <> setCollateralChange
  where
    setCollateralChange = mconcat [w1 `owns` [(aCoin1, -50)], appOwns [(aCoin1, 50)]]

--------------------------------------------------------------------------------
-- withdraw test

{- | User1 deposits 50 out of 100 and gets back 25.
 So we check that user has 75 coins and 25 aCoins
-}
withdrawScript :: Trace.EmulatorTrace ()
withdrawScript = do
  depositScript
  userAct1
    WithdrawAct
      { act'amount = 25
      , act'asset = coin1
      }

withdrawScene :: Scene
withdrawScene = depositScene <> withdrawChange
  where
    withdrawChange = mconcat [w1 `owns` [(aCoin1, -25), (coin1, 25)], appOwns [(coin1, -25)]]

--------------------------------------------------------------------------------
-- repay test

repayScript :: Trace.EmulatorTrace ()
repayScript = do
  borrowScript
  userAct1 $
    RepayAct
      { act'asset = coin2
      , act'amount = 20
      , act'rate = StableRate
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
  priceAct wAdmin $ SetAssetPriceAct coin2 (R.fromInteger 2)
  next
  userAct2 $
    LiquidationCallAct
      { act'collateral = coin1
      , act'debt = BadBorrow (toUserId w1) coin2
      , act'debtToCover = 10
      , act'receiveAToken = receiveAToken
      }
  next

liquidationCallScene :: Bool -> Scene
liquidationCallScene receiveAToken = borrowScene <> liquidationCallChange
  where
    liquidationCallChange =
      mconcat
        [ w2 `owns` [(receiveCoin, 20), (coin2, -10), (adaCoin, 1)]
        , appOwns [(adaCoin, -1), (coin2, 10), (receiveCoin, -20)]
        ]

    receiveCoin
      | receiveAToken = aCoin1
      | otherwise = coin1

--------------------------------------------------------------------------------
-- queryAllLendexes test

queryAllLendexesScript :: Trace.EmulatorTrace ()
queryAllLendexesScript = do
  depositScript
  void $ L.queryAllLendexes lendexId w1 (L.QueryAllLendexes sp)
  where
    sp =
      StartParams
        { sp'coins =
            fmap
              ( \(coin, aCoin) ->
                  CoinCfg
                    { coinCfg'coin = coin
                    , coinCfg'rate = R.fromInteger 1
                    , coinCfg'aToken = aCoin
                    , coinCfg'interestModel = defaultInterestModel
                    , coinCfg'liquidationBonus = 5 R.% 100
                    }
              )
              [(adaCoin, aAda), (coin1, aToken1), (coin2, aToken2), (coin3, aToken3)]
        , sp'initValue = assetClassValue adaCoin 1000
        , sp'admins = [toPubKeyHash wAdmin]
        , sp'oracles = [toPubKeyHash wAdmin]
        }

queryAllLendexesScene :: Scene
queryAllLendexesScene = depositScene

--------------------------------------------------------------------------------
-- querry get Current Balance test

-- TODO Write QueryCurrentBalance TEST

-- queryCurrentBalanceScript :: Trace.EmulatorTrace ()
-- queryCurrentBalanceScript = do
--   depositScript
--   void $ L.queryCurrentBalance lendexId w1 (L.QueryCurrentBalance ())

{- | Scene is identical as the State is not changed.
 queryCurrentBalanceScene :: Scene
 queryCurrentBalanceScene = depositScene
-}

--------------------------------------------------------------------------------
-- querry supported currencies test
testQuerrySupportedCurrencies :: TestTree
testQuerrySupportedCurrencies =
  checkPredicateOptions
    checkOptions
    "QuerrySupportedCurrencies"
    ( assertAccumState
        contract
        tag
        (== expectedQueryResult)
        "contract state after QuerrySupportedCurrencies call doesn't match expected"
    )
    $ do
      initLendex lendexId
      next
      hdl <- Trace.activateContractWallet w1 contract
      void $ callEndpoint' @Api.QuerySupportedCurrencies hdl (Api.QuerySupportedCurrencies ())
      next
  where
    initLendex lid = L.callStartLendex lid wAdmin . StartLendex $ sp
    contract = Server.queryEndpoints lendexId
    tag = Trace.walletInstanceTag w1
    coins = [(adaCoin, aAda, 1 R.% 1), (coin1, aToken1, 1 R.% 2)]
    expectedQueryResult =
      Just . Last . QueryResSupportedCurrencies $
        (\(coin, aCoin, rate) -> SupportedCurrency coin aCoin (CoinRate rate 0)) <$> coins
    sp =
      StartParams
        { sp'coins =
            fmap
              ( \(coin, aCoin, rate) ->
                  CoinCfg
                    { coinCfg'coin = coin
                    , coinCfg'rate = rate
                    , coinCfg'aToken = aCoin
                    , coinCfg'interestModel = defaultInterestModel
                    , coinCfg'liquidationBonus = 5 R.% 100
                    }
              )
              coins
        , sp'initValue = assetClassValue adaCoin 1000
        , sp'admins = [toPubKeyHash wAdmin]
        , sp'oracles = [toPubKeyHash wAdmin]
        }

--------------------------------------------------
-- names as in script test

priceAct :: Wallet -> PriceAct -> Trace.EmulatorTrace ()
priceAct = L.callPriceAct lendexId
