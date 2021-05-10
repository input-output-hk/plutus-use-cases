-- | Tests for lending application contracts.
module Test.Lending.Contract(
  test
) where

import Prelude

import Data.Default

import Test.Tasty
import Test.Tasty.HUnit

import qualified Plutus.V1.Ledger.Ada as Ada
import qualified Plutus.V1.Ledger.Value as Value
import qualified Data.Map as M

import Plutus.Contract.Test hiding (tx)
import qualified Plutus.Trace.Emulator as Trace
import qualified PlutusTx.Ratio as R

import Mlabs.Lending.Logic.Types (Coin, UserAct(..), InterestRate(..))
import qualified Mlabs.Lending.Logic.App as L
import qualified Mlabs.Lending.Contract.Lendex as L

import Test.Utils

test :: TestTree
test = testGroup "Contract"
  [ testCase "Deposit" testDeposit
  , testCase "Borrow"  testBorrow
  ]
  where
    testDeposit = testNoErrors initConfig depositScript
    testBorrow  = do
      Trace.runEmulatorTraceIO' def initConfig borrowScript
      testNoErrors initConfig borrowScript

-- | 3 users deposit 50 coins to lending app. Each of them uses different coin.
depositScript :: Trace.EmulatorTrace ()
depositScript = do
  L.callStartLendex w1 $ L.StartParams
    { sp'coins = fmap (, R.fromInteger 1) [adaCoin, coin1, coin2, coin3] }
  wait 5
  userAct1 $ DepositAct 50 coin1
  userAct2 $ DepositAct 50 coin2
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

-- | Wallets that are used for testing.
w1, w2, w3 :: Wallet
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

adaCoin = Value.AssetClass (Ada.adaSymbol, Ada.adaToken)

-- | Initial config
initConfig :: Trace.EmulatorConfig
initConfig = cfg
  where
    cfg = Trace.EmulatorConfig $ Left $ M.fromList
            [ (w1, val 1000 <> v1 100)
            , (w2, val 1000 <> v2 100)
            , (w3, val 1000 <> v3 100)
            ]

    val x = Value.singleton Ada.adaSymbol Ada.adaToken x

    coinVal coin = uncurry Value.singleton (Value.unAssetClass coin)
    v1 = coinVal coin1
    v2 = coinVal coin2
    v3 = coinVal coin3

