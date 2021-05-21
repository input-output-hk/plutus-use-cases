-- | Tests for logic of state transitions for aave prototype
module Test.Nft.Logic(
    test
) where

import Test.Tasty
import Test.Tasty.HUnit

import Plutus.V1.Ledger.Value
import Plutus.V1.Ledger.Crypto (PubKeyHash(..))

import Mlabs.Emulator.App
import Mlabs.Emulator.Blockchain
import Mlabs.Emulator.Types

import Mlabs.Nft.Logic.App
import Mlabs.Nft.Logic.Types

import qualified Data.Map.Strict as M
import qualified PlutusTx.Ratio as R

-- | Test suite for a logic of lending application
test :: TestTree
test = testGroup "Logic"
  [ testCase "Buy"                           testBuy
  , testCase "Buy twice"                     testBuyTwice
  , testCase "Sets price without ownership"  testFailToSetPrice
  , testCase "Buy locked NFT"                testBuyLocked
  , testCase "Buy not enough price"          testBuyNotEnoughPrice
  ]
  where
    testBuy               = testWallets     buyWallets      buyScript
    testFailToSetPrice    = testWalletsFail buyWallets      failToSetPriceScript
    testBuyLocked         = testWalletsFail initWallets     failToBuyLocked
    testBuyNotEnoughPrice = testWalletsFail initWallets     failToBuyNotEnoughPrice
    testBuyTwice          = testWallets     buyTwiceWallets buyTwiceScript

    testWallets wals script = do
      noErrors app
      checkWallets wals app
      where
        app = runNftApp defaultAppCfg script

    testWalletsFail wals script = do
      someErrors app
      checkWallets wals app
      where
        app = runNftApp defaultAppCfg script

initWallets :: [(UserId, BchWallet)]
initWallets = [(user1, wal), (user2, wal)]
  where
    wal = BchWallet $ M.fromList [(adaCoin, 1000)]

----------------------------------------------------------------------
-- scripts

-- buy

buyScript :: Script
buyScript = do
  setPrice user1 (Just 100)
  buy user2 100 Nothing
  setPrice user2 (Just 500)

-- * User 1 sets the price to 100
-- * User 2 buys for 100 and becomes owner
-- * User 1 receives 110 (100 + 10% as author)
buyWallets :: [(UserId, BchWallet)]
buyWallets = [(user1, w1), (user2, w2)]
  where
    w1 = BchWallet $ M.fromList [(adaCoin, 1110)]
    w2 = BchWallet $ M.fromList [(adaCoin, 890)]

-- buy twice

-- |
-- * User 2 buys from user 1
-- * User 3 buys from user 2
buyTwiceScript :: Script
buyTwiceScript = do
  buyScript
  buy user3 500 (Just 1000)

buyTwiceWallets :: [(UserId, BchWallet)]
buyTwiceWallets = [(user1, w1), (user2, w2), (user3, w3)]
  where
    w1 = BchWallet $ M.fromList [(adaCoin, 1160)] -- 1000 + 100 + 10 + 50
    w2 = BchWallet $ M.fromList [(adaCoin, 1390)] -- 1000 - 100 - 10 + 500
    w3 = BchWallet $ M.fromList [(adaCoin, 450)]  -- 1000 - 500 - 50

-- fail to set price

-- | User 1 tries to set price after user 2 owned the NFT.
-- It should fail.
failToSetPriceScript :: Script
failToSetPriceScript = do
  buyScript
  setPrice user1 (Just 200)

-- fail to buy locked

-- | User 2 tries to buy NFT which is locked (no price is set)
failToBuyLocked :: Script
failToBuyLocked = do
  buy user2 1000 Nothing

-- fail to buy with not enough money

-- | User 2 tries to buy open NFT with not enough money
failToBuyNotEnoughPrice :: Script
failToBuyNotEnoughPrice = do
  setPrice user1 (Just 100)
  buy user2 10 Nothing


----------------------------------------------------------------------
-- constants

-- users
user1, user2, user3 :: UserId
user1 = UserId $ PubKeyHash "1"
user2 = UserId $ PubKeyHash "2"
user3 = UserId $ PubKeyHash "3"

