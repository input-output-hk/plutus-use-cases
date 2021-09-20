module Test.Nft.Contract (
  test,
) where

import PlutusTx.Prelude hiding (foldMap, mconcat, (<>))
import Prelude (foldMap, mconcat, (<>))

import Plutus.Contract.Test (Wallet (..), checkPredicateOptions)
import Test.Nft.Init (Script, adaCoin, checkOptions, runScript, userAct, w1, w2, w3)
import Test.Tasty (TestTree, testGroup)

import Mlabs.Emulator.Scene (Scene, checkScene, owns)
import Mlabs.Nft.Logic.Types (UserAct (..))

test :: TestTree
test =
  testGroup
    "Contract"
    [ check "Buy" buyScene buyScript
    , check "Buy twice" buyTwiceScene buyTwiceScript
    , check "Sets price without ownership" buyScene failToSetPriceScript
    , check "Buy locked NFT" noChangesScene failToBuyLockedScript
    , check "Buy not enough price" noChangesScene failToBuyNotEnoughPriceScript
    ]
  where
    check msg scene script = checkPredicateOptions checkOptions msg (checkScene scene) (runScript script)

--------------------------------------------------------------------------------
-- buy test

ownsAda :: Wallet -> Integer -> Scene
ownsAda wal amount = wal `owns` [(adaCoin, amount)]

noChangesScene :: Scene
noChangesScene = foldMap (`ownsAda` 0) [w1, w2, w3]

-- | 3 users deposit 50 coins to lending app. Each of them uses different coin.
buyScript :: Script
buyScript = do
  userAct w1 $ SetPriceAct (Just 100)
  userAct w2 $ BuyAct 100 Nothing
  userAct w2 $ SetPriceAct (Just 500)

buyScene :: Scene
buyScene =
  mconcat
    [ w1 `ownsAda` 110
    , w2 `ownsAda` (-110)
    ]

-- buy twice

{- |
 * User 2 buys from user 1
 * User 3 buys from user 2
-}
buyTwiceScript :: Script
buyTwiceScript = do
  buyScript
  userAct w3 $ BuyAct 500 (Just 1000)

buyTwiceScene :: Scene
buyTwiceScene = buyScene <> buyTwiceChange
  where
    buyTwiceChange =
      mconcat
        [ w1 `ownsAda` 50
        , w2 `ownsAda` 500
        , w3 `ownsAda` (-550)
        ]

--------------------------------------------------------------------------------
-- fail to set price

{- | User 1 tries to set price after user 2 owned the NFT.
 It should fail.
-}
failToSetPriceScript :: Script
failToSetPriceScript = do
  buyScript
  userAct w1 $ SetPriceAct (Just 200)

--------------------------------------------------------------------------------
-- fail to buy locked

-- | User 2 tries to buy NFT which is locked (no price is set)
failToBuyLockedScript :: Script
failToBuyLockedScript = do
  userAct w2 $ BuyAct 1000 Nothing

--------------------------------------------------------------------------------
-- fail to buy with not enough money

-- | User 2 tries to buy open NFT with not enough money
failToBuyNotEnoughPriceScript :: Script
failToBuyNotEnoughPriceScript = do
  userAct w1 $ SetPriceAct (Just 100)
  userAct w2 $ BuyAct 10 Nothing
