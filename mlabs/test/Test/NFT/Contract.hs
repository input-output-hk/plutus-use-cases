module Test.NFT.Contract (
  test,
) where

import PlutusTx.Prelude hiding (mconcat, check)
import Prelude (mconcat)
import Test.Tasty (TestTree, testGroup)

import Mlabs.Emulator.Scene (checkScene)
import Mlabs.NFT.Validation
import Test.NFT.Init

test :: TestTree
test =
  testGroup
    "Contract"
    [ testBuyOnce
    , testBuyTwice
    , testChangePriceWithoutOwnership
    , testBuyLockedScript
    , testBuyNotEnoughPriceScript
    ]

-- | User 2 buys from user 1
testBuyOnce :: TestTree
testBuyOnce = check "Buy once" (checkScene scene) script
  where
    script = do
      userAct w1 $ SetPriceAct (Just 100) "NFT"
      userAct w2 $ BuyAct 100 Nothing "NFT"
      userAct w2 $ SetPriceAct (Just 500) "NFT"
    scene =
      mconcat
        [ w1 `ownsAda` 100
        , w2 `ownsAda` (-100)
        ]

{- |
- * User 2 buys from user 1
- * User 3 buys from user 2
-}
testBuyTwice :: TestTree
testBuyTwice = check "Buy twice" (checkScene scene) script
  where
    script = do
      userAct w1 $ SetPriceAct (Just 100) "NFT"
      userAct w2 $ BuyAct 100 Nothing "NFT"
      userAct w2 $ SetPriceAct (Just 500) "NFT"
      userAct w3 $ BuyAct 500 (Just 1000) "NFT"
    scene =
      mconcat
        [ w1 `ownsAda` 150
        , w2 `ownsAda` 350
        , w3 `ownsAda` (-500)
        ]

-- | User 1 tries to set price after user 2 owned the NFT.
testChangePriceWithoutOwnership :: TestTree
testChangePriceWithoutOwnership = check "Sets price without ownership" (checkScene scene) script
  where
    script = do
      userAct w1 $ SetPriceAct (Just 100) "NFT"
      userAct w2 $ BuyAct 100 Nothing "NFT"
      userAct w1 $ SetPriceAct (Just 200) "NFT"
    scene =
      mconcat
        [ w1 `ownsAda` 100
        , w2 `ownsAda` (-100)
        ]

-- | User 2 tries to buy NFT which is locked (no price is set)
testBuyLockedScript :: TestTree
testBuyLockedScript = check "Buy locked NFT" (checkScene noChangesScene) script
  where
    script = userAct w2 $ BuyAct 1000 Nothing "NFT"

-- | User 2 tries to buy open NFT with not enough money
testBuyNotEnoughPriceScript :: TestTree
testBuyNotEnoughPriceScript = check "Buy not enough price" (checkScene noChangesScene) script
  where
    script = do
      userAct w1 $ SetPriceAct (Just 100) "NFT"
      userAct w2 $ BuyAct 10 Nothing "NFT"
