module Test.Nft.Contract(
  test
) where

import Prelude
import Data.Functor (void)

import Test.Tasty

import Plutus.Contract.Test hiding (tx)
import qualified Plutus.Trace.Emulator as Trace
import qualified PlutusTx.Ratio as R

import Mlabs.Emulator.Scene
import Mlabs.Nft.Logic.Types ( UserAct(..))
import qualified Mlabs.Nft.Contract.Nft as N

import Test.Utils
import Test.Nft.Init

test :: TestTree
test = testGroup "Contract"
  [ check "Buy"                           buyScene buyScript
  , check "Buy twice"                     buyTwiceScene buyTwiceScript
  , check "Sets price without ownership"  buyScene failToSetPriceScript
  , check "Buy locked NFT"                noChangesScene failToBuyLockedScript
  , check "Buy not enough price"          noChangesScene failToBuyNotEnoughPriceScript
  ]
  where
    check msg scene = checkPredicateOptions checkOptions msg (checkScene scene)

--------------------------------------------------------------------------------
-- buy test

ownsAda :: Wallet -> Integer -> Scene
ownsAda wal amount = wal `owns` [(adaCoin, amount)]

noChangesScene :: Scene
noChangesScene = foldMap ( `ownsAda` 0) [w1, w2, w3]

-- | 3 users deposit 50 coins to lending app. Each of them uses different coin.
buyScript :: Trace.EmulatorTrace ()
buyScript = do
  void $ N.callStartNft w1 $ N.StartParams
    { sp'content = nftContent
    , sp'share   = 1 R.% 10
    , sp'price   = Nothing
    }
  next
  userAct1 $ SetPrice (Just 100)
  userAct2 $ Buy 100 Nothing
  userAct2 $ SetPrice (Just 500)

buyScene :: Scene
buyScene = mconcat
  [ appAddress $ N.nftAddress nftId
  -- , appOwns [(nftCoin, 1)]
  , w1 `ownsAda` 110
  , w2 `ownsAda` (-110)
  ]

-- buy twice

-- |
-- * User 2 buys from user 1
-- * User 3 buys from user 2
buyTwiceScript :: Trace.EmulatorTrace ()
buyTwiceScript = do
  buyScript
  userAct3 $ Buy 500 (Just 1000)

buyTwiceScene :: Scene
buyTwiceScene = buyScene <> buyTwiceChange
  where
    buyTwiceChange = mconcat
      [ w1 `ownsAda` 50
      , w2 `ownsAda` 500
      , w3 `ownsAda` (-550)
      ]


--------------------------------------------------------------------------------
-- fail to set price

-- | User 1 tries to set price after user 2 owned the NFT.
-- It should fail.
failToSetPriceScript :: Trace.EmulatorTrace ()
failToSetPriceScript = do
  buyScript
  userAct1 $ SetPrice (Just 200)

--------------------------------------------------------------------------------
-- fail to buy locked

-- | User 2 tries to buy NFT which is locked (no price is set)
failToBuyLockedScript :: Trace.EmulatorTrace ()
failToBuyLockedScript = do
  userAct2 $ Buy 1000 Nothing

--------------------------------------------------------------------------------
-- fail to buy with not enough money

-- | User 2 tries to buy open NFT with not enough money
failToBuyNotEnoughPriceScript :: Trace.EmulatorTrace ()
failToBuyNotEnoughPriceScript = do
  userAct1 $ SetPrice (Just 100)
  userAct2 $ Buy 10 Nothing

