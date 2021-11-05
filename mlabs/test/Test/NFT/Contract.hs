module Test.NFT.Contract (
  test,
) where

import Control.Monad (void)
import Data.Aeson (Value (..))
import Data.Monoid (Last (..))
import Data.Text qualified as T
import Ledger.Crypto (pubKeyHash)
import Mlabs.Emulator.Scene (checkScene)
import Mlabs.NFT.Api (endpoints, queryEndpoints)
import Mlabs.NFT.Contract.Aux (hashData)
import Mlabs.NFT.Contract.Init (createListHead, getAppSymbol, initApp)
import Mlabs.NFT.Contract.Mint (mint)
import Mlabs.NFT.Contract.Query (queryCurrentPrice)
import Mlabs.NFT.Types (
  BuyRequestUser (..),
  MintParams (..),
  NftAppSymbol (..),
  NftId (..),
  QueryResponse (..),
  SetPriceParams (..),
  UserId (..),
 )
import Plutus.Contract (waitNSlots)
import Plutus.Contract.Test (CheckOptions, assertAccumState, assertInstanceLog, assertUserLog, checkPredicateOptions, walletPubKey)
import Plutus.Trace.Emulator (activateContractWallet, callEndpoint)
import Plutus.Trace.Emulator.Types (ContractInstanceLog (..), ContractInstanceMsg (..), UserThreadMsg (..), walletInstanceTag)
import PlutusTx.Prelude hiding (check, mconcat)
import Test.NFT.Init (
  artwork1,
  artwork2,
  callStartNft,
  check,
  checkOptions,
  noChangesScene,
  ownsAda,
  runScript,
  toUserId,
  userBuy,
  userMint,
  userQueryOwner,
  userQueryPrice,
  userSetPrice,
  w1,
  w2,
  w3,
  wA,
 )
import Test.NFT.Trace (AppInitHandle, mintTrace)
import Test.Tasty (TestTree, testGroup)
import Wallet.Emulator.MultiAgent (EmulatorTimeEvent (..))
import Wallet.Emulator.Wallet (walletPubKey)
import Prelude (mconcat, show)
import Prelude qualified as Hask

test :: TestTree
test =
  testGroup
    "Contract"
    [ testBuyOnce
    , testBuyTwice
    , testChangePriceWithoutOwnership
    , testBuyLockedScript
    , testBuyNotEnoughPriceScript
    , testQueryPrice
    , testQueryOwner
    ]

-- | User 2 buys from user 1
testBuyOnce :: TestTree
testBuyOnce = check "Buy once" (checkScene scene) w1 script
  where
    script = do
      nft1 <- userMint w1 artwork1
      userSetPrice w1 $ SetPriceParams nft1 (Just 1_000_000)
      userBuy w2 $ BuyRequestUser nft1 1_000_000 Nothing
      userSetPrice w2 $ SetPriceParams nft1 (Just 2_000_000)
    scene =
      mconcat
        [ w1 `ownsAda` 1_000_000
        , w2 `ownsAda` (-1_000_000)
        ]

{- |
- * User 2 buys from user 1
- * User 3 buys from user 2
-}
testBuyTwice :: TestTree
testBuyTwice = check "Buy twice" (checkScene scene) w1 script
  where
    script = do
      nft1 <- userMint w1 artwork1
      userSetPrice w1 $ SetPriceParams nft1 (Just 1_000_000)
      userBuy w2 $ BuyRequestUser nft1 1_000_000 Nothing
      userSetPrice w2 $ SetPriceParams nft1 (Just 2_000_000)
      userBuy w3 $ BuyRequestUser nft1 2_000_000 Nothing
    scene =
      mconcat
        [ w1 `ownsAda` 1_200_000
        , w2 `ownsAda` 800_000
        , w3 `ownsAda` (-2_000_000)
        ]

-- | User 1 tries to set price after user 2 owned the NFT.
testChangePriceWithoutOwnership :: TestTree
testChangePriceWithoutOwnership = check "Sets price without ownership" (checkScene scene) w1 script
  where
    script = do
      nft1 <- userMint w1 artwork1
      userSetPrice w1 $ SetPriceParams nft1 (Just 1_000_000)
      userBuy w2 $ BuyRequestUser nft1 1_000_000 Nothing
      userSetPrice w1 $ SetPriceParams nft1 (Just 2_000_000)
    scene =
      mconcat
        [ w1 `ownsAda` 1_000_000
        , w2 `ownsAda` (-1_000_000)
        ]

-- | User 2 tries to buy NFT which is locked (no price is set)
testBuyLockedScript :: TestTree
testBuyLockedScript = check "Buy locked NFT" (checkScene noChangesScene) w1 script
  where
    script = do
      nft1 <- userMint w1 artwork1
      userBuy w2 $ BuyRequestUser nft1 1_000_000 Nothing

-- | User 2 tries to buy open NFT with not enough money
testBuyNotEnoughPriceScript :: TestTree
testBuyNotEnoughPriceScript = check "Buy not enough price" (checkScene noChangesScene) w1 script
  where
    script = do
      nft1 <- userMint w1 artwork1
      userSetPrice w1 $ SetPriceParams nft1 (Just 1_000_000)
      userBuy w2 $ BuyRequestUser nft1 500_000 Nothing

-- | User checks the price of the artwork.
testQueryPrice :: TestTree
testQueryPrice = check "Query price" assertState w1 script
  where
    script = do
      nftId <- userMint w1 artwork2
      userQueryPrice w1 nftId

    assertState = assertInstanceLog (walletInstanceTag w1) (any predicate)

    predicate = \case
      (EmulatorTimeEvent _ (ContractInstanceLog (ContractLog (String str)) _ _)) ->
        T.pack msg Hask.== str
      _ -> False
      where
        nftId = NftId . hashData . mp'content $ artwork2
        price = QueryCurrentPrice . mp'price $ artwork2
        msg = mconcat ["Current price of: ", show nftId, " is: ", show price]

testQueryOwner :: TestTree
testQueryOwner = check "Query owner" assertState w1 script
  where
    script = do
      nftId <- userMint w1 artwork2
      userQueryOwner w1 nftId

    assertState = assertInstanceLog (walletInstanceTag w1) (any predicate)

    predicate = \case
      (EmulatorTimeEvent _ (ContractInstanceLog (ContractLog (String str)) _ _)) ->
        T.pack msg Hask.== str
      _ -> False
      where
        nftId = NftId . hashData . mp'content $ artwork2
        owner = QueryCurrentOwner . Just . UserId . pubKeyHash . walletPubKey $ w1
        msg = mconcat ["Current owner of: ", show nftId, " is: ", show owner]
