module Test.NFT.Contract (
  test,
) where

import Control.Monad (void)
import Data.Default (def)
import Data.List (sortOn)
import Data.Text qualified as T
import Ledger.Crypto (pubKeyHash)
import Ledger.Index (ValidationError (..))
import Ledger.Scripts (ScriptError (..))
import Ledger.TimeSlot (slotToBeginPOSIXTime)
import Plutus.Contract.Test (assertFailedTransaction, assertInstanceLog)
import Plutus.Contract.Trace (walletPubKeyHash)
import PlutusTx.Prelude hiding (check, mconcat)
import Test.Tasty (TestTree, testGroup)
import Prelude (mconcat)
import Prelude qualified as Hask

import Mlabs.Emulator.Scene (checkScene)
import Mlabs.NFT.Contract.Aux (hashData)
import Mlabs.NFT.Contract.Mint (mintParamsToInfo)
import Mlabs.NFT.Contract.Query (queryContentLog, queryCurrentOwnerLog, queryCurrentPriceLog, queryListNftsLog)
import Mlabs.NFT.Types (
  AuctionBidParams (..),
  AuctionCloseParams (..),
  AuctionOpenParams (..),
  BuyRequestUser (..),
  Content (..),
  InformationNft (..),
  MintParams (..),
  NftId (..),
  QueryResponse (..),
  SetPriceParams (..),
  UserId (..),
 )
import Test.NFT.Init (
  artwork1,
  artwork2,
  callStartNftFail,
  check,
  containsLog,
  noChangesScene,
  ownsAda,
  toUserId,
  userBidAuction,
  userBuy,
  userCloseAuction,
  userMint,
  userQueryContent,
  userQueryListNfts,
  userQueryOwner,
  userQueryPrice,
  userSetPrice,
  userStartAuction,
  userWait,
  w1,
  w2,
  w3,
  wA,
 )

test :: TestTree
test =
  testGroup
    "Contract"
    [ testInitApp
    , testBuyOnce
    , testBuyTwice
    , testChangePriceWithoutOwnership
    , testBuyLockedScript
    , testBuyNotEnoughPriceScript
    , testGroup
        "Auction"
        [ testAuctionOneBid
        , testAuctionOneBidNoClosing
        , testAuctionManyBids
        , testBidAfterDeadline
        , testAuctionWithPrice
        , testSetPriceDuringAuction
        ]
    , testGroup
        "Query"
        [ testQueryPrice
        , testQueryOwner
        , testQueryListNfts
        , testQueryContent
        ]
    ]

-- | Test initialisation of an app instance
testInitApp :: TestTree
testInitApp = check "Init app" assertState wA script
  where
    script = callStartNftFail wA
    assertState =
      assertFailedTransaction
        ( \_ vEr _ ->
            case vEr of
              (ScriptFailure (EvaluationError (er : _) _)) -> msg Hask.== T.unpack er
              _ -> False
        )
    msg = "Only an admin can initialise app."

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

testAuctionOneBid :: TestTree
testAuctionOneBid = check "Single bid" (checkScene scene) w1 script
  where
    script = do
      nft1 <- userMint w1 artwork1
      userStartAuction w1 $ AuctionOpenParams nft1 (slotToBeginPOSIXTime def 20) 0
      userBidAuction w2 $ AuctionBidParams nft1 1_000_000
      userWait 20
      userCloseAuction w1 $ AuctionCloseParams nft1
      userWait 3

    scene =
      mconcat
        [ w1 `ownsAda` 1_000_000
        , w2 `ownsAda` (-1_000_000)
        ]

testAuctionOneBidNoClosing :: TestTree
testAuctionOneBidNoClosing = check "Single bid without closing" (checkScene scene) w1 script
  where
    script = do
      nft1 <- userMint w1 artwork1
      userStartAuction w1 $ AuctionOpenParams nft1 (slotToBeginPOSIXTime def 9999) 500_000
      userBidAuction w1 $ AuctionBidParams nft1 1_000_000
      void $ userMint w1 artwork2

    scene =
      mconcat
        [ w1 `ownsAda` (-1_000_000)
        ]

testAuctionManyBids :: TestTree
testAuctionManyBids = check "Multiple  bids" (checkScene scene) w1 script
  where
    script = do
      nft1 <- userMint w1 artwork1
      userStartAuction w1 $ AuctionOpenParams nft1 (slotToBeginPOSIXTime def 20) 0
      userBidAuction w3 $ AuctionBidParams nft1 100_000
      userBidAuction w2 $ AuctionBidParams nft1 1_000_000
      userBidAuction w3 $ AuctionBidParams nft1 200_000

      userWait 20
      userCloseAuction w1 $ AuctionCloseParams nft1
      userWait 3

    scene =
      mconcat
        [ w1 `ownsAda` 1_000_000
        , w2 `ownsAda` (-1_000_000)
        ]

testAuctionWithPrice :: TestTree
testAuctionWithPrice = check "Starting auction overrides price" (containsLog w1 msg) w1 script
  where
    script = do
      nft2 <- userMint w1 artwork2
      userSetPrice w1 $ SetPriceParams nft2 (Just 1_000_000)
      userStartAuction w1 $ AuctionOpenParams nft2 (slotToBeginPOSIXTime def 20) 500_000
      userQueryPrice w1 nft2

    nftId = NftId . hashData . mp'content $ artwork2
    price = QueryCurrentPrice Nothing
    msg = queryCurrentPriceLog nftId price

testSetPriceDuringAuction :: TestTree
testSetPriceDuringAuction = check "Cannot set price during auction" (containsLog w1 msg) w1 script
  where
    script = do
      nft2 <- userMint w1 artwork2
      userSetPrice w1 $ SetPriceParams nft2 (Just 1_000_000)
      userStartAuction w1 $ AuctionOpenParams nft2 (slotToBeginPOSIXTime def 20) 500_000
      userQueryPrice w1 nft2
      userSetPrice w1 $ SetPriceParams nft2 (Just 1_000_000)

    nftId = NftId . hashData . mp'content $ artwork2
    price = QueryCurrentPrice Nothing
    msg = queryCurrentPriceLog nftId price

testBidAfterDeadline :: TestTree
testBidAfterDeadline = check "Cannot bid after deadline" (checkScene scene) w1 script
  where
    script = do
      nft1 <- userMint w1 artwork1
      userStartAuction w1 $ AuctionOpenParams nft1 (slotToBeginPOSIXTime def 20) 0
      userBidAuction w3 $ AuctionBidParams nft1 100_000
      userBidAuction w2 $ AuctionBidParams nft1 1_000_000
      userBidAuction w3 $ AuctionBidParams nft1 200_000

      userWait 20
      userCloseAuction w1 $ AuctionCloseParams nft1
      userWait 3
      userBidAuction w3 $ AuctionBidParams nft1 1_500_000

    scene =
      mconcat
        [ w1 `ownsAda` 1_000_000
        , w2 `ownsAda` (-1_000_000)
        ]

-- | User checks the price of the artwork.
testQueryPrice :: TestTree
testQueryPrice = check "Query price" (containsLog w1 msg) w1 script
  where
    script = do
      nft2 <- userMint w1 artwork2
      userQueryPrice w1 nft2

    nftId = NftId . hashData . mp'content $ artwork2
    price = QueryCurrentPrice . mp'price $ artwork2
    msg = queryCurrentPriceLog nftId price

testQueryOwner :: TestTree
testQueryOwner = check "Query owner" (containsLog w1 msg) w1 script
  where
    script = do
      nft2 <- userMint w1 artwork2
      userQueryOwner w1 nft2

    nftId = NftId . hashData . mp'content $ artwork2
    owner = QueryCurrentOwner . Just . toUserId $ w1
    msg = queryCurrentOwnerLog nftId owner

-- | User lists all NFTs in app
testQueryListNfts :: TestTree
testQueryListNfts = check "Query list NFTs" (containsLog w1 msg) w1 script
  where
    script = do
      mapM_ (userMint w1) artworks
      userQueryListNfts w1

    artworks = [artwork1, artwork2]

    nfts =
      sortOn info'id
        . fmap (\mp -> mintParamsToInfo mp (toUserId w1))
        $ artworks

    msg = queryListNftsLog nfts

testQueryContent :: TestTree
testQueryContent = check "Query content" (containsLog w1 msg) w1 script
  where
    script = do
      nftId <- userMint w1 artwork2
      userQueryContent w1 $ mp'content artwork2

    content = mp'content artwork2
    msg = queryContentLog content $ QueryContent $ Just infoNft
    userId = toUserId w1
    infoNft = mintParamsToInfo artwork2 userId
