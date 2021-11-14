module Test.NFT.Contract (
  test,
) where

import Data.Aeson (Value (..))
import Data.List (sortOn)
import Data.Text qualified as T
import Ledger.Crypto (pubKeyHash)
import Ledger.Scripts (ScriptError (..))
import Ledger.Index (ValidationError (..))
import Plutus.Contract.Test (assertInstanceLog, assertFailedTransaction)
import Plutus.Contract.Trace (walletPubKeyHash)
import Plutus.Trace.Emulator.Types (ContractInstanceLog (..), ContractInstanceMsg (..), walletInstanceTag)
import PlutusTx.Prelude hiding (check, mconcat)
import Test.Tasty (TestTree, testGroup)
import Wallet.Emulator.MultiAgent (EmulatorTimeEvent (..))
import Prelude (mconcat)
import Prelude qualified as Hask

import Mlabs.Emulator.Scene (checkScene)
import Mlabs.NFT.Contract.Aux (hashData)
import Mlabs.NFT.Contract.Mint (mintParamsToInfo)
import Mlabs.NFT.Contract.Query (queryContentLog, queryCurrentOwnerLog, queryCurrentPriceLog, queryListNftsLog)
import Mlabs.NFT.Types (
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
  check,
  noChangesScene,
  ownsAda,
  callStartNftFail,
  userBuy,
  userMint,
  userQueryContent,
  userQueryListNfts,
  userQueryOwner,
  userQueryPrice,
  userSetPrice,
  w1,
  w2,
  w3,
  wA,
  toUserId,
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
    , testQueryPrice
    , testQueryOwner
    , testQueryListNfts
    , testQueryContent
    ]

-- | Test initialisation of an app instance
testInitApp :: TestTree
testInitApp = check "Init app" assertState wA script
  where
    script = callStartNftFail wA
    assertState = assertFailedTransaction
      (\_ vEr _ ->
        case vEr of
          (ScriptFailure (EvaluationError (er:_) _)) -> msg Hask.== T.unpack er
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

-- | User checks the price of the artwork.
testQueryPrice :: TestTree
testQueryPrice = check "Query price" assertState w1 script
  where
    script = do
      nftId <- userMint w1 artwork2
      userQueryPrice w1 nftId

    assertState = assertInstanceLog (walletInstanceTag w1) (any $ queryLogPredicate msg)
      where
        nftId = NftId . hashData . mp'content $ artwork2
        price = QueryCurrentPrice . mp'price $ artwork2
        msg = queryCurrentPriceLog nftId price

testQueryOwner :: TestTree
testQueryOwner = check "Query owner" assertState w1 script
  where
    script = do
      nftId <- userMint w1 artwork2
      userQueryOwner w1 nftId

    assertState = assertInstanceLog (walletInstanceTag w1) (any $ queryLogPredicate msg)
      where
        nftId = NftId . hashData . mp'content $ artwork2
        owner = QueryCurrentOwner . Just . toUserId $ w1
        msg = queryCurrentOwnerLog nftId owner

-- | User lists all NFTs in app
testQueryListNfts :: TestTree
testQueryListNfts = check "Query list NFTs" assertState w1 script
  where
    script = do
      mapM_ (userMint w1) artworks
      userQueryListNfts w1

    assertState = assertInstanceLog (walletInstanceTag w1) (any $ queryLogPredicate msg)
      where
        msg = queryListNftsLog nfts

    artworks = [artwork1, artwork2]

    nfts =
      sortOn info'id
        . fmap (\mp -> mintParamsToInfo mp (toUserId w1))
        $ artworks

testQueryContent :: TestTree
testQueryContent = check "Query content" assertState w1 script
  where
    script = do
      nftId <- userMint w1 artwork2
      userQueryContent w1 $ mp'content artwork2

    assertState = assertInstanceLog (walletInstanceTag w1) (any $ queryLogPredicate msg)
      where
        content = mp'content artwork2
        msg = queryContentLog content $ QueryContent $ Just infoNft
        userId = toUserId w1
        infoNft = mintParamsToInfo artwork2 userId

{- | Predicate that checks that the last logged message by the contract is the
 same as a given message.
-}
queryLogPredicate :: Hask.String -> EmulatorTimeEvent ContractInstanceLog -> Bool
queryLogPredicate msg = \case
  (EmulatorTimeEvent _ (ContractInstanceLog (ContractLog (String str)) _ _)) -> T.pack msg Hask.== str
  _ -> False
