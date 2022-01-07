module Test.NFT.Trace (
  AppInitHandle,
  appInitTrace,
  mintFail1,
  mintTrace,
  setPriceTrace,
  testAny,
  testInit,
  testMint,
  testMint2,
  test,
  testAuction1,
  severalBuysTest,
  testGetContent2,
  testGetContent1,
  test2Admins,
) where

import PlutusTx.Prelude
import Prelude qualified as Hask

import Data.Default (def)
import Data.Monoid (Last (..))
import Data.Text (Text)

import Control.Monad (void)
import Control.Monad.Freer.Extras.Log as Extra (logInfo)

import Ledger.TimeSlot (slotToBeginPOSIXTime)
import Plutus.Trace.Emulator (EmulatorTrace, activateContractWallet, callEndpoint, runEmulatorTraceIO)
import Plutus.Trace.Emulator qualified as Trace
import Wallet.Emulator qualified as Emulator

import Mlabs.NFT.Api
import Mlabs.NFT.Contract.Aux (hashData)
import Mlabs.NFT.Spooky
import Mlabs.NFT.Types
import Mlabs.Utils.Wallet (walletFromNumber)

-- | Generic application Trace Handle.
type AppTraceHandle = Trace.ContractHandle UserWriter NFTAppSchema Text

-- | Initialisation Trace Handle.
type AppInitHandle = Trace.ContractHandle (Last NftAppInstance) NFTAppSchema Text

-- | Initialise the Application
appInitTrace :: EmulatorTrace NftAppInstance
appInitTrace = do
  let admin = walletFromNumber 4 :: Emulator.Wallet
  let params = InitParams [UserId . toSpooky . Emulator.walletPubKeyHash $ admin] (5 % 1000) (Emulator.walletPubKeyHash admin)
  hAdmin :: AppInitHandle <- activateContractWallet admin adminEndpoints
  callEndpoint @"app-init" hAdmin params
  void $ Trace.waitNSlots 3
  oState <- Trace.observableState hAdmin
  appInstace <- case getLast oState of
    Nothing -> Trace.throwError $ Trace.GenericError "App Instance Could not be established."
    Just aS -> return aS
  void $ Trace.waitNSlots 1
  return appInstace

mintTrace :: UniqueToken -> Emulator.Wallet -> EmulatorTrace NftId
mintTrace aSymb wallet = do
  h1 :: AppTraceHandle <- activateContractWallet wallet $ endpoints aSymb
  callEndpoint @"mint" h1 artwork
  void $ Trace.waitNSlots 1
  return . NftId . toSpooky . hashData . mp'content $ artwork
  where
    artwork =
      MintParams
        { mp'content = Content . toSpooky @BuiltinByteString $ "A painting."
        , mp'title = Title . toSpooky @BuiltinByteString $ "Fiona Lisa"
        , mp'share = 1 % 10
        , mp'price = Just 5
        }

-- | Emulator Trace 1. Mints one NFT.
mint1Trace :: EmulatorTrace ()
mint1Trace = do
  appInstance <- appInitTrace
  let uniqueToken = appInstance'UniqueToken appInstance
      wallet1 = walletFromNumber 1 :: Emulator.Wallet
  h1 :: AppTraceHandle <- activateContractWallet wallet1 $ endpoints uniqueToken

  callEndpoint @"mint" h1 artwork
  void $ Trace.waitNSlots 1
  where
    artwork =
      MintParams
        { mp'content = Content . toSpooky @BuiltinByteString $ "A painting."
        , mp'title = Title . toSpooky @BuiltinByteString $ "Fiona Lisa"
        , mp'share = 1 % 10
        , mp'price = Just 5
        }

getContentTrace1 :: EmulatorTrace ()
getContentTrace1 = do
  appInstance <- appInitTrace
  let uniqueToken = appInstance'UniqueToken appInstance
  let wallet1 = walletFromNumber 1 :: Emulator.Wallet
  h1 :: AppTraceHandle <- activateContractWallet wallet1 $ endpoints uniqueToken

  callEndpoint @"mint" h1 artwork
  void $ Trace.waitNSlots 1

  h1' :: AppTraceHandle <- activateContractWallet wallet1 $ queryEndpoints uniqueToken

  callEndpoint @"query-content" h1' $ Content . toSpooky @BuiltinByteString $ "A painting."
  void $ Trace.waitNSlots 1

  oState <- Trace.observableState h1'
  void $ Trace.waitNSlots 1
  logInfo $ Hask.show oState
  void $ Trace.waitNSlots 1
  where
    artwork =
      MintParams
        { mp'content = Content . toSpooky @BuiltinByteString $ "A painting."
        , mp'title = Title . toSpooky @BuiltinByteString $ "Fiona Lisa"
        , mp'share = 1 % 10
        , mp'price = Just 5
        }

-- | Two users mint two different artworks.
getContentTrace2 :: EmulatorTrace ()
getContentTrace2 = do
  appInstance <- appInitTrace
  let uniqueToken = appInstance'UniqueToken appInstance
  let wallet1 = walletFromNumber 1 :: Emulator.Wallet
  h1 :: AppTraceHandle <- activateContractWallet wallet1 $ endpoints uniqueToken
  void $ Trace.waitNSlots 1
  h1' :: AppTraceHandle <- activateContractWallet wallet1 $ queryEndpoints uniqueToken

  callEndpoint @"mint" h1 artwork
  void $ Trace.waitNSlots 1
  callEndpoint @"mint" h1 artwork2
  void $ Trace.waitNSlots 1
  callEndpoint @"mint" h1 artwork3
  void $ Trace.waitNSlots 1
  callEndpoint @"query-content" h1' $ Content . toSpooky @BuiltinByteString $ "A painting."
  void $ Trace.waitNSlots 1
  oState <- Trace.observableState h1'
  void $ Trace.waitNSlots 1
  logInfo $ Hask.show oState
  void $ Trace.waitNSlots 1
  where
    artwork =
      MintParams
        { mp'content = Content . toSpooky @BuiltinByteString $ "A painting."
        , mp'title = Title . toSpooky @BuiltinByteString $ "Fiona Lisa"
        , mp'share = 1 % 10
        , mp'price = Just 5
        }
    artwork2 =
      MintParams
        { mp'content = Content . toSpooky @BuiltinByteString $ "Another painting."
        , mp'title = Title . toSpooky @BuiltinByteString $ "Fiona Lisa"
        , mp'share = 1 % 10
        , mp'price = Just 5
        }
    artwork3 =
      MintParams
        { mp'content = Content . toSpooky @BuiltinByteString $ "Another painting2."
        , mp'title = Title . toSpooky @BuiltinByteString $ "Fiona Lisa"
        , mp'share = 1 % 10
        , mp'price = Just 5
        }

-- | Two users mint two different artworks.
mintTrace2 :: EmulatorTrace ()
mintTrace2 = do
  appInstance <- appInitTrace
  let uniqueToken = appInstance'UniqueToken appInstance
  let wallet1 = walletFromNumber 1 :: Emulator.Wallet
  h1 :: AppTraceHandle <- activateContractWallet wallet1 $ endpoints uniqueToken
  callEndpoint @"mint" h1 artwork
  void $ Trace.waitNSlots 1
  callEndpoint @"mint" h1 artwork2
  void $ Trace.waitNSlots 1
  where
    artwork =
      MintParams
        { mp'content = Content . toSpooky @BuiltinByteString $ "A painting."
        , mp'title = Title . toSpooky @BuiltinByteString $ "Fiona Lisa"
        , mp'share = 1 % 10
        , mp'price = Just 5
        }
    artwork2 =
      MintParams
        { mp'content = Content . toSpooky @BuiltinByteString $ "Another painting."
        , mp'title = Title . toSpooky @BuiltinByteString $ "Fiona Lisa"
        , mp'share = 1 % 10
        , mp'price = Just 5
        }

findNftId :: forall a b. Last (Either a b) -> Maybe a
findNftId x = case getLast x of
  Just (Left x') -> Just x'
  _ -> Nothing

-- | Two users mint the same artwork.  Should Fail
mintFail1 :: EmulatorTrace ()
mintFail1 = do
  appInstance <- appInitTrace
  let uniqueToken = appInstance'UniqueToken appInstance
  let wallet1 = walletFromNumber 1 :: Emulator.Wallet
  h1 :: AppTraceHandle <- activateContractWallet wallet1 $ endpoints uniqueToken
  callEndpoint @"mint" h1 artwork
  void $ Trace.waitNSlots 1
  callEndpoint @"mint" h1 artwork
  where
    artwork =
      MintParams
        { mp'content = Content . toSpooky @BuiltinByteString $ "A painting."
        , mp'title = Title . toSpooky @BuiltinByteString $ "Fiona Lisa"
        , mp'share = 1 % 10
        , mp'price = Just 5
        }

-- | Emulator Trace 1. Mints one NFT.
eTrace1 :: EmulatorTrace ()
eTrace1 = do
  let wallet1 = walletFromNumber 1 :: Emulator.Wallet
      wallet2 = walletFromNumber 2 :: Emulator.Wallet

  appInstance <- appInitTrace
  let uniqueToken = appInstance'UniqueToken appInstance

  h1 :: AppTraceHandle <- activateContractWallet wallet1 $ endpoints uniqueToken
  h2 :: AppTraceHandle <- activateContractWallet wallet2 $ endpoints uniqueToken
  callEndpoint @"mint" h1 artwork
  -- callEndpoint @"mint" h2 artwork2
  void $ Trace.waitNSlots 1
  oState <- Trace.observableState h1
  nftId <- case findNftId oState of
    Nothing -> Trace.throwError (Trace.GenericError "NftId not found")
    Just nid -> return nid
  void $ Trace.waitNSlots 1
  callEndpoint @"buy" h2 (buyParams nftId)

  logInfo @Hask.String $ Hask.show oState
  where
    --  callEndpoint @"mint" h1 artwork
    artwork =
      MintParams
        { mp'content = Content . toSpooky @BuiltinByteString $ "A painting."
        , mp'title = Title . toSpooky @BuiltinByteString $ "Fiona Lisa"
        , mp'share = 1 % 10
        , mp'price = Just 5
        }
    buyParams nftId = BuyRequestUser nftId 6 (Just 200)

severalBuysTrace :: EmulatorTrace ()
severalBuysTrace = do
  let wallet1 = walletFromNumber 1 :: Emulator.Wallet
      wallet2 = walletFromNumber 2 :: Emulator.Wallet
      wallet3 = walletFromNumber 3 :: Emulator.Wallet

  appInstance <- appInitTrace
  let uniqueToken = appInstance'UniqueToken appInstance

  h1 :: AppTraceHandle <- activateContractWallet wallet1 $ endpoints uniqueToken
  h2 :: AppTraceHandle <- activateContractWallet wallet2 $ endpoints uniqueToken
  h3 :: AppTraceHandle <- activateContractWallet wallet3 $ endpoints uniqueToken
  callEndpoint @"mint" h1 artwork
  -- callEndpoint @"mint" h2 artwork2
  void $ Trace.waitNSlots 1
  oState <- Trace.observableState h1
  nftId <- case findNftId oState of
    Nothing -> Trace.throwError (Trace.GenericError "NftId not found")
    Just nid -> return nid
  void $ Trace.waitNSlots 1
  callEndpoint @"buy" h2 (buyParams nftId 6)
  void $ Trace.waitNSlots 1
  callEndpoint @"buy" h3 (buyParams nftId 200)
  void $ Trace.waitNSlots 1
  callEndpoint @"set-price" h2 (SetPriceParams nftId (Just 20))
  where
    -- logInfo @Hask.String $ Hask.show oState

    artwork =
      MintParams
        { mp'content = Content . toSpooky @BuiltinByteString $ "A painting."
        , mp'title = Title . toSpooky @BuiltinByteString $ "Fiona Lisa"
        , mp'share = 1 % 10
        , mp'price = Just 5
        }
    buyParams nftId bid = BuyRequestUser nftId bid (Just 200)

setPriceTrace :: EmulatorTrace ()
setPriceTrace = do
  let wallet1 = walletFromNumber 1 :: Emulator.Wallet
      wallet2 = walletFromNumber 5 :: Emulator.Wallet
  authMintH <- activateContractWallet wallet1 (endpoints $ error ())
  void $ Trace.waitNSlots 2
  oState <- Trace.observableState authMintH
  nftId <- case findNftId oState of
    Nothing -> Trace.throwError (Trace.GenericError "NftId not found")
    Just nid -> return nid
  logInfo $ Hask.show nftId
  void $ Trace.waitNSlots 1
  authUseH :: AppTraceHandle <- activateContractWallet wallet1 (endpoints $ error ())
  callEndpoint @"set-price" authUseH (SetPriceParams nftId (Just 20))
  void $ Trace.waitNSlots 1
  callEndpoint @"set-price" authUseH (SetPriceParams nftId (Just (-20)))
  void $ Trace.waitNSlots 1
  userUseH :: AppTraceHandle <- activateContractWallet wallet2 (endpoints $ error ())
  callEndpoint @"set-price" userUseH (SetPriceParams nftId Nothing)
  void $ Trace.waitNSlots 1
  callEndpoint @"set-price" userUseH (SetPriceParams nftId (Just 30))
  void $ Trace.waitNSlots 1

-- queryPriceTrace :: EmulatorTrace ()
-- queryPriceTrace = do
--   let wallet1 = walletFromNumber 1 :: Emulator.Wallet
--       wallet2 = walletFromNumber 5 :: Emulator.Wallet
--   authMintH :: AppTraceHandle <- activateContractWallet wallet1 endpoints
--   callEndpoint @"mint" authMintH artwork
--   void $ Trace.waitNSlots 2
--   oState <- Trace.observableState authMintH
--   nftId <- case getLast oState of
--     Nothing -> Trace.throwError (Trace.GenericError "NftId not found")
--     Just nid -> return nid
--   logInfo $ Hask.show nftId
--   void $ Trace.waitNSlots 1

--   authUseH <- activateContractWallet wallet1 endpoints
--   callEndpoint @"set-price" authUseH (SetPriceParams nftId (Just 20))
--   void $ Trace.waitNSlots 2

--   queryHandle <- activateContractWallet wallet2 queryEndpoints
--   callEndpoint @"query-current-price" queryHandle nftId
--   -- hangs if this is not called before `observableState`
--   void $ Trace.waitNSlots 1
--   queryState <- Trace.observableState queryHandle
--   queriedPrice <- case getLast queryState of
--     Nothing -> Trace.throwError (Trace.GenericError "QueryResponse not found")
--     Just resp -> case resp of
--       QueryCurrentOwner _ -> Trace.throwError (Trace.GenericError "wrong query state, got owner instead of price")
--       QueryCurrentPrice price -> return price
--   logInfo $ "Queried price: " <> Hask.show queriedPrice

--   callEndpoint @"query-current-owner" queryHandle nftId
--   void $ Trace.waitNSlots 1
--   queryState2 <- Trace.observableState queryHandle
--   queriedOwner <- case getLast queryState2 of
--     Nothing -> Trace.throwError (Trace.GenericError "QueryResponse not found")
--     Just resp -> case resp of
--       QueryCurrentOwner owner -> return owner
--       QueryCurrentPrice _ -> Trace.throwError (Trace.GenericError "wrong query state, got price instead of owner")
--   logInfo $ "Queried owner: " <> Hask.show queriedOwner

--   void $ Trace.waitNSlots 1
--   where
--     artwork =
--       MintParams
--         { mp'content = Content "A painting."
--         , mp'title = Title "Fiona Lisa"
--         , mp'share = 1 % 10
--         , mp'price = Just 100
--         }

-- | Test for initialising the App
testInit :: Hask.IO ()
testInit = runEmulatorTraceIO $ void appInitTrace

-- | Test for Minting one token
testMint :: Hask.IO ()
testMint = runEmulatorTraceIO mint1Trace

testMint2 :: Hask.IO ()
testMint2 = runEmulatorTraceIO mintTrace2

test2Admins :: Hask.IO ()
test2Admins = runEmulatorTraceIO mintTrace2

testAny :: EmulatorTrace () -> Hask.IO ()
testAny = runEmulatorTraceIO

testGetContent1 :: Hask.IO ()
testGetContent1 = runEmulatorTraceIO getContentTrace1
testGetContent2 :: Hask.IO ()
testGetContent2 = runEmulatorTraceIO getContentTrace2

auctionTrace1 :: EmulatorTrace ()
auctionTrace1 = do
  appInstance <- appInitTrace
  let uniqueToken = appInstance'UniqueToken appInstance
  let wallet1 = walletFromNumber 1 :: Emulator.Wallet
      wallet2 = walletFromNumber 2 :: Emulator.Wallet
      wallet3 = walletFromNumber 3 :: Emulator.Wallet
  h1 :: AppTraceHandle <- activateContractWallet wallet1 $ endpoints uniqueToken
  h2 :: AppTraceHandle <- activateContractWallet wallet2 $ endpoints uniqueToken
  h3 :: AppTraceHandle <- activateContractWallet wallet3 $ endpoints uniqueToken
  callEndpoint @"mint" h1 artwork

  void $ Trace.waitNSlots 1
  oState <- Trace.observableState h1
  nftId <- case findNftId oState of
    Nothing -> Trace.throwError (Trace.GenericError "NftId not found")
    Just nid -> return nid

  logInfo @Hask.String $ Hask.show oState
  void $ Trace.waitNSlots 1

  callEndpoint @"auction-open" h1 (openParams nftId)
  void $ Trace.waitNSlots 1

  -- callEndpoint @"set-price" h1 (SetPriceParams nftId (Just 20))
  -- void $ Trace.waitNSlots 1

  callEndpoint @"auction-bid" h2 (bidParams nftId 1111110)
  void $ Trace.waitNSlots 1

  callEndpoint @"auction-bid" h3 (bidParams nftId 77777700)
  void $ Trace.waitNSlots 2
  -- void $ Trace.waitNSlots 12

  callEndpoint @"auction-close" h1 (closeParams nftId)
  void $ Trace.waitNSlots 2

  callEndpoint @"set-price" h3 (SetPriceParams nftId (Just 20))
  void $ Trace.waitNSlots 5

  logInfo @Hask.String "auction1 test end"
  where
    artwork =
      MintParams
        { mp'content = Content . toSpooky @BuiltinByteString $ "A painting."
        , mp'title = Title . toSpooky @BuiltinByteString $ "Fiona Lisa"
        , mp'share = 1 % 10
        , mp'price = Just 5
        }

    slotTenTime = slotToBeginPOSIXTime def 10
    openParams nftId = AuctionOpenParams nftId slotTenTime 400
    closeParams nftId = AuctionCloseParams nftId
    bidParams = AuctionBidParams

-- | Test for prototyping.
test :: Hask.IO ()
test = runEmulatorTraceIO eTrace1

severalBuysTest :: Hask.IO ()
severalBuysTest = runEmulatorTraceIO severalBuysTrace

-- testSetPrice :: Hask.IO ()
-- testSetPrice = runEmulatorTraceIO setPriceTrace

-- testQueryPrice :: Hask.IO ()
-- testQueryPrice = runEmulatorTraceIO queryPriceTrace

testAuction1 :: Hask.IO ()
testAuction1 = runEmulatorTraceIO auctionTrace1
