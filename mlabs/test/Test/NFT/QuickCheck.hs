{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Test.NFT.QuickCheck where

import Control.Lens (at, makeLenses, set, view, (^.))
import Control.Monad (forM_, void, when)
import Data.Default (def)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Monoid (Last (..))
import Data.String (IsString (..))
import Data.Text (Text)
import Ledger.TimeSlot (slotToBeginPOSIXTime)
import Plutus.Contract.Test (Wallet (..), walletPubKeyHash)
import Plutus.Contract.Test.ContractModel (
  Action,
  Actions,
  ContractInstanceSpec (..),
  ContractModel (..),
  DL,
  action,
  anyActions_,
  assertModel,
  contractState,
  currentSlot,
  deposit,
  forAllDL,
  getModelState,
  lockedValue,
  propRunActionsWithOptions,
  transfer,
  viewContractState,
  wait,
  withdraw,
  ($=),
  ($~),
 )
import Plutus.Trace.Emulator (callEndpoint)
import Plutus.Trace.Emulator qualified as Trace
import Plutus.V1.Ledger.Ada (lovelaceValueOf)
import Plutus.V1.Ledger.Slot (Slot (..))
import Plutus.V1.Ledger.Value (valueOf)
import PlutusTx.Prelude hiding ((<$>), (<*>), (==))
import Test.QuickCheck qualified as QC
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Prelude ((<$>), (<*>), (==))
import Prelude qualified as Hask

import Mlabs.NFT.Api (NFTAppSchema, adminEndpoints, endpoints)
import Mlabs.NFT.Contract (hashData)
import Mlabs.NFT.Spooky (toSpooky)
import Mlabs.NFT.Types (
  AuctionBidParams (..),
  AuctionCloseParams (..),
  AuctionOpenParams (..),
  BuyRequestUser (..),
  Content (..),
  InitParams (..),
  MintParams (..),
  NftAppInstance,
  NftId (..),
  QueryResponse,
  SetPriceParams (..),
  Title (..),
 )
import Mlabs.NFT.Validation (calculateShares)
import Test.NFT.Init (appSymbol, checkOptions, mkFreeGov, toUserId, w1, w2, w3, wA)

data MockAuctionState = MockAuctionState
  { _auctionHighestBid :: Maybe (Integer, Wallet)
  , _auctionDeadline :: Slot
  , _auctionMinBid :: Integer
  }
  deriving (Hask.Show, Hask.Eq)
makeLenses ''MockAuctionState

-- Mock content for overriding Show instance, to show hash instead, useful for debugging
newtype MockContent = MockContent {getMockContent :: Content}
  deriving (Hask.Eq)

instance Hask.Show MockContent where
  show x = Hask.show (hashData . getMockContent $ x, getMockContent x)

-- We cannot use InformationNft because we need access to `Wallet`
-- `PubKeyHash` is not enough for simulation
data MockNft = MockNft
  { _nftId :: NftId
  , _nftPrice :: Maybe Integer
  , _nftOwner :: Wallet
  , _nftAuthor :: Wallet
  , _nftShare :: Rational
  , _nftAuctionState :: Maybe MockAuctionState
  }
  deriving (Hask.Show, Hask.Eq)
makeLenses ''MockNft

data NftModel = NftModel
  { _mMarket :: Map NftId MockNft
  , _mMintedCount :: Integer
  , _mStarted :: Bool
  }
  deriving (Hask.Show, Hask.Eq)
makeLenses ''NftModel

instance ContractModel NftModel where
  data Action NftModel
    = ActionInit
    | ActionMint
        { aPerformer :: Wallet
        , aContent :: MockContent
        , aTitle :: Title
        , aNewPrice :: Maybe Integer
        , aShare :: Rational
        }
    | ActionSetPrice
        { aPerformer :: Wallet
        , aNftId :: ~NftId
        , aNewPrice :: Maybe Integer
        }
    | ActionBuy
        { aPerformer :: Wallet
        , aNftId :: ~NftId
        , aPrice :: Integer
        , aNewPrice :: Maybe Integer
        }
    | ActionAuctionOpen
        { aPerformer :: Wallet
        , aNftId :: ~NftId
        , aDeadline :: Slot
        , aMinBid :: Integer
        }
    | ActionAuctionBid
        { aPerformer :: Wallet
        , aNftId :: ~NftId
        , aBid :: Integer
        }
    | ActionAuctionClose
        { aPerformer :: Wallet
        , aNftId :: ~NftId
        }
    | ActionWait -- This action should not be generated (do NOT add it to arbitraryAction)
        { aWaitSlots :: Integer
        }
    deriving (Hask.Show, Hask.Eq)

  data ContractInstanceKey NftModel w s e where
    InitKey :: Wallet -> ContractInstanceKey NftModel (Last NftAppInstance) NFTAppSchema Text
    UserKey :: Wallet -> ContractInstanceKey NftModel (Last (Either NftId QueryResponse)) NFTAppSchema Text

  instanceTag key _ = fromString $ Hask.show key

  arbitraryAction model =
    let nfts = Map.keys (model ^. contractState . mMarket)
        genWallet = QC.elements wallets
        genNonNeg = ((* 1_000_000) . (+ 1)) . QC.getNonNegative <$> QC.arbitrary
        genDeadline = Slot . QC.getNonNegative <$> QC.arbitrary
        -- genDeadline = Hask.pure @QC.Gen (Slot 9999)
        genMaybePrice = QC.oneof [Hask.pure Nothing, Just <$> genNonNeg]
        genString = QC.listOf (QC.elements [Hask.minBound .. Hask.maxBound])
        genContent = MockContent . Content . toSpooky @BuiltinByteString . fromString . ('x' :) <$> genString
        -- genTitle = Title . fromString <$> genString
        genTitle = Hask.pure (Title . toSpooky @BuiltinByteString $ "")
        genShare = (% 100) <$> QC.elements [1 .. 99]
        genNftId = QC.elements nfts
     in QC.oneof
          [ Hask.pure ActionInit
          , ActionMint
              <$> genWallet
              <*> genContent
              <*> genTitle
              <*> genMaybePrice
              <*> genShare
          , ActionSetPrice
              <$> genWallet
              <*> genNftId
              <*> genMaybePrice
          , ActionBuy
              <$> genWallet
              <*> genNftId
              <*> genNonNeg
              <*> genMaybePrice
              -- , ActionAuctionOpen
              --     <$> genWallet
              --     <*> genNftId
              --     <*> genDeadline
              --     <*> genNonNeg
              -- , ActionAuctionBid
              --     <$> genWallet
              --     <*> genNftId
              --     <*> genNonNeg
              -- , ActionAuctionClose
              --     <$> genWallet
              --     <*> genNftId
          ]

  initialState = NftModel Map.empty 0 False

  precondition s ActionInit {} = not (s ^. contractState . mStarted)
  precondition s ActionMint {..} =
    (s ^. contractState . mStarted)
      && (s ^. contractState . mMintedCount <= 5)
      && not (Map.member (NftId . toSpooky . hashData . getMockContent $ aContent) (s ^. contractState . mMarket))
  precondition s ActionBuy {..} =
    (s ^. contractState . mStarted)
      && (s ^. contractState . mMintedCount > 0)
      && isJust ((s ^. contractState . mMarket . at aNftId) >>= _nftPrice)
      && (Just aPrice >= ((s ^. contractState . mMarket . at aNftId) >>= _nftPrice))
  precondition s ActionSetPrice {..} =
    (s ^. contractState . mStarted)
      && (s ^. contractState . mMintedCount > 0)
      && (Just aPerformer == (view nftOwner <$> (s ^. contractState . mMarket . at aNftId)))
      && isNothing ((s ^. contractState . mMarket . at aNftId) >>= _nftAuctionState)
  precondition s ActionAuctionOpen {..} =
    (s ^. contractState . mStarted)
      && (s ^. contractState . mMintedCount > 0)
      && (Just aPerformer == (view nftOwner <$> (s ^. contractState . mMarket . at aNftId)))
      && isNothing ((s ^. contractState . mMarket . at aNftId) >>= _nftAuctionState)
  precondition s ActionAuctionBid {..} =
    (s ^. contractState . mStarted)
      && (s ^. contractState . mMintedCount > 0)
      && isJust ((s ^. contractState . mMarket . at aNftId) >>= _nftAuctionState)
      && (Just aBid > (view auctionMinBid <$> ((s ^. contractState . mMarket . at aNftId) >>= _nftAuctionState)))
      && (Just aBid > (fst <$> ((s ^. contractState . mMarket . at aNftId) >>= _nftAuctionState >>= _auctionHighestBid)))
      && (Just (s ^. currentSlot + 1) < (view auctionDeadline <$> ((s ^. contractState . mMarket . at aNftId) >>= _nftAuctionState)))
  precondition s ActionAuctionClose {..} =
    (s ^. contractState . mStarted)
      && (s ^. contractState . mMintedCount > 0)
      && isJust ((s ^. contractState . mMarket . at aNftId) >>= _nftAuctionState)
      && (Just (s ^. currentSlot) > (view auctionDeadline <$> ((s ^. contractState . mMarket . at aNftId) >>= _nftAuctionState)))
  precondition _ ActionWait {} = True

  nextState ActionInit {} = do
    mStarted $= True
    wait 5
  nextState ActionMint {..} = do
    s <- view contractState <$> getModelState
    let nft =
          MockNft
            { _nftId = NftId . toSpooky . hashData . getMockContent $ aContent
            , _nftPrice = aNewPrice
            , _nftOwner = aPerformer
            , _nftAuthor = aPerformer
            , _nftShare = aShare
            , _nftAuctionState = Nothing
            }
    let nft' = s ^. mMarket . at (nft ^. nftId)
    case nft' of
      Nothing -> do
        mMarket $~ Map.insert (nft ^. nftId) nft
        mMintedCount $~ (+ 1)
      Just _ -> Hask.pure () -- NFT is already minted
    wait 5
  nextState ActionSetPrice {..} = do
    s <- view contractState <$> getModelState
    let nft' = s ^. mMarket . at aNftId
    case nft' of
      Nothing -> Hask.pure () -- NFT not found
      Just nft -> do
        when ((nft ^. nftOwner) == aPerformer && isNothing (nft ^. nftAuctionState)) $ do
          let newNft = set nftPrice aNewPrice nft
          mMarket $~ Map.insert aNftId newNft
    wait 5
  nextState ActionBuy {..} = do
    s <- view contractState <$> getModelState
    let nft' = s ^. mMarket . at aNftId
    case nft' of
      Nothing -> Hask.pure () -- NFT not found
      Just nft -> case nft ^. nftPrice of
        Nothing -> Hask.pure () -- NFT is locked
        Just nftPrice' -> do
          when (nftPrice' <= aPrice) $ do
            let newNft = set nftOwner aPerformer . set nftPrice aNewPrice $ nft
                feeValue = round $ fromInteger aPrice * feeRate
                (ownerShare, authorShare) = calculateShares (aPrice - feeValue) (nft ^. nftShare)
            mMarket $~ Map.insert aNftId newNft
            transfer aPerformer (nft ^. nftOwner) ownerShare
            transfer aPerformer (nft ^. nftAuthor) authorShare
            transfer aPerformer wAdmin (lovelaceValueOf feeValue)
            deposit aPerformer (mkFreeGov aPerformer feeValue)
    wait 5
  nextState ActionAuctionOpen {..} = do
    s <- view contractState <$> getModelState
    let nft' = s ^. mMarket . at aNftId
    case nft' of
      Nothing -> Hask.pure () -- NFT not found
      Just nft -> do
        when ((nft ^. nftOwner) == aPerformer && isNothing (nft ^. nftAuctionState)) $ do
          let ac =
                MockAuctionState
                  { _auctionHighestBid = Nothing
                  , _auctionDeadline = aDeadline
                  , _auctionMinBid = aMinBid
                  }
              newNft =
                set nftAuctionState (Just ac) $
                  set nftPrice Nothing nft
          mMarket $~ Map.insert aNftId newNft
    wait 5
  nextState ActionAuctionBid {..} = do
    s <- view contractState <$> getModelState
    curSlot <- view currentSlot <$> getModelState
    let nft' = s ^. mMarket . at aNftId
    case nft' of
      Nothing -> Hask.pure () -- NFT not found
      Just nft -> case nft ^. nftAuctionState of
        Nothing -> Hask.pure () -- NFT not on auction
        Just ac -> do
          when (ac ^. auctionDeadline > curSlot) $ do
            let newAc = set auctionHighestBid (Just (aBid, aPerformer)) ac
                newNft = set nftAuctionState (Just newAc) nft
            case ac ^. auctionHighestBid of
              Nothing -> do
                -- First bid
                when (ac ^. auctionMinBid <= aBid) $ do
                  mMarket $~ Map.insert aNftId newNft
                  withdraw aPerformer (lovelaceValueOf aBid)
              Just hb ->
                -- Next bid
                when (fst hb < aBid) $ do
                  mMarket $~ Map.insert aNftId newNft
                  deposit (snd hb) (lovelaceValueOf . fst $ hb)
                  withdraw aPerformer (lovelaceValueOf aBid)
    wait 10
  nextState ActionAuctionClose {..} = do
    s <- view contractState <$> getModelState
    curSlot <- view currentSlot <$> getModelState
    let nft' = s ^. mMarket . at aNftId
    case nft' of
      Nothing -> Hask.pure () -- NFT not found
      Just nft -> case nft ^. nftAuctionState of
        Nothing -> Hask.pure () -- NFT not on auction
        Just ac -> do
          when (ac ^. auctionDeadline < curSlot) $ do
            case ac ^. auctionHighestBid of
              Nothing -> do
                -- No bids
                let newNft = set nftAuctionState Nothing nft
                mMarket $~ Map.insert aNftId newNft
              Just hb -> do
                let newOwner = snd hb
                    newNft = set nftOwner newOwner $ set nftAuctionState Nothing nft
                    price = fst hb
                    feeValue = round $ fromInteger price * feeRate
                    (ownerShare, authorShare) = calculateShares (price - feeValue) (nft ^. nftShare)
                mMarket $~ Map.insert aNftId newNft
                deposit (nft ^. nftOwner) ownerShare
                deposit (nft ^. nftAuthor) authorShare
                deposit wAdmin (lovelaceValueOf feeValue)
                deposit newOwner (mkFreeGov newOwner feeValue)
    wait 5
  nextState ActionWait {..} = do
    wait aWaitSlots

  perform h _ = \case
    ActionInit -> do
      let hAdmin = h $ InitKey wAdmin
          params =
            InitParams
              [toUserId wAdmin]
              (5 % 1000)
              (walletPubKeyHash wAdmin)
      callEndpoint @"app-init" hAdmin params
      void $ Trace.waitNSlots 5
    ActionMint {..} -> do
      let h1 = h $ UserKey aPerformer
      callEndpoint @"mint" h1 $
        MintParams
          { mp'content = getMockContent aContent
          , mp'title = aTitle
          , mp'share = aShare
          , mp'price = aNewPrice
          }
      void $ Trace.waitNSlots 5
    ActionSetPrice {..} -> do
      let h1 = h $ UserKey aPerformer
      callEndpoint @"set-price" h1 $
        SetPriceParams
          { sp'nftId = aNftId
          , sp'price = aNewPrice
          }
      void $ Trace.waitNSlots 5
    ActionBuy {..} -> do
      let h1 = h $ UserKey aPerformer
      callEndpoint @"buy" h1 $
        BuyRequestUser
          { ur'nftId = aNftId
          , ur'newPrice = aNewPrice
          , ur'price = aPrice
          }
      void $ Trace.waitNSlots 5
    ActionAuctionOpen {..} -> do
      let h1 = h $ UserKey aPerformer
      callEndpoint @"auction-open" h1 $
        AuctionOpenParams
          { op'nftId = aNftId
          , op'deadline = slotToBeginPOSIXTime def aDeadline
          , op'minBid = aMinBid
          }
      void $ Trace.waitNSlots 5
    ActionAuctionBid {..} -> do
      let h1 = h $ UserKey aPerformer
      callEndpoint @"auction-bid" h1 $
        AuctionBidParams
          { bp'nftId = aNftId
          , bp'bidAmount = aBid
          }
      void $ Trace.waitNSlots 9
      callEndpoint @"query-list-nfts" h1 ()
      void $ Trace.waitNSlots 1
    ActionAuctionClose {..} -> do
      let h1 = h $ UserKey aPerformer
      callEndpoint @"auction-close" h1 $
        AuctionCloseParams
          { cp'nftId = aNftId
          }
      void $ Trace.waitNSlots 5
    ActionWait {..} -> do
      void . Trace.waitNSlots . Hask.fromInteger $ aWaitSlots

deriving instance Hask.Eq (ContractInstanceKey NftModel w s e)
deriving instance Hask.Show (ContractInstanceKey NftModel w s e)

feeRate :: Rational
feeRate = 5 % 1000

wallets :: [Wallet]
wallets = [w1, w2, w3]

wAdmin :: Wallet
wAdmin = wA

instanceSpec :: [ContractInstanceSpec NftModel]
instanceSpec =
  [ ContractInstanceSpec (InitKey wAdmin) wAdmin adminEndpoints
  ]
    <> Hask.fmap (\w -> ContractInstanceSpec (UserKey w) w (endpoints appSymbol)) wallets

propContract :: Actions NftModel -> QC.Property
propContract =
  -- HACK
  -- 10 test runs execute relatively quickly, which we can then
  -- run multiple times in 'test/Main.hs'
  QC.withMaxSuccess 10
    . propRunActionsWithOptions
      checkOptions
      instanceSpec
      (const $ Hask.pure True)

noLockedFunds :: DL NftModel ()
noLockedFunds = do
  action ActionInit
  anyActions_

  nfts <- viewContractState mMarket

  -- Wait for all auctions to end
  forM_ nfts $ \nft -> do
    case nft ^. nftAuctionState of
      Just as -> do
        action $ ActionWait $ getSlot (as ^. auctionDeadline)
      Nothing -> Hask.pure ()

  -- Close all auctions
  forM_ nfts $ \nft -> do
    case nft ^. nftAuctionState of
      Just _ -> do
        action $ ActionAuctionClose w1 (nft ^. nftId)
      Nothing -> Hask.pure ()

  assertModel "Locked funds should be zero" $ (== 0) . (\v -> valueOf v "" "") . lockedValue

propNoLockedFunds :: QC.Property
propNoLockedFunds = QC.withMaxSuccess 10 $ forAllDL noLockedFunds propContract

test :: TestTree
test =
  testGroup
    "QuickCheck"
    [ testProperty "Can get funds out" propNoLockedFunds
    , testProperty "Contract" propContract
    ]
