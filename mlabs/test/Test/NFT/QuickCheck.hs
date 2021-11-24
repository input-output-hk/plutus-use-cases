{-# LANGUAGE GADTs #-}

module Test.NFT.QuickCheck where

import Control.Lens (at, makeLenses, set, view, (^.))
import Control.Monad (void, when)
import Data.Default (def)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Monoid (Last (..))
import Data.String (IsString (..))
import Data.Text (Text)
import Ledger.TimeSlot (slotToBeginPOSIXTime)
import Plutus.Contract.Test (Wallet (..))
import Plutus.Contract.Test.ContractModel (
  Action,
  Actions,
  ContractInstanceSpec (..),
  ContractModel (..),
  contractState,
  currentSlot,
  deposit,
  getModelState,
  propRunActionsWithOptions,
  transfer,
  wait,
  withdraw,
  ($=),
  ($~),
 )
import Plutus.Trace.Emulator (activateContractWallet, callEndpoint)
import Plutus.Trace.Emulator qualified as Trace
import Plutus.V1.Ledger.Ada (lovelaceValueOf)
import Plutus.V1.Ledger.Slot (Slot (..))
import PlutusTx.Prelude hiding (fmap, length, mconcat, unless, (<$>), (<*>), (==))
import Test.QuickCheck qualified as QC
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Prelude ((<$>), (<*>), (==))
import Prelude qualified as Hask

import Mlabs.NFT.Api
import Mlabs.NFT.Contract
import Mlabs.NFT.Types
import Mlabs.NFT.Validation
import Test.NFT.Init

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
  show = Hask.show . hashData . getMockContent

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
        , aNftId :: NftId
        , aNewPrice :: Maybe Integer
        }
    | ActionBuy
        { aPerformer :: Wallet
        , aNftId :: NftId
        , aPrice :: Integer
        , aNewPrice :: Maybe Integer
        }
    | ActionAuctionOpen
        { aPerformer :: Wallet
        , aNftId :: NftId
        , aDeadline :: Slot
        , aMinBid :: Integer
        }
    | ActionAuctionBid
        { aPerformer :: Wallet
        , aNftId :: NftId
        , aBid :: Integer
        }
    | ActionAuctionClose
        { aPerformer :: Wallet
        , aNftId :: NftId
        }
    deriving (Hask.Show, Hask.Eq)

  data ContractInstanceKey NftModel w s e where
    InitKey :: Wallet -> ContractInstanceKey NftModel (Last NftAppSymbol) NFTAppSchema Text

  instanceTag key _ = fromString $ Hask.show key

  arbitraryAction model =
    let invalidNft = NftId "I AM INVALID"
        nfts = view nftId <$> Map.elems (model ^. contractState . mMarket)
        genWallet = QC.elements wallets
        genNonNeg = ((* 100) . (+ 1)) . QC.getNonNegative <$> QC.arbitrary
        genDeadline = Slot . QC.getNonNegative <$> QC.arbitrary
        genMaybePrice = QC.oneof [Hask.pure Nothing, Just <$> genNonNeg]
        genString = QC.listOf (QC.elements [Hask.minBound .. Hask.maxBound])
        genContent = MockContent . Content . fromString <$> genString
        genTitle = Title . fromString <$> genString
        genShare = (1 %) <$> QC.elements [2 .. 100] -- Shares like 1/2, 1/3 ... 1/100
        genNftId = QC.elements (invalidNft : nfts)
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
          , ActionAuctionOpen
              <$> genWallet
              <*> genNftId
              <*> genDeadline
              <*> genNonNeg
          , ActionAuctionBid
              <$> genWallet
              <*> genNftId
              <*> genNonNeg
          , ActionAuctionClose
              <$> genWallet
              <*> genNftId
          ]

  initialState = NftModel Map.empty 0 False

  precondition s ActionInit {} = not (s ^. contractState . mStarted)
  precondition s ActionMint {} = (s ^. contractState . mStarted) && (s ^. contractState . mMintedCount <= 5)
  precondition s _ = s ^. contractState . mStarted

  nextState ActionInit {} = do
    mStarted $= True
    wait 2
  nextState action@ActionMint {} = do
    s <- view contractState <$> getModelState
    let nft =
          MockNft
            { _nftId = NftId . hashData . getMockContent . aContent $ action
            , _nftPrice = aNewPrice action
            , _nftOwner = aPerformer action
            , _nftAuthor = aPerformer action
            , _nftShare = aShare action
            , _nftAuctionState = Nothing
            }
    let nft' = s ^. mMarket . at (nft ^. nftId)
    case nft' of
      Nothing -> do
        mMarket $~ Map.insert (nft ^. nftId) nft
      Just _ -> Hask.pure () -- NFT is already minted
    wait 2
  nextState action@ActionSetPrice {} = do
    s <- view contractState <$> getModelState
    let nft' = s ^. mMarket . at (aNftId action)
    case nft' of
      Nothing -> Hask.pure () -- NFT not found
      Just nft -> do
        when ((nft ^. nftOwner) == aPerformer action && isNothing (nft ^. nftAuctionState)) $ do
          let newNft = set nftPrice (aNewPrice action) nft
          mMarket $~ Map.insert (aNftId action) newNft
    wait 2
  nextState action@ActionBuy {} = do
    s <- view contractState <$> getModelState
    let nft' = s ^. mMarket . at (aNftId action)
    case nft' of
      Nothing -> Hask.pure () -- NFT not found
      Just nft -> case nft ^. nftPrice of
        Nothing -> Hask.pure () -- NFT is locked
        Just nftPrice' -> do
          when (nftPrice' <= aPrice action) $ do
            let newNft = set nftOwner (aPerformer action) . set nftPrice (aNewPrice action) $ nft
                (ownerShare, authorShare) = calculateShares (aPrice action) (nft ^. nftShare)
            mMarket $~ Map.insert (aNftId action) newNft
            transfer (aPerformer action) (nft ^. nftOwner) ownerShare
            transfer (aPerformer action) (nft ^. nftAuthor) authorShare
    wait 2
  nextState action@ActionAuctionOpen {} = do
    s <- view contractState <$> getModelState
    let nft' = s ^. mMarket . at (aNftId action)
    case nft' of
      Nothing -> Hask.pure () -- NFT not found
      Just nft -> do
        when ((nft ^. nftOwner) == aPerformer action && isNothing (nft ^. nftAuctionState)) $ do
          let ac =
                MockAuctionState
                  { _auctionHighestBid = Nothing
                  , _auctionDeadline = aDeadline action
                  , _auctionMinBid = aMinBid action
                  }
              newNft =
                set nftAuctionState (Just ac) $
                  set nftPrice Nothing nft
          mMarket $~ Map.insert (aNftId action) newNft
    wait 2
  nextState action@ActionAuctionBid {} = do
    s <- view contractState <$> getModelState
    curSlot <- view currentSlot <$> getModelState
    let nft' = s ^. mMarket . at (aNftId action)
    case nft' of
      Nothing -> Hask.pure () -- NFT not found
      Just nft -> case nft ^. nftAuctionState of
        Nothing -> Hask.pure () -- NFT not on auction
        Just ac -> do
          when (ac ^. auctionDeadline > curSlot) $ do
            let newAc = set auctionHighestBid (Just (aBid action, aPerformer action)) ac
                newNft = set nftAuctionState (Just newAc) nft
            case ac ^. auctionHighestBid of
              Nothing -> do
                -- First bid
                when (ac ^. auctionMinBid <= aBid action) $ do
                  mMarket $~ Map.insert (aNftId action) newNft
                  withdraw (aPerformer action) (lovelaceValueOf . aBid $ action)
              Just hb ->
                -- Next bid
                when (fst hb < aBid action) $ do
                  mMarket $~ Map.insert (aNftId action) newNft
                  deposit (snd hb) (lovelaceValueOf . fst $ hb)
                  withdraw (aPerformer action) (lovelaceValueOf . aBid $ action)
    wait 2
  nextState action@ActionAuctionClose {} = do
    s <- view contractState <$> getModelState
    curSlot <- view currentSlot <$> getModelState
    let nft' = s ^. mMarket . at (aNftId action)
    case nft' of
      Nothing -> Hask.pure () -- NFT not found
      Just nft -> case nft ^. nftAuctionState of
        Nothing -> Hask.pure () -- NFT not on auction
        Just ac -> do
          when (ac ^. auctionDeadline < curSlot && nft ^. nftOwner == aPerformer action) $ do
            case ac ^. auctionHighestBid of
              Nothing -> do
                -- No bids
                let newNft = set nftAuctionState Nothing nft
                mMarket $~ Map.insert (aNftId action) newNft
              Just hb -> do
                let newNft = set nftOwner (snd hb) $ set nftAuctionState Nothing nft
                    (ownerShare, authorShare) = calculateShares (aPrice action) (nft ^. nftShare)
                mMarket $~ Map.insert (aNftId action) newNft
                deposit (nft ^. nftOwner) ownerShare
                deposit (nft ^. nftAuthor) authorShare
    wait 2

  perform h _ = \case
    ActionInit {} -> do
      let hAdmin = h $ InitKey wAdmin
      callEndpoint @"app-init" hAdmin [toUserId wAdmin]
      waitInit
      void getSymbol
    action@ActionMint {} -> do
      aSymbol <- getSymbol
      h1 <- activateContractWallet (aPerformer action) $ endpoints aSymbol
      callEndpoint @"mint" h1 $
        MintParams
          { mp'content = getMockContent $ aContent action
          , mp'title = aTitle action
          , mp'share = aShare action
          , mp'price = aNewPrice action
          }
      void $ Trace.waitNSlots 2
    action@ActionSetPrice {} -> do
      aSymbol <- getSymbol
      h1 <- activateContractWallet (aPerformer action) $ endpoints aSymbol
      callEndpoint @"set-price" h1 $
        SetPriceParams
          { sp'nftId = aNftId action
          , sp'price = aNewPrice action
          }
      void $ Trace.waitNSlots 2
    action@ActionBuy {} -> do
      aSymbol <- getSymbol
      h1 <- activateContractWallet (aPerformer action) $ endpoints aSymbol
      callEndpoint @"buy" h1 $
        BuyRequestUser
          { ur'nftId = aNftId action
          , ur'newPrice = aNewPrice action
          , ur'price = aPrice action
          }
      void $ Trace.waitNSlots 2
    action@ActionAuctionOpen {} -> do
      aSymbol <- getSymbol
      h1 <- activateContractWallet (aPerformer action) $ endpoints aSymbol
      callEndpoint @"auction-open" h1 $
        AuctionOpenParams
          { op'nftId = aNftId action
          , op'deadline = slotToBeginPOSIXTime def . aDeadline $ action
          , op'minBid = aMinBid action
          }
      void $ Trace.waitNSlots 2
    action@ActionAuctionBid {} -> do
      aSymbol <- getSymbol
      h1 <- activateContractWallet (aPerformer action) $ endpoints aSymbol
      callEndpoint @"auction-bid" h1 $
        AuctionBidParams
          { bp'nftId = aNftId action
          , bp'bidAmount = aBid action
          }
      void $ Trace.waitNSlots 2
    action@ActionAuctionClose {} -> do
      aSymbol <- getSymbol
      h1 <- activateContractWallet (aPerformer action) $ endpoints aSymbol
      callEndpoint @"auction-close" h1 $
        AuctionCloseParams
          { cp'nftId = aNftId action
          }
      void $ Trace.waitNSlots 2
    where
      getSymbol = do
        let hAdmin = h $ InitKey wAdmin
        oState <- Trace.observableState hAdmin
        case getLast oState of
          Nothing -> Trace.throwError $ Trace.GenericError "App Symbol Could not be established."
          Just aS -> return aS

deriving instance Hask.Eq (ContractInstanceKey NftModel w s e)
deriving instance Hask.Show (ContractInstanceKey NftModel w s e)

wallets :: [Wallet]
wallets = [w1, w2, w3]

wAdmin :: Wallet
wAdmin = wA

instanceSpec :: [ContractInstanceSpec NftModel]
instanceSpec = Hask.pure $ ContractInstanceSpec (InitKey wAdmin) wA adminEndpoints

propContract :: Actions NftModel -> QC.Property
propContract =
  QC.withMaxSuccess 50
    . propRunActionsWithOptions -- Keeping 50 tests limits time to ~1m, 100 tests took ~8m
      checkOptions
      instanceSpec
      (const $ Hask.pure True)

test :: TestTree
test = testGroup "QuickCheck" [testProperty "Contract" propContract]
