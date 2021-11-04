{-# LANGUAGE GADTs #-}

module Test.NFT.QuickCheck where

import Control.Lens (at, makeLenses, set, view, (^.))
import Control.Monad (void, when)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Monoid (Last (..))
import Data.String (IsString (..))
import Data.Text (Text)
import Ledger.Crypto (pubKeyHash)
import Plutus.Contract.Test (Wallet (..), walletPubKey)
import Plutus.Contract.Test.ContractModel (Action, Actions, ContractInstanceSpec (..), ContractModel (..), contractState, getModelState, propRunActionsWithOptions, transfer, wait, ($=), ($~))
import Plutus.Trace.Emulator (EmulatorRuntimeError (..), activateContractWallet, callEndpoint, observableState, throwError, waitNSlots)
import Plutus.Trace.Emulator qualified as Trace
import PlutusTx.Prelude hiding (fmap, length, mconcat, unless, (<$>), (<*>), (==))
import Test.QuickCheck qualified as QC
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Prelude (div, fmap, (<$>), (<*>), (==))
import Prelude qualified as Hask

import Mlabs.NFT.Api
import Mlabs.NFT.Contract
import Mlabs.NFT.Contract.Aux
import Mlabs.NFT.Types
import Mlabs.NFT.Validation
import Test.NFT.Init

-- We cannot use InformationNft because we need access to `Wallet`
-- `PubKeyHash` is not enough for simulation
data MockNft = MockNft
  { _nftId :: NftId
  , _nftPrice :: Maybe Integer
  , _nftOwner :: Wallet
  , _nftAuthor :: Wallet
  , _nftShare :: Rational
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
        , aContent :: Content
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
    deriving (Hask.Show, Hask.Eq)

  data ContractInstanceKey NftModel w s e where
    InitKey :: Wallet -> ContractInstanceKey NftModel (Last NftAppSymbol) NFTAppSchema Text

  instanceTag key _ = fromString $ Hask.show key

  arbitraryAction model =
    let invalidNft = NftId "I am invalid"
        nfts = view nftId <$> Map.elems (model ^. contractState . mMarket)
        genWallet = QC.elements wallets
        genNonNeg = ((* 100) . (+ 1)) . QC.getNonNegative <$> QC.arbitrary
        genMaybePrice = QC.oneof [Hask.pure Nothing, Just <$> genNonNeg]
        genString = QC.listOf (QC.elements [Hask.minBound .. Hask.maxBound])
        genContent = Content . fromString <$> genString
        genTitle = Title . fromString <$> genString
        genShare = (1 %) <$> QC.elements [2 .. 100] -- Shares like 1/2, 1/3 ... 1/100
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
          ]

  initialState = NftModel Map.empty 0 False

  precondition s ActionInit {} = not (s ^. contractState . mStarted)
  precondition s ActionMint {} = s ^. contractState . mStarted && ((s ^. contractState . mMintedCount) <= 5)
  precondition s _ = (s ^. contractState . mMintedCount) > 0

  nextState ActionInit {} = do
    mStarted $= True
  nextState action@ActionMint {} = do
    let nft =
          MockNft
            { _nftId = NftId . hashData $ aContent action
            , _nftPrice = aNewPrice action
            , _nftOwner = aPerformer action
            , _nftAuthor = aPerformer action
            , _nftShare = aShare action
            }
    mMarket $~ Map.insert (nft ^. nftId) nft
    mMintedCount $~ (+ 1)
  nextState action@ActionSetPrice {} = do
    s <- view contractState <$> getModelState
    let nft' = s ^. mMarket . at (aNftId action)
    case nft' of
      Nothing -> Hask.pure ()
      Just nft -> do
        when ((nft ^. nftOwner) == aPerformer action) $ do
          let newNft = set nftPrice (aNewPrice action) nft
          mMarket $~ Map.insert (aNftId action) newNft
  nextState action@ActionBuy {} = do
    s <- view contractState <$> getModelState
    let nft' = s ^. mMarket . at (aNftId action)
    case nft' of
      Nothing -> Hask.pure () -- Nft not found
      Just nft -> case nft ^. nftPrice of
        Nothing -> Hask.pure () -- Nft is locked
        Just nftPrice' -> do
          when (nftPrice' <= aPrice action) $ do
            let newNft = set nftOwner (aPerformer action) . set nftPrice (aNewPrice action) $ nft
                (ownerShare, authorShare) = calculateShares (aPrice action) (nft ^. nftShare)
            mMarket $~ Map.insert (aNftId action) newNft
            transfer (aPerformer action) (nft ^. nftOwner) ownerShare
            transfer (aPerformer action) (nft ^. nftAuthor) authorShare

  perform h _ = \case
    action@ActionInit {} -> do
      let hAdmin = h $ InitKey wAdmin
      callEndpoint @"app-init" hAdmin ()
      void $ Trace.waitNSlots 2
      void getSymbol
    action@ActionMint {} -> do
      aSymbol <- getSymbol
      h1 <- activateContractWallet (aPerformer action) $ endpoints aSymbol
      callEndpoint @"mint" h1 $
        MintParams
          { mp'content = aContent action
          , mp'title = aTitle action
          , mp'share = aShare action
          , mp'price = aNewPrice action
          }
      void $ Trace.waitNSlots 1
    action@ActionSetPrice {} -> do
      aSymbol <- getSymbol
      h1 <- activateContractWallet (aPerformer action) $ endpoints aSymbol
      callEndpoint @"set-price" h1 $
        SetPriceParams
          { sp'nftId = aNftId action
          , sp'price = aNewPrice action
          }
      void $ Trace.waitNSlots 1
    action@ActionBuy {} -> do
      aSymbol <- getSymbol
      h1 <- activateContractWallet (aPerformer action) $ endpoints aSymbol
      callEndpoint @"buy" h1 $
        BuyRequestUser
          { ur'nftId = aNftId action
          , ur'newPrice = aNewPrice action
          , ur'price = aPrice action
          }
      void $ Trace.waitNSlots 1
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
instanceSpec = Hask.pure $ ContractInstanceSpec (InitKey wAdmin) w1 adminEndpoints

propContract :: Actions NftModel -> QC.Property
propContract =
  QC.withMaxSuccess 50
    . propRunActionsWithOptions -- Keeping 50 tests limits time to ~1m, 100 tests took ~8m
      checkOptions
      instanceSpec
      (const $ Hask.pure True)

test :: TestTree
test = testGroup "QuickCheck" [testProperty "Contract" propContract]
