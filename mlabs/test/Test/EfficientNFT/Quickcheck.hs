{-# LANGUAGE GADTs #-}

module Test.EfficientNFT.Quickcheck (test) where

import Control.Lens (makeLenses, view, (&), (.~), (^.))
import Control.Monad (void, when)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Monoid (Last (..))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Ledger (AssetClass, PaymentPubKeyHash (PaymentPubKeyHash), PubKeyHash, ValidatorHash (ValidatorHash), minAdaTxOut, scriptCurrencySymbol, unPaymentPubKeyHash)
import Ledger.Typed.Scripts (validatorHash)
import Mlabs.Utils.Wallet (walletFromNumber)
import Plutus.Contract.Test (CheckOptions, Wallet (..), defaultCheckOptions, emulatorConfig, mockWalletPaymentPubKeyHash)
import Plutus.Contract.Test.ContractModel (
  Action,
  Actions,
  ContractInstanceSpec (..),
  ContractModel (..),
  contractState,
  defaultCoverageOptions,
  deposit,
  getModelState,
  propRunActionsWithOptions,
  transfer,
  wait,
  withdraw,
  ($=),
  ($~),
 )
import Plutus.Trace.Emulator (callEndpoint, initialChainState)
import Plutus.Trace.Emulator qualified as Trace
import Plutus.V1.Ledger.Ada (adaSymbol, adaToken, getLovelace, lovelaceValueOf, toValue)
import Plutus.V1.Ledger.Value (CurrencySymbol (CurrencySymbol), Value, assetClass, assetClassValue, singleton, unAssetClass)
import PlutusTx.Natural (Natural)
import PlutusTx.Prelude hiding ((<$>), (<*>), (==))
import Test.QuickCheck qualified as QC
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Prelude ((<$>), (<*>))
import Prelude qualified as Hask

import Mlabs.EfficientNFT.Api (NFTAppSchema, endpoints)
import Mlabs.EfficientNFT.Dao (daoValidator)
import Mlabs.EfficientNFT.Lock (lockValidator)
import Mlabs.EfficientNFT.Token (mkTokenName, policy)
import Mlabs.EfficientNFT.Types

data MockInfo = MockInfo
  { _mock'owner :: Wallet
  , _mock'author :: Wallet
  }
  deriving (Hask.Show, Hask.Eq)
makeLenses ''MockInfo

data NftModel = NftModel
  { -- | Map of NFTs and owners
    _mNfts :: Map NftData MockInfo
  , -- |
    _mMarketplace :: Map NftData MockInfo
  , -- | Preminted not used collection NFTs
    _mUnusedCollections :: Set AssetClass
  , _mLockedFees :: Integer
  }
  deriving (Hask.Show, Hask.Eq)
makeLenses ''NftModel

instance ContractModel NftModel where
  data Action NftModel
    = ActionMint
        { aAuthor :: Wallet
        , aPrice :: Natural
        , aAuthorShare :: Natural
        , aDaoShare :: Natural
        , aCollection :: AssetClass
        }
    | ActionSetPrice
        { aNftData :: NftData
        , aMockInfo :: MockInfo
        , aPrice :: Natural
        }
    | ActionMarketplaceDeposit
        { aNftData :: NftData
        , aMockInfo :: MockInfo
        }
    | ActionMarketplaceRedeem
        { aNftData :: NftData
        , aMockInfo :: MockInfo
        }
    | ActionMarketplaceSetPrice
        { aNftData :: NftData
        , aMockInfo :: MockInfo
        , aPrice :: Natural
        }
    | ActionMarketplaceBuy
        { aNftData :: NftData
        , aMockInfo :: MockInfo
        , aNewOwner :: Wallet
        }
    | ActionFeeWithdraw
        { aPerformer :: Wallet -- TODO: better name
        }
    deriving (Hask.Show, Hask.Eq)

  data ContractInstanceKey NftModel w s e where
    UserKey :: Wallet -> ContractInstanceKey NftModel (Last NftData) NFTAppSchema Text

  initialHandleSpecs = Hask.fmap (\w -> ContractInstanceSpec (UserKey w) w endpoints) (wallets <> feeValultKeys)

  initialState = NftModel Hask.mempty Hask.mempty (Set.fromList hardcodedCollections) 0

  arbitraryAction model =
    let genWallet = QC.elements wallets
        genNonNeg = toEnum . (* 1_000_000) . (+ 25) . QC.getNonNegative <$> QC.arbitrary
        genShare = toEnum <$> QC.elements [0 .. 4500]
        genNftId = QC.elements $ addNonExistingNFT $ Map.toList (model ^. contractState . mNfts)
        genMarketplaceNftId = QC.elements $ addNonExistingNFT $ Map.toList (model ^. contractState . mMarketplace)
        genCollection = QC.elements hardcodedCollections
        -- We need this hack cause `QC.elements` cannot take an empty list.
        -- It will be filtered out in `precondition` check
        addNonExistingNFT = ((NftData nonExistingCollection nonExsistingNFT, MockInfo w1 w1) :)
     in QC.oneof
          [ ActionMint
              <$> genWallet
              <*> genNonNeg
              <*> genShare
              <*> genShare
              <*> genCollection
          , uncurry ActionSetPrice
              <$> genNftId
              <*> genNonNeg
          , uncurry ActionMarketplaceDeposit
              <$> genNftId
          , uncurry ActionMarketplaceRedeem
              <$> genMarketplaceNftId
          , uncurry ActionMarketplaceSetPrice
              <$> genMarketplaceNftId
              <*> genNonNeg
          , uncurry ActionMarketplaceBuy
              <$> genMarketplaceNftId
              <*> genWallet
          , ActionFeeWithdraw
              <$> QC.elements feeValultKeys
          ]

  precondition s ActionMint {..} =
    Set.member aCollection (s ^. contractState . mUnusedCollections)
  precondition s ActionSetPrice {..} =
    not (Map.null $ s ^. contractState . mNfts)
      && Map.member aNftData (s ^. contractState . mNfts)
      && aPrice /= nftId'price (nftData'nftId aNftData)
  precondition s ActionMarketplaceDeposit {..} =
    not (Map.null $ s ^. contractState . mNfts)
      && Map.member aNftData (s ^. contractState . mNfts)
  precondition s ActionMarketplaceRedeem {..} =
    not (Map.null $ s ^. contractState . mMarketplace)
      && Map.member aNftData (s ^. contractState . mMarketplace)
  precondition s ActionMarketplaceSetPrice {..} =
    not (Map.null $ s ^. contractState . mMarketplace)
      && Map.member aNftData (s ^. contractState . mMarketplace)
      && aPrice /= nftId'price (nftData'nftId aNftData)
  precondition s ActionMarketplaceBuy {..} =
    not (Map.null $ s ^. contractState . mMarketplace)
      && Map.member aNftData (s ^. contractState . mMarketplace)
      && mockWalletPaymentPubKeyHash aNewOwner /= nftId'owner (nftData'nftId aNftData)
  precondition s ActionFeeWithdraw {} =
    (s ^. contractState . mLockedFees) > 0

  perform h _ ActionMint {..} = do
    let params = MintParams aAuthorShare aDaoShare aPrice 5 5 Nothing feeValultKeys'
    callEndpoint @"mint-with-collection" (h $ UserKey aAuthor) (aCollection, params)
    void $ Trace.waitNSlots 5
  perform h _ ActionSetPrice {..} = do
    let params = SetPriceParams aNftData aPrice
    callEndpoint @"set-price" (h $ UserKey (aMockInfo ^. mock'owner)) params
    void $ Trace.waitNSlots 5
  perform h _ ActionMarketplaceDeposit {..} = do
    callEndpoint @"marketplace-deposit" (h $ UserKey (aMockInfo ^. mock'owner)) aNftData
    void $ Trace.waitNSlots 5
  perform h _ ActionMarketplaceRedeem {..} = do
    callEndpoint @"marketplace-redeem" (h $ UserKey (aMockInfo ^. mock'owner)) aNftData
    void $ Trace.waitNSlots 5
  perform h _ ActionMarketplaceSetPrice {..} = do
    let params = SetPriceParams aNftData aPrice
    callEndpoint @"marketplace-set-price" (h $ UserKey (aMockInfo ^. mock'owner)) params
    void $ Trace.waitNSlots 5
  perform h _ ActionMarketplaceBuy {..} = do
    callEndpoint @"marketplace-buy" (h $ UserKey aNewOwner) aNftData
    void $ Trace.waitNSlots 5
  perform h _ ActionFeeWithdraw {..} = do
    callEndpoint @"fee-withdraw" (h $ UserKey aPerformer) feeValultKeys'
    void $ Trace.waitNSlots 5

  nextState ActionMint {..} = do
    wait 1
    let nft =
          NftId
            { nftId'price = aPrice
            , nftId'owner = mockWalletPaymentPubKeyHash aAuthor
            , nftId'collectionNftTn = snd . unAssetClass $ aCollection
            }
        collection =
          NftCollection
            { nftCollection'collectionNftCs = fst . unAssetClass $ aCollection
            , nftCollection'lockLockup = 5 -- 7776000
            , nftCollection'lockLockupEnd = 5 -- 7776000
            , nftCollection'lockingScript =
                validatorHash $ lockValidator (fst $ unAssetClass aCollection) 5 5 -- 7776000 7776000
            , nftCollection'author = mockWalletPaymentPubKeyHash aAuthor
            , nftCollection'authorShare = aAuthorShare
            , nftCollection'daoScript =
                validatorHash $ daoValidator feeValultKeys'
            , nftCollection'daoShare = aDaoShare
            }
        nftData = NftData collection nft
        curr = getCurr nftData
    mNfts $~ Map.insert nftData (MockInfo aAuthor aAuthor)
    mUnusedCollections $~ Set.delete aCollection
    deposit aAuthor $ singleton curr (mkTokenName nft) 1
    withdraw aAuthor (toValue minAdaTxOut <> assetClassValue aCollection 1)
    wait 4
  nextState ActionSetPrice {..} = do
    let oldNft = nftData'nftId aNftData
        newNft = oldNft {nftId'price = aPrice}
        collection = nftData'nftCollection aNftData
        wal = aMockInfo ^. mock'owner
        curr = getCurr aNftData
    mNfts $~ (Map.insert (NftData collection newNft) aMockInfo . Map.delete aNftData)
    deposit wal $ singleton curr (mkTokenName newNft) 1
    withdraw wal $ singleton curr (mkTokenName oldNft) 1
    wait 5
  nextState ActionMarketplaceDeposit {..} = do
    let wal = aMockInfo ^. mock'owner
        curr = getCurr aNftData
        nft = nftData'nftId aNftData
    mNfts $~ Map.delete aNftData
    mMarketplace $~ Map.insert aNftData aMockInfo
    withdraw wal (singleton curr (mkTokenName nft) 1 <> toValue minAdaTxOut)
    wait 5
  nextState ActionMarketplaceRedeem {..} = do
    let wal = aMockInfo ^. mock'owner
        curr = getCurr aNftData
        newPrice = toEnum (fromEnum (nftId'price oldNft) + 1)
        oldNft = nftData'nftId aNftData
        newNft = oldNft {nftId'price = newPrice}
        collection = nftData'nftCollection aNftData
    mNfts $~ Map.insert (NftData collection newNft) aMockInfo
    mMarketplace $~ Map.delete aNftData
    deposit wal (singleton curr (mkTokenName newNft) 1 <> toValue minAdaTxOut)
    wait 5
  nextState ActionMarketplaceSetPrice {..} = do
    let oldNft = nftData'nftId aNftData
        newNft = oldNft {nftId'price = aPrice}
        collection = nftData'nftCollection aNftData
    mMarketplace $~ (Map.insert (NftData collection newNft) aMockInfo . Map.delete aNftData)
    wait 5
  nextState ActionMarketplaceBuy {..} = do
    let oldNft = nftData'nftId aNftData
        newNft = oldNft {nftId'owner = mockWalletPaymentPubKeyHash aNewOwner}
        collection = nftData'nftCollection aNftData
        newInfo = mock'owner .~ aNewOwner $ aMockInfo
        nftPrice = nftId'price oldNft
        getShare share = (fromEnum nftPrice * share) `divide` 100_00
        authorShare = getShare (fromEnum . nftCollection'authorShare $ collection)
        daoShare = getShare (fromEnum . nftCollection'daoShare $ collection)
        ownerShare = lovelaceValueOf (fromEnum nftPrice - filterLow authorShare - filterLow daoShare)
        filterLow v
          | fromEnum v < getLovelace minAdaTxOut = 0
          | otherwise = fromEnum v
        moreThanMinAda v =
          v > getLovelace minAdaTxOut
    mMarketplace $~ (Map.insert (NftData collection newNft) newInfo . Map.delete aNftData)
    when (moreThanMinAda authorShare) $ transfer aNewOwner (aMockInfo ^. mock'author) (lovelaceValueOf authorShare)
    when (moreThanMinAda daoShare) $ do
      withdraw aNewOwner (lovelaceValueOf daoShare)
      mLockedFees $~ (+ daoShare)
    transfer aNewOwner (aMockInfo ^. mock'owner) ownerShare
    wait 5
  nextState ActionFeeWithdraw {..} = do
    s <- view contractState <$> getModelState
    deposit aPerformer $ lovelaceValueOf (s ^. mLockedFees)
    mLockedFees $= 0

deriving instance Hask.Eq (ContractInstanceKey NftModel w s e)
deriving instance Hask.Show (ContractInstanceKey NftModel w s e)

getCurr :: NftData -> CurrencySymbol
getCurr nft =
  let policy' = policy . nftData'nftCollection $ nft
   in scriptCurrencySymbol policy'

hardcodedCollections :: [AssetClass]
hardcodedCollections = [assetClass cs tn | cs <- ["aa", "bb"], tn <- ["NFT1", "NFT2"]]

w1, w2, w3, w4, w5 :: Wallet
w1 = walletFromNumber 1
w2 = walletFromNumber 2
w3 = walletFromNumber 3
w4 = walletFromNumber 4
w5 = walletFromNumber 5

wallets :: [Wallet]
wallets = [w1, w2, w3]

feeValultKeys :: [Wallet]
feeValultKeys = [w4, w5]

feeValultKeys' :: [PubKeyHash]
feeValultKeys' = fmap (unPaymentPubKeyHash . mockWalletPaymentPubKeyHash) feeValultKeys

propContract :: Actions NftModel -> QC.Property
propContract =
  QC.withMaxSuccess 100
    . propRunActionsWithOptions
      checkOptions
      defaultCoverageOptions
      (const $ Hask.pure True)

checkOptions :: CheckOptions
checkOptions = defaultCheckOptions & emulatorConfig . initialChainState .~ Left initialDistribution

initialDistribution :: Map Wallet Value
initialDistribution =
  Map.fromList $
    fmap (,vals) (wallets <> feeValultKeys)
  where
    vals =
      singleton adaSymbol adaToken 100_000_000_000
        <> mconcat (fmap (`assetClassValue` 1) hardcodedCollections)

nonExsistingNFT :: NftId
nonExsistingNFT =
  NftId
    { nftId'price = toEnum 0
    , nftId'owner = PaymentPubKeyHash ""
    , nftId'collectionNftTn = ""
    }

nonExistingCollection :: NftCollection
nonExistingCollection =
  NftCollection
    { nftCollection'collectionNftCs = CurrencySymbol "ff"
    , nftCollection'lockLockup = 0
    , nftCollection'lockLockupEnd = 0
    , nftCollection'lockingScript = ValidatorHash ""
    , nftCollection'author = PaymentPubKeyHash ""
    , nftCollection'authorShare = toEnum 0
    , nftCollection'daoScript = ValidatorHash ""
    , nftCollection'daoShare = toEnum 0
    }

test :: TestTree
test =
  testGroup
    "QuickCheck"
    [ -- testProperty "Can get funds out" propNoLockedFunds
      testProperty "Contract" propContract
    ]
