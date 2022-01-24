{-# LANGUAGE GADTs #-}

module Test.EfficientNFT.Quickcheck (test) where

import Control.Lens (makeLenses, (&), (.~), (^.))
import Control.Monad (void)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Monoid (Last (..))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String (IsString (..))
import Data.Text (Text)
import Ledger (AssetClass, PaymentPubKeyHash (PaymentPubKeyHash), ValidatorHash (ValidatorHash), minAdaTxOut, scriptCurrencySymbol)
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
  propRunActionsWithOptions,
  transfer,
  wait,
  withdraw,
  ($~),
 )
import Plutus.Trace.Emulator (callEndpoint, initialChainState)
import Plutus.Trace.Emulator qualified as Trace
import Plutus.V1.Ledger.Ada (adaSymbol, adaToken, getLovelace, lovelaceValueOf, toValue)
import Plutus.V1.Ledger.Value (CurrencySymbol, Value, assetClass, assetClassValue, singleton, valueOf)
import PlutusTx.Natural (Natural)
import PlutusTx.Prelude hiding ((<$>), (<*>), (==))
import Test.QuickCheck qualified as QC
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Prelude ((<$>), (<*>))
import Prelude qualified as Hask

import Mlabs.EfficientNFT.Api (NFTAppSchema, endpoints)
import Mlabs.EfficientNFT.Burn (burnValidator)
import Mlabs.EfficientNFT.Marketplace (marketplaceValidator)
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
    _mNfts :: Map NftId MockInfo
  , -- |
    _mMarketplace :: Map NftId MockInfo
  , -- | Preminted not used collection NFTs
    _mUnusedCollections :: Set AssetClass
  }
  deriving (Hask.Show, Hask.Eq)
makeLenses ''NftModel

instance ContractModel NftModel where
  data Action NftModel
    = ActionMint
        { aAuthor :: Wallet
        , aContent :: Content
        , aPrice :: Natural
        , aShare :: Natural
        , aCollection :: AssetClass
        }
    | ActionSetPrice
        { aNftId :: NftId
        , aMockInfo :: MockInfo
        , aPrice :: Natural
        }
    | ActionMarketplaceDeposit
        { aNftId :: NftId
        , aMockInfo :: MockInfo
        }
    | ActionMarketplaceWithdraw
        { aNftId :: NftId
        , aMockInfo :: MockInfo
        }
    | ActionMarketplaceSetPrice
        { aNftId :: NftId
        , aMockInfo :: MockInfo
        , aPrice :: Natural
        }
    | ActionMarketplaceBuy
        { aNftId :: NftId
        , aMockInfo :: MockInfo
        , aNewOwner :: Wallet
        }
    deriving (Hask.Show, Hask.Eq)

  data ContractInstanceKey NftModel w s e where
    UserKey :: Wallet -> ContractInstanceKey NftModel (Last NftId) NFTAppSchema Text

  initialHandleSpecs = Hask.fmap (\w -> ContractInstanceSpec (UserKey w) w endpoints) wallets

  initialState = NftModel Hask.mempty Hask.mempty (Set.fromList hardcodedCollections)

  arbitraryAction model =
    let genWallet = QC.elements wallets
        genNonNeg = toEnum . (* 1_000_000) . (+ 10) . QC.getNonNegative <$> QC.arbitrary
        genString = QC.listOf (QC.elements [Hask.minBound .. Hask.maxBound])
        genContent = Content . fromString . ('x' :) <$> genString
        genShare = toEnum <$> QC.elements [10 .. 100]
        genNftId = QC.elements $ addNonExistingNFT $ Map.toList (model ^. contractState . mNfts)
        genMarketplaceNftId = QC.elements $ addNonExistingNFT $ Map.toList (model ^. contractState . mMarketplace)
        genCollection = QC.elements hardcodedCollections
        -- We need this hack cause `QC.elements` cannot take an empty list.
        -- It will be filtered out in `precondition` check
        addNonExistingNFT = ((nonExsistingNFT, MockInfo w1 w1) :)
     in QC.oneof
          [ ActionMint
              <$> genWallet
              <*> genContent
              <*> genNonNeg
              <*> genShare
              <*> genCollection
          , uncurry ActionSetPrice
              <$> genNftId
              <*> genNonNeg
          , uncurry ActionMarketplaceDeposit
              <$> genNftId
          , uncurry ActionMarketplaceWithdraw
              <$> genMarketplaceNftId
          , uncurry ActionMarketplaceSetPrice
              <$> genMarketplaceNftId
              <*> genNonNeg
          , uncurry ActionMarketplaceBuy
              <$> genMarketplaceNftId
              <*> genWallet
          ]

  precondition s ActionMint {..} =
    Set.member aCollection (s ^. contractState . mUnusedCollections)
  precondition s ActionSetPrice {..} =
    not (Map.null $ s ^. contractState . mNfts)
      && Map.member aNftId (s ^. contractState . mNfts)
      && aPrice /= nftId'price aNftId
  precondition s ActionMarketplaceDeposit {..} =
    not (Map.null $ s ^. contractState . mNfts)
      && Map.member aNftId (s ^. contractState . mNfts)
  precondition s ActionMarketplaceWithdraw {..} =
    not (Map.null $ s ^. contractState . mMarketplace)
      && Map.member aNftId (s ^. contractState . mMarketplace)
  precondition s ActionMarketplaceSetPrice {..} =
    not (Map.null $ s ^. contractState . mMarketplace)
      && Map.member aNftId (s ^. contractState . mMarketplace)
      && aPrice /= nftId'price aNftId
  precondition s ActionMarketplaceBuy {..} =
    not (Map.null $ s ^. contractState . mMarketplace)
      && Map.member aNftId (s ^. contractState . mMarketplace)
      && mockWalletPaymentPubKeyHash aNewOwner /= nftId'owner aNftId

  perform h _ ActionMint {..} = do
    let params = MintParams aContent aShare aPrice
    callEndpoint @"mint-with-collection" (h $ UserKey aAuthor) (aCollection, params)
    void $ Trace.waitNSlots 5
  perform h _ ActionSetPrice {..} = do
    let params = SetPriceParams aNftId aPrice
    callEndpoint @"set-price" (h $ UserKey (aMockInfo ^. mock'owner)) params
    void $ Trace.waitNSlots 5
  perform h _ ActionMarketplaceDeposit {..} = do
    callEndpoint @"marketplace-deposit" (h $ UserKey (aMockInfo ^. mock'owner)) aNftId
    void $ Trace.waitNSlots 5
  perform h _ ActionMarketplaceWithdraw {..} = do
    callEndpoint @"marketplace-redeem" (h $ UserKey (aMockInfo ^. mock'owner)) aNftId
    void $ Trace.waitNSlots 5
  perform h _ ActionMarketplaceSetPrice {..} = do
    let params = SetPriceParams aNftId aPrice
    callEndpoint @"marketplace-set-price" (h $ UserKey (aMockInfo ^. mock'owner)) params
    void $ Trace.waitNSlots 5
  perform h _ ActionMarketplaceBuy {..} = do
    callEndpoint @"marketplace-buy" (h $ UserKey aNewOwner) aNftId
    void $ Trace.waitNSlots 5

  nextState ActionMint {..} = do
    let burnHash = validatorHash burnValidator
        policy' = policy burnHash Nothing aCollection
        curr = scriptCurrencySymbol policy'
        nft =
          NftId
            { nftId'content = aContent
            , nftId'price = aPrice
            , nftId'owner = mockWalletPaymentPubKeyHash aAuthor
            , nftId'author = mockWalletPaymentPubKeyHash aAuthor
            , nftId'authorShare = aShare
            , nftId'collectionNft = aCollection
            , nftId'marketplaceValHash = validatorHash . marketplaceValidator $ curr
            , nftId'marketplaceShare = toEnum 5
            }
    mNfts $~ Map.insert nft (MockInfo aAuthor aAuthor)
    mUnusedCollections $~ Set.delete aCollection
    deposit aAuthor $ singleton curr (mkTokenName nft) 1
    withdraw aAuthor (toValue minAdaTxOut <> assetClassValue aCollection 1)
    wait 5
  nextState ActionSetPrice {..} = do
    let newNft = aNftId {nftId'price = aPrice}
        wal = aMockInfo ^. mock'owner
        curr = getCurr aNftId
    mNfts $~ (Map.insert newNft aMockInfo . Map.delete aNftId)
    deposit wal $ singleton curr (mkTokenName newNft) 1
    withdraw wal $ singleton curr (mkTokenName aNftId) 1
    wait 5
  nextState ActionMarketplaceDeposit {..} = do
    let wal = aMockInfo ^. mock'owner
        curr = getCurr aNftId
    mNfts $~ Map.delete aNftId
    mMarketplace $~ Map.insert aNftId aMockInfo
    withdraw wal (singleton curr (mkTokenName aNftId) 1 <> toValue minAdaTxOut)
    wait 5
  nextState ActionMarketplaceWithdraw {..} = do
    let wal = aMockInfo ^. mock'owner
        curr = getCurr aNftId
    mNfts $~ Map.insert aNftId aMockInfo
    mMarketplace $~ Map.delete aNftId
    deposit wal (singleton curr (mkTokenName aNftId) 1 <> toValue minAdaTxOut)
    wait 5
  nextState ActionMarketplaceSetPrice {..} = do
    let newNft = aNftId {nftId'price = aPrice}
    mMarketplace $~ (Map.insert newNft aMockInfo . Map.delete aNftId)
    wait 5
  nextState ActionMarketplaceBuy {..} = do
    let newNft = aNftId {nftId'owner = mockWalletPaymentPubKeyHash aNewOwner}
        newInfo = mock'owner .~ aNewOwner $ aMockInfo
        nftPrice = nftId'price aNftId
        getShare share = lovelaceValueOf $ fromEnum nftPrice * share `divide` 10000
        authorShare = getShare (fromEnum . nftId'authorShare $ aNftId)
        marketplaceShare = getShare (fromEnum . nftId'marketplaceShare $ aNftId)
        ownerShare = lovelaceValueOf (fromEnum nftPrice) - authorShare - marketplaceShare
        filterLowValue v t
          | valueOf v adaSymbol adaToken < getLovelace minAdaTxOut = Hask.pure ()
          | otherwise = t
    mMarketplace $~ (Map.insert newNft newInfo . Map.delete aNftId)
    filterLowValue authorShare $ transfer aNewOwner (aMockInfo ^. mock'author) authorShare
    filterLowValue authorShare $ withdraw aNewOwner marketplaceShare
    transfer aNewOwner (aMockInfo ^. mock'owner) ownerShare
    wait 5

deriving instance Hask.Eq (ContractInstanceKey NftModel w s e)
deriving instance Hask.Show (ContractInstanceKey NftModel w s e)

getCurr :: NftId -> CurrencySymbol
getCurr nft =
  let burnHash = validatorHash burnValidator
      policy' = policy burnHash Nothing (nftId'collectionNft nft)
   in scriptCurrencySymbol policy'

hardcodedCollections :: [AssetClass]
hardcodedCollections = fmap (`assetClass` "NFT") ["aa", "bb", "cc"]

w1, w2, w3 :: Wallet
w1 = walletFromNumber 1
w2 = walletFromNumber 2
w3 = walletFromNumber 3

wallets :: [Wallet]
wallets = [w1, w2, w3]

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
    fmap (,vals) wallets
  where
    vals =
      singleton adaSymbol adaToken 100_000_000_000
        <> mconcat (fmap (`assetClassValue` 1) hardcodedCollections)

nonExsistingNFT :: NftId
nonExsistingNFT =
  NftId
    { nftId'content = Content ""
    , nftId'price = toEnum 0
    , nftId'owner = PaymentPubKeyHash ""
    , nftId'author = PaymentPubKeyHash ""
    , nftId'authorShare = toEnum 0
    , nftId'collectionNft = assetClass "ff" ""
    , nftId'marketplaceValHash = ValidatorHash ""
    , nftId'marketplaceShare = toEnum 0
    }

test :: TestTree
test =
  testGroup
    "QuickCheck"
    [ -- testProperty "Can get funds out" propNoLockedFunds
      testProperty "Contract" propContract
    ]
