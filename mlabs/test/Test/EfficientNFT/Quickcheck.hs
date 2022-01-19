{-# LANGUAGE GADTs #-}
module Test.EfficientNFT.Quickcheck (test) where

import Control.Lens (makeLenses, (^.), (&), (.~))
import Control.Monad (void)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Monoid (Last (..))
import Data.String (IsString (..))
import Data.Text (Text)
import Plutus.Contract.Test (Wallet (..), mockWalletPaymentPubKeyHash, defaultCheckOptions, emulatorConfig)
import Plutus.Contract.Test.ContractModel (
  Action,
  Actions,
  ContractInstanceSpec (..),
  ContractModel (..),
  contractState,
  defaultCoverageOptions,
  propRunActionsWithOptions,
  wait,
  ($~),
 )
import Plutus.Trace.Emulator (callEndpoint, initialChainState)
import Plutus.Trace.Emulator qualified as Trace
import Plutus.V1.Ledger.Ada (adaSymbol, adaToken)
import Plutus.V1.Ledger.Value (assetClass, singleton, assetClassValue, Value)
import PlutusTx.Prelude hiding ((<$>), (<*>), (==))
import Test.QuickCheck qualified as QC
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Prelude ((<$>), (<*>))
import Prelude qualified as Hask
import Ledger (scriptCurrencySymbol, AssetClass)
import Mlabs.Utils.Wallet (walletFromNumber)
import Ledger.Typed.Scripts (validatorHash)
import PlutusTx.Natural (Natural)

import Mlabs.EfficientNFT.Api (NFTAppSchema, endpoints)
import Mlabs.EfficientNFT.Types
import Mlabs.EfficientNFT.Token (policy)
import Mlabs.EfficientNFT.Burn (burnValidator)
import Mlabs.EfficientNFT.Marketplace (marketplaceValidator)

data NftModel = NftModel
  { -- | Map of NFTs and owners
    _mMarket ::  Map NftId Wallet
  , -- | Preminted not used collection NFTs 
    _mUnusedCollections :: [AssetClass]
  }
  deriving (Hask.Show, Hask.Eq)
makeLenses ''NftModel


instance ContractModel NftModel where
  data Action NftModel
    = ActionMint
        { aPerformer :: Wallet
        , aContent :: Content
        , aPrice :: Natural
        , aShare :: Natural
        , aCollection :: AssetClass
        }
    | ActionSetPrice
        { aNftIdAndOwner :: (NftId, Wallet)
        , aPrice :: Natural
        }
    deriving (Hask.Show, Hask.Eq)

  data ContractInstanceKey NftModel w s e where
    UserKey :: Wallet -> ContractInstanceKey NftModel (Last NftId) NFTAppSchema Text

  initialHandleSpecs = Hask.fmap (\w -> ContractInstanceSpec (UserKey w) w endpoints) wallets

  initialState = NftModel Hask.mempty hardcodedCollections

  arbitraryAction model =
    let nfts = Map.toList (model ^. contractState . mMarket)
        genWallet = QC.elements wallets
        genNonNeg = toEnum . (* 1_000_000) . (+ 1) . QC.getNonNegative <$> QC.arbitrary
        genString = QC.listOf (QC.elements [Hask.minBound .. Hask.maxBound])
        genContent = Content . fromString . ('x' :) <$> genString
        genShare = toEnum <$> QC.elements [1 .. 9000]
        genNftId = QC.elements nfts
        genCollection = Hask.pure $ head (model ^. contractState . mUnusedCollections)
     in QC.oneof
          [ ActionMint
              <$> genWallet
              <*> genContent
              <*> genNonNeg
              <*> genShare
              <*> genCollection
          , ActionSetPrice
              <$> genNftId
              <*> genNonNeg
          ]

  precondition s ActionMint {} =
    -- Chack that there are not used collection NFTs left
    not $ null (s ^. contractState . mUnusedCollections)
  precondition s ActionSetPrice {..} =
    not (Map.null (s ^. contractState . mMarket))
    && aPrice /= nftId'price (fst aNftIdAndOwner)

  perform h _ ActionMint{..} = do
    let params = MintParams aContent aShare aPrice
    callEndpoint @"mint-with-collection" (h $ UserKey aPerformer) (aCollection, params)
    void $ Trace.waitNSlots 5
  perform h _ ActionSetPrice {..} = do
    let params = SetPriceParams (fst aNftIdAndOwner) aPrice
    callEndpoint @"set-price" (h $ UserKey (snd aNftIdAndOwner)) params
    void $ Trace.waitNSlots 5

  nextState ActionMint{..} = do
    let burnHash = validatorHash burnValidator
        policy' = policy burnHash Nothing aCollection
        curr = scriptCurrencySymbol policy'
    let nft = NftId
          { nftId'content = aContent
          , nftId'price = aPrice
          , nftId'owner = mockWalletPaymentPubKeyHash aPerformer
          , nftId'author = mockWalletPaymentPubKeyHash aPerformer
          , nftId'authorShare = aShare
          , nftId'collectionNft = aCollection
          , nftId'marketplaceValHash = validatorHash . marketplaceValidator $ curr
          , nftId'marketplaceShare = toEnum 5
          }
    mMarket $~ Map.insert nft aPerformer
    -- Remove used collection NFT
    mUnusedCollections $~ tail
    wait 5
  nextState ActionSetPrice {..} = do
    let newNft = (fst aNftIdAndOwner) {nftId'price = aPrice}
    let wal = snd aNftIdAndOwner
    mMarket $~ (Map.insert newNft wal . Map.delete (fst aNftIdAndOwner))
    wait 5

deriving instance Hask.Eq (ContractInstanceKey NftModel w s e)
deriving instance Hask.Show (ContractInstanceKey NftModel w s e)

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
  QC.withMaxSuccess 50
    . propRunActionsWithOptions
      checkOptions
      defaultCoverageOptions
      (const $ Hask.pure True)

checkOptions = defaultCheckOptions & emulatorConfig . initialChainState .~ Left initialDistribution

initialDistribution :: Map Wallet Value
initialDistribution =
  Map.fromList
  $ fmap (,vals) wallets
  where
    vals = singleton adaSymbol adaToken 1_000_000_000 <> mconcat (fmap (`assetClassValue` 1) hardcodedCollections)

test :: TestTree
test =
  testGroup
    "QuickCheck"
    [ -- testProperty "Can get funds out" propNoLockedFunds
      testProperty "Contract" propContract
    ]
