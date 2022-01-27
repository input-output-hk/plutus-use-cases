module Test.EfficientNFT.Script.Values (
  authorPkh,
  nftPrice,
  tokenName,
  marketplValHash,
  marketplShare,
  marketplShareVal,
  authorShare,
  authorShareVal,
  ownerShareVal,
  userOnePkh,
  userTwoPkh,
  collectionNft,
  collection,
  nft1,
  nft2,
  nft3,
  nftPrice,
  tokenName,
  collectionNft,
  nft1,
  burnHash,
  collectionNft,
  mintTxOutRef,
  nft1,
  newPriceNft1,
  otherPkh,
  tokenName,
  newPriceTokenName,
  testTokenPolicy,
) where

import PlutusTx qualified
import PlutusTx.Prelude

import Ledger (
  AssetClass,
  PaymentPubKeyHash (PaymentPubKeyHash),
  TokenName,
  TxOutRef (TxOutRef),
  ValidatorHash,
 )
import Ledger.CardanoWallet qualified as CardanoWallet
import Plutus.V1.Ledger.Value (AssetClass (unAssetClass), assetClass)
import Test.Tasty.Plutus.Context (
  Purpose (ForMinting),
 )
import Test.Tasty.Plutus.TestScript (TestScript, mkTestMintingPolicy, toTestMintingPolicy)

import Data.Aeson (FromJSON, decode)
import Data.ByteString.Lazy (ByteString)
import Data.Maybe (fromJust)
import Ledger.Ada qualified as Ada
import Ledger.Typed.Scripts (validatorHash)
import Ledger.Value (Value)
import Mlabs.EfficientNFT.Token (mkPolicy, mkTokenName)
import PlutusTx.Natural (Natural)
import Wallet.Emulator.Types qualified as Emu

import Mlabs.EfficientNFT.Lock
import Mlabs.EfficientNFT.Marketplace
import Mlabs.EfficientNFT.Types

mintTxOutRef :: TxOutRef
mintTxOutRef = TxOutRef txId 1
  where
    txId =
      unsafeDecode
        "{\"getTxId\" : \"3a9e96cbb9e2399046e7b653e29e2cc27ac88b3810b15f448b91425a9a27ef3a\"}"

authorPkh :: PaymentPubKeyHash
authorPkh =
  PaymentPubKeyHash $
    unsafeDecode
      "{\"getPubKeyHash\" : \"25bd24abedaf5c68d898484d757f715c7b4413ad91a80d3cb0b3660d\"}"

-- User 1
userOneWallet :: Emu.Wallet
userOneWallet = Emu.fromWalletNumber (CardanoWallet.WalletNumber 2)

userOnePkh :: Ledger.PaymentPubKeyHash
userOnePkh = Emu.mockWalletPaymentPubKeyHash userOneWallet

-- User 2
userTwoWallet :: Emu.Wallet
userTwoWallet = Emu.fromWalletNumber (CardanoWallet.WalletNumber 3)

userTwoPkh :: Ledger.PaymentPubKeyHash
userTwoPkh = Emu.mockWalletPaymentPubKeyHash userTwoWallet

nftPrice :: Natural
nftPrice = toEnum 100_000_000

marketplValHash :: ValidatorHash
marketplValHash = validatorHash marketplaceValidator

marketplShare :: Natural
marketplShare = toEnum 10_00

marketplShareVal :: Value
marketplShareVal = Ada.lovelaceValueOf 10_000_000

authorShare :: Natural
authorShare = toEnum 15_00

authorShareVal :: Value
authorShareVal = Ada.lovelaceValueOf 15_000_000

ownerShareVal :: Value
ownerShareVal = Ada.lovelaceValueOf 75_000_000

otherPkh :: PaymentPubKeyHash
otherPkh =
  PaymentPubKeyHash $
    unsafeDecode
      "{\"getPubKeyHash\" : \"75bd24abfdaf5c68d898484d757f715c7b4413ad91a80d3cb0b3660d\"}"

tokenName :: TokenName
tokenName = mkTokenName nft1

newPriceTokenName :: TokenName
newPriceTokenName = mkTokenName newPriceNft1

-- newPrice :: Natural
-- newPrice = nftPrice + nftPrice

-- newPriceTokenName :: TokenName
-- newPriceTokenName = mkTokenName authorPkh newPrice

unsafeDecode :: FromJSON a => ByteString -> a
unsafeDecode = fromJust . decode

collectionNft :: AssetClass
collectionNft = assetClass "abcd" "NFT"

collection :: NftCollection
collection =
  NftCollection
    { nftCollection'collectionNftCs = fst . unAssetClass $ collectionNft
    , nftCollection'lockingScript = validatorHash $ lockValidator (fst $ unAssetClass collectionNft) 7776000 7776000
    , nftCollection'author = authorPkh
    , nftCollection'authorShare = authorShare
    , nftCollection'marketplaceScript = validatorHash marketplaceValidator
    , nftCollection'marketplaceShare = marketplShare
    }

nft1 :: NftId
nft1 =
  NftId
    { nftId'price = nftPrice
    , nftId'owner = authorPkh
    , nftId'collectionNftTn = snd . unAssetClass $ collectionNft
    }

nft2 :: NftId
nft2 =
  nft1 {nftId'owner = userOnePkh}

nft3 :: NftId
nft3 =
  nft1 {nftId'owner = userTwoPkh}

newPriceNft1 :: NftId
newPriceNft1 = nft1 {nftId'price = nftId'price nft1 * toEnum 2}

burnHash :: ValidatorHash
burnHash = validatorHash $ lockValidator (fst $ unAssetClass collectionNft) 7776000 7776000

testTokenPolicy :: TestScript ( 'ForMinting MintAct)
testTokenPolicy =
  mkTestMintingPolicy
    ( $$(PlutusTx.compile [||mkPolicy||])
        `PlutusTx.applyCode` PlutusTx.liftCode (nftCollection'collectionNftCs collection)
        `PlutusTx.applyCode` PlutusTx.liftCode (nftCollection'lockingScript collection)
        `PlutusTx.applyCode` PlutusTx.liftCode (nftCollection'author collection)
        `PlutusTx.applyCode` PlutusTx.liftCode (nftCollection'authorShare collection)
        `PlutusTx.applyCode` PlutusTx.liftCode (nftCollection'marketplaceScript collection)
        `PlutusTx.applyCode` PlutusTx.liftCode (nftCollection'marketplaceShare collection)
    )
    $$(PlutusTx.compile [||toTestMintingPolicy||])
