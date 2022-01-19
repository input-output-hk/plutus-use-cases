module Test.EfficientNFT.Script.Values (
  mintTxOutRef,
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
  nft1,
  nft2,
  nft3,
  burnHash,
) where

import PlutusTx.Prelude

import Ledger (
  AssetClass,
  PaymentPubKeyHash (PaymentPubKeyHash),
  TokenName,
  TxOutRef (TxOutRef),
  ValidatorHash,
 )
import Ledger.CardanoWallet qualified as CardanoWallet
import Plutus.V1.Ledger.Value (assetClass)

import Data.Aeson (FromJSON, decode)
import Data.ByteString.Lazy (ByteString)
import Data.Maybe (fromJust)
import Ledger.Ada qualified as Ada
import Ledger.Typed.Scripts (validatorHash)
import Ledger.Value (Value)
import Mlabs.EfficientNFT.Token (mkTokenName)
import PlutusTx.Natural (Natural)
import Wallet.Emulator.Types qualified as Emu

import Mlabs.EfficientNFT.Burn
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
nftPrice = toEnum 10_000_000

marketplValHash :: ValidatorHash
marketplValHash = validatorHash . marketplaceValidator $ "ff"

marketplShare :: Natural
marketplShare = toEnum 10_00

marketplShareVal :: Value
marketplShareVal = Ada.lovelaceValueOf 1_000_000

authorShare :: Natural
authorShare = toEnum 15_00

authorShareVal :: Value
authorShareVal = Ada.lovelaceValueOf 1_500_000

ownerShareVal :: Value
ownerShareVal = Ada.lovelaceValueOf 7_500_000

tokenName :: TokenName
tokenName = mkTokenName nft1

unsafeDecode :: FromJSON a => ByteString -> a
unsafeDecode = fromJust . decode

collectionNft :: AssetClass
collectionNft = assetClass "abcd" "NFT"

nft1 :: NftId
nft1 =
  NftId
    { nftId'content = Content "NFT content"
    , nftId'price = nftPrice
    , nftId'owner = authorPkh
    , nftId'author = authorPkh
    , nftId'authorShare = authorShare
    , nftId'collectionNft = collectionNft
    , nftId'marketplaceValHash = marketplValHash
    , nftId'marketplaceShare = marketplShare
    }

nft2 :: NftId
nft2 =
  nft1 {nftId'owner = userOnePkh}

nft3 :: NftId
nft3 =
  nft1 {nftId'owner = userTwoPkh}

burnHash :: ValidatorHash
burnHash = validatorHash burnValidator
