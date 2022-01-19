module Test.EfficientNFT.Script.Values (
  mintTxOutRef,
  authorPkh,
  platformPkh,
  nftPrice,
  tokenName,
  platformCfg,
  contentHash,
  marketplShare,
  marketplShareVal,
  authorShare,
  authorShareVal,
  ownerShareVal,
  userOnePkh,
  userTwoPkh,
) where

import PlutusTx.Prelude

import Ledger (
  PaymentPubKeyHash (PaymentPubKeyHash),
  TokenName,
  TxOutRef (TxOutRef),
 )
import Ledger.CardanoWallet qualified as CardanoWallet

import Data.Aeson (FromJSON, decode)
import Data.ByteString.Lazy (ByteString)
import Data.Maybe (fromJust)
import Ledger.Ada qualified as Ada
import Ledger.Value (Value)
import Mlabs.EfficientNFT.Token (mkTokenName)
import PlutusTx.Natural (Natural)
import Wallet.Emulator.Types qualified as Emu

import Mlabs.EfficientNFT.Types (
  ContentHash,
  PlatformConfig (PlatformConfig, pcMarketplacePkh, pcMarketplaceShare),
 )

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

platformPkh :: PaymentPubKeyHash
platformPkh =
  PaymentPubKeyHash $
    unsafeDecode
      "{\"getPubKeyHash\" : \"bcd6bceeb0d22a7ca6ba1cd00669f7eb60ca8938d853666d30d56a56\"}"

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
nftPrice = toEnum 2_000_000

marketplShare :: Natural
marketplShare = toEnum 10_00

marketplShareVal :: Value
marketplShareVal = Ada.lovelaceValueOf 200_000

authorShare :: Natural
authorShare = toEnum 15_00

authorShareVal :: Value
authorShareVal = Ada.lovelaceValueOf 300_000

ownerShareVal :: Value
ownerShareVal = Ada.lovelaceValueOf 1_500_000

tokenName :: TokenName
tokenName = mkTokenName authorPkh nftPrice

unsafeDecode :: FromJSON a => ByteString -> a
unsafeDecode = fromJust . decode

platformCfg :: PlatformConfig
platformCfg =
  PlatformConfig
    { pcMarketplacePkh = platformPkh
    , pcMarketplaceShare = marketplShare
    }

contentHash :: ContentHash
contentHash = sha2_256 "Some NFT content"
