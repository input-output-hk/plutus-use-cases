module Test.EfficientNFT.Script.Values (
  authorWallet,
  authorPkh,
  nftPrice,
  tokenName,
  daoValHash,
  daoShare,
  daoShareVal,
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
  burnHash,
  mintTxOutRef,
  newNftPrice,
  newPriceNft1,
  otherPkh,
  newPriceTokenName,
  testTokenPolicy,
  testLockup,
  testLockupEnd,
  testLockScript,
  shouldFailWithErr,
  afterDeadline,
  afterDeadlineAndLockup,
  beforeDeadline,
  testMarketplaceScript,
) where

import PlutusTx qualified
import PlutusTx.Prelude hiding (elem)

import Data.Aeson (FromJSON, decode)
import Data.ByteString.Lazy (ByteString)
import Data.Data (Typeable)
import Data.Default (def)
import Data.Maybe (fromJust)
import Data.String (String)
import Ledger (
  AssetClass,
  Extended (Finite, PosInf),
  Interval (Interval),
  LowerBound (LowerBound),
  PaymentPubKeyHash (PaymentPubKeyHash),
  Slot (Slot),
  TokenName,
  TxOutRef (TxOutRef),
  UpperBound (UpperBound),
  ValidatorHash,
 )
import Ledger.Ada qualified as Ada
import Ledger.CardanoWallet qualified as CardanoWallet
import Ledger.TimeSlot (slotToBeginPOSIXTime)
import Ledger.Typed.Scripts (validatorHash)
import Ledger.Value (Value)
import Mlabs.EfficientNFT.Token (mkPolicy, mkTokenName)
import Plutus.V1.Ledger.Value (AssetClass (unAssetClass), assetClass)
import PlutusTx.Natural (Natural)
import Test.Tasty.Plutus.Context (
  ContextBuilder,
  Purpose (ForMinting, ForSpending),
 )
import Test.Tasty.Plutus.Options (TimeRange (TimeRange))
import Test.Tasty.Plutus.Script.Unit (shouldn'tValidateTracing)
import Test.Tasty.Plutus.TestData (TestData)
import Test.Tasty.Plutus.TestScript (TestScript, mkTestMintingPolicy, mkTestValidator, toTestMintingPolicy, toTestValidator)
import Test.Tasty.Plutus.WithScript (WithScript)
import Wallet.Emulator.Types qualified as Emu
import Prelude (elem)

import Mlabs.EfficientNFT.Dao (daoValidator)
import Mlabs.EfficientNFT.Lock (lockValidator, mkValidator)
import Mlabs.EfficientNFT.Marketplace (mkValidator)
import Mlabs.EfficientNFT.Types (LockAct, LockDatum, MarketplaceAct, MintAct, NftCollection (..), NftId (..))

mintTxOutRef :: TxOutRef
mintTxOutRef = TxOutRef txId 1
  where
    txId =
      unsafeDecode
        "{\"getTxId\" : \"3a9e96cbb9e2399046e7b653e29e2cc27ac88b3810b15f448b91425a9a27ef3a\"}"

authorWallet :: Emu.Wallet
authorWallet = Emu.fromWalletNumber (CardanoWallet.WalletNumber 1)

authorPkh :: PaymentPubKeyHash
authorPkh = Emu.mockWalletPaymentPubKeyHash authorWallet

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

daoValHash :: ValidatorHash
daoValHash = validatorHash daoValidator

daoShare :: Natural
daoShare = toEnum 10_00

daoShareVal :: Value
daoShareVal = Ada.lovelaceValueOf 10_000_000

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
    , nftCollection'daoScript = validatorHash daoValidator
    , nftCollection'daoShare = daoShare
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

newNftPrice :: Natural
newNftPrice = nftPrice * toEnum 2

newPriceNft1 :: NftId
newPriceNft1 = nft1 {nftId'price = newNftPrice}

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
        `PlutusTx.applyCode` PlutusTx.liftCode (nftCollection'daoScript collection)
        `PlutusTx.applyCode` PlutusTx.liftCode (nftCollection'daoShare collection)
    )
    $$(PlutusTx.compile [||toTestMintingPolicy||])

testLockup :: Integer
testLockup = 7776000

testLockupEnd :: Slot
testLockupEnd = 7776000

testLockScript :: TestScript ( 'ForSpending LockDatum LockAct)
testLockScript =
  mkTestValidator
    ( $$(PlutusTx.compile [||Mlabs.EfficientNFT.Lock.mkValidator||])
        `PlutusTx.applyCode` PlutusTx.liftCode (fst . unAssetClass $ collectionNft)
        `PlutusTx.applyCode` PlutusTx.liftCode testLockup
        `PlutusTx.applyCode` PlutusTx.liftCode testLockupEnd
    )
    $$(PlutusTx.compile [||toTestValidator||])

shouldFailWithErr ::
  forall (p :: Purpose).
  Typeable p =>
  String ->
  BuiltinString ->
  TestData p ->
  ContextBuilder p ->
  WithScript p ()
shouldFailWithErr name errMsg =
  shouldn'tValidateTracing name (errMsg' `elem`)
  where
    errMsg' = fromBuiltin errMsg

mkRange :: Slot -> TimeRange
mkRange slot =
  TimeRange $
    Interval
      (LowerBound (Finite (slotToBeginPOSIXTime def slot)) True)
      (UpperBound PosInf False)

afterDeadline :: TimeRange
afterDeadline = mkRange (testLockupEnd + 1)

afterDeadlineAndLockup :: TimeRange
afterDeadlineAndLockup = mkRange (testLockupEnd + Slot testLockup + 1)

beforeDeadline :: TimeRange
beforeDeadline = mkRange (testLockupEnd - 1)

testMarketplaceScript :: TestScript ( 'ForSpending BuiltinData MarketplaceAct)
testMarketplaceScript =
  mkTestValidator
    $$(PlutusTx.compile [||Mlabs.EfficientNFT.Marketplace.mkValidator||])
    $$(PlutusTx.compile [||toTestValidator||])
