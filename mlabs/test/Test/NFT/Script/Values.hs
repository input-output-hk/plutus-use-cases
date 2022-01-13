module Test.NFT.Script.Values where

import Data.Aeson qualified as Aeson
import Data.Maybe (fromJust)
import Ledger qualified

import Data.Kind (Type)

-- import Ledger.Value (TokenName (..))
import Ledger.Value qualified as Value

import Ledger.CardanoWallet qualified as CardanoWallet
import Test.Tasty.Plutus.Context

import Plutus.V1.Ledger.Ada qualified as Ada
import PlutusTx qualified
import PlutusTx.IsData.Class (FromData)
import PlutusTx.Prelude hiding ((<>))
import Wallet.Emulator.Wallet qualified as Emu

import Mlabs.NFT.Contract.Aux qualified as NFT
import Mlabs.NFT.Contract.Init (uniqueTokenName)
import Mlabs.NFT.Governance
import Mlabs.NFT.Governance qualified as Gov
import Mlabs.NFT.Spooky
import Mlabs.NFT.Types
import Mlabs.NFT.Validation qualified as NFT

-- test values

-- NFT Author
authorWallet :: Emu.Wallet
authorWallet = Emu.fromWalletNumber (CardanoWallet.WalletNumber 1)

authorAddr :: Ledger.Address
authorAddr = Emu.mockWalletAddress authorWallet

authorPkh :: Ledger.PaymentPubKeyHash
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

-- User 3
userThreeWallet :: Emu.Wallet
userThreeWallet = Emu.fromWalletNumber (CardanoWallet.WalletNumber 4)

userThreePkh :: Ledger.PaymentPubKeyHash
userThreePkh = Emu.mockWalletPaymentPubKeyHash userThreeWallet

testTxId :: Ledger.TxId
testTxId = fromJust $ Aeson.decode "{\"getTxId\" : \"61626364\"}"

testTokenName :: TokenName
testTokenName = TokenName . toSpooky $ hData
  where
    hData = NFT.hashData . Content . toSpooky @BuiltinByteString $ "A painting."

testNftId :: NftId
testNftId = NftId . toSpooky . unTokenName $ testTokenName

nftPolicy :: Ledger.MintingPolicy
nftPolicy = NFT.mintPolicy appInstance

oneNft :: Value.Value
oneNft = Value.singleton nftCurrencySymbol (unSpookyTokenName testTokenName) 1

nftCurrencySymbol :: Value.CurrencySymbol
nftCurrencySymbol = unSpookyCurrencySymbol . app'symbol $ appSymbol

oneAda :: Value.Value
oneAda = Ada.lovelaceValueOf 1_000_000

adaValue :: Integer -> Value.Value
adaValue = Ada.lovelaceValueOf . (* 1_000_000)

testStateAddr :: UniqueToken -> Ledger.Address
testStateAddr = unSpookyAddress . NFT.txScrAddress

appInstance :: NftAppInstance
appInstance = NftAppInstance (toSpooky . toSpookyAddress . testStateAddr $ uniqueAsset) (toSpooky uniqueAsset) (toSpooky . toSpookyAddress $ Gov.govScrAddress uniqueAsset) (toSpooky [UserId . toSpooky $ userOnePkh])

appSymbol :: NftAppSymbol
appSymbol = NftAppSymbol . toSpooky . NFT.curSymbol $ appInstance

{-
   We can't get rid of hard-coding the CurrencySymbol of UniqueToken at the moment since the mintContract produces it
   which works inside the Contract monad. Getting this value from our initApp endpoint need to encapsulate almost everything here
   to a Contract monad or using a similar approach such as ScriptM, which is operationally heavy and isn't worth doing.
   We can almost make sure that this value won't change unless upgrading weird things in plutus, or predetermining
   the initial state UTxOs to something other than the default.
-}

-- | Hardcoded UniqueToken
{-# INLINEABLE uniqueAsset #-}
uniqueAsset :: UniqueToken
uniqueAsset = assetClass (CurrencySymbol . toSpooky @BuiltinByteString $ "00a6b45b792d07aa2a778d84c49c6a0d0c0b2bf80d6c1c16accdbe01") (TokenName . toSpooky $ uniqueTokenName)

includeGovHead :: ContextBuilder a
includeGovHead = paysToOther (NFT.txValHash uniqueAsset) (Value.assetClassValue (unSpookyAssetClass uniqueAsset) 1) govHeadDatum
  where
    govHeadDatum = GovDatum $ HeadLList (GovLHead (5 % 1000) "") Nothing
