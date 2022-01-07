module Test.NFT.Script.Values where

import Data.Aeson qualified as Aeson
import Data.Maybe (fromJust)
import Ledger qualified

import Data.Kind (Type)
import Ledger.Value (TokenName (..))
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
authorAddr = Emu.walletAddress authorWallet

authorPkh :: Ledger.PubKeyHash
authorPkh = Emu.walletPubKeyHash authorWallet

-- User 1
userOneWallet :: Emu.Wallet
userOneWallet = Emu.fromWalletNumber (CardanoWallet.WalletNumber 2)

userOnePkh :: Ledger.PubKeyHash
userOnePkh = Emu.walletPubKeyHash userOneWallet

-- User 2
userTwoWallet :: Emu.Wallet
userTwoWallet = Emu.fromWalletNumber (CardanoWallet.WalletNumber 3)

userTwoPkh :: Ledger.PubKeyHash
userTwoPkh = Emu.walletPubKeyHash userTwoWallet

-- User 3
userThreeWallet :: Emu.Wallet
userThreeWallet = Emu.fromWalletNumber (CardanoWallet.WalletNumber 4)

userThreePkh :: Ledger.PubKeyHash
userThreePkh = Emu.walletPubKeyHash userThreeWallet

testTxId :: Ledger.TxId
testTxId = fromJust $ Aeson.decode "{\"getTxId\" : \"61626364\"}"

testTokenName :: TokenName
testTokenName = TokenName hData
  where
    hData = NFT.hashData . Content . toSpooky @BuiltinByteString $ "A painting."

testNftId :: NftId
testNftId = NftId . toSpooky . unTokenName $ testTokenName

nftPolicy :: Ledger.MintingPolicy
nftPolicy = NFT.mintPolicy appInstance

oneNft :: Value.Value
oneNft = Value.singleton nftCurrencySymbol testTokenName 1

nftCurrencySymbol :: Value.CurrencySymbol
nftCurrencySymbol = app'symbol appSymbol

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
uniqueAsset = Value.AssetClass ("00a6b45b792d07aa2a778d84c49c6a0d0c0b2bf80d6c1c16accdbe01", TokenName uniqueTokenName)

includeGovHead :: ContextBuilder a
includeGovHead = paysOther (NFT.txValHash uniqueAsset) (Value.assetClassValue uniqueAsset 1) govHeadDatum
  where
    govHeadDatum = GovDatum $ HeadLList (GovLHead (5 % 1000) "") Nothing

-- We need to keep it until something happens with https://github.com/Liqwid-Labs/plutus-extra/issues/140
-- Functions are copy-pasted, only signatures are generalised

{-# INLINEABLE myToTestValidator #-}
myToTestValidator ::
  forall (datum :: Type) (redeemer :: Type) (ctx :: Type).
  (FromData datum, FromData redeemer, FromData ctx) =>
  (datum -> redeemer -> ctx -> Bool) ->
  (BuiltinData -> BuiltinData -> BuiltinData -> ())
myToTestValidator f d r p = case PlutusTx.fromBuiltinData d of
  Nothing -> reportParseFailed "Datum"
  Just d' -> case PlutusTx.fromBuiltinData r of
    Nothing -> reportParseFailed "Redeemer"
    Just r' -> case PlutusTx.fromBuiltinData p of
      Nothing -> reportParseFailed "ScriptContext"
      Just p' ->
        if f d' r' p'
          then reportPass
          else reportFail

{-# INLINEABLE myToTestMintingPolicy #-}
myToTestMintingPolicy ::
  forall (ctx :: Type) (redeemer :: Type).
  (FromData redeemer, FromData ctx) =>
  (redeemer -> ctx -> Bool) ->
  (BuiltinData -> BuiltinData -> ())
myToTestMintingPolicy f r p = case PlutusTx.fromBuiltinData r of
  Nothing -> reportParseFailed "Redeemer"
  Just r' -> case PlutusTx.fromBuiltinData p of
    Nothing -> reportParseFailed "ScriptContext"
    Just p' ->
      if f r' p'
        then reportPass
        else reportFail

{-# INLINEABLE reportParseFailed #-}
reportParseFailed :: BuiltinString -> ()
reportParseFailed what = report ("Parse failed: " `appendString` what)

{-# INLINEABLE reportPass #-}
reportPass :: ()
reportPass = report "Pass"

{-# INLINEABLE reportFail #-}
reportFail :: ()
reportFail = report "Fail"

{-# INLINEABLE report #-}
report :: BuiltinString -> ()
report what = trace ("tasty-plutus: " `appendString` what) ()
