module Test.NFT.Script.Values where

import Data.Aeson qualified as Aeson
import Data.Maybe (fromJust)
import Ledger qualified

import Ledger.Value (TokenName (..))
import Ledger.Value qualified as Value

import Mlabs.NFT.Contract.Aux qualified as NFT
import Mlabs.NFT.Types (Content (..), NftAppInstance (..), NftAppSymbol (..), NftId (..))
import Mlabs.NFT.Validation qualified as NFT
import Plutus.V1.Ledger.Ada qualified as Ada
import PlutusTx.Prelude hiding ((<>))
import Wallet.Emulator.Wallet qualified as Emu

-- test values

-- NFT Author
authorWallet :: Emu.Wallet
authorWallet = Emu.fromWalletNumber (Emu.WalletNumber 1)

authorAddr :: Ledger.Address
authorAddr = Emu.walletAddress authorWallet

authorPk :: Ledger.PubKey
authorPk = Emu.walletPubKey authorWallet

authorPkh :: Ledger.PubKeyHash
authorPkh = Ledger.pubKeyHash authorPk

-- User 1
userOneWallet :: Emu.Wallet
userOneWallet = Emu.fromWalletNumber (Emu.WalletNumber 2)

userOnePk :: Ledger.PubKey
userOnePk = Emu.walletPubKey userOneWallet

userOnePkh :: Ledger.PubKeyHash
userOnePkh = Ledger.pubKeyHash userOnePk

-- User 2
userTwoWallet :: Emu.Wallet
userTwoWallet = Emu.fromWalletNumber (Emu.WalletNumber 3)

userTwoPk :: Ledger.PubKey
userTwoPk = Emu.walletPubKey userTwoWallet

userTwoPkh :: Ledger.PubKeyHash
userTwoPkh = Ledger.pubKeyHash userTwoPk

-- User 3
userThreeWallet :: Emu.Wallet
userThreeWallet = Emu.fromWalletNumber (Emu.WalletNumber 4)

userThreePk :: Ledger.PubKey
userThreePk = Emu.walletPubKey userThreeWallet

userThreePkh :: Ledger.PubKeyHash
userThreePkh = Ledger.pubKeyHash userThreePk

testTxId :: Ledger.TxId
testTxId = fromJust $ Aeson.decode "{\"getTxId\" : \"61626364\"}"

testTokenName :: TokenName
testTokenName = TokenName hData
  where
    hData = NFT.hashData $ Content "A painting."

testNftId :: NftId
testNftId = NftId . unTokenName $ testTokenName

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

testStateAddr :: Ledger.Address
testStateAddr = NFT.txScrAddress

-- FIXME
appInstance :: NftAppInstance
appInstance = NftAppInstance testStateAddr (Value.AssetClass ("00a6b45b792d07aa2a778d84c49c6a0d0c0b2bf80d6c1c16accdbe01", "Unique App Token"))

appSymbol :: NftAppSymbol
appSymbol = NftAppSymbol . NFT.curSymbol $ appInstance
