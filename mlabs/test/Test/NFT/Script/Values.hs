module Test.NFT.Script.Values where

import Data.Aeson qualified as Aeson
import Data.Maybe (fromJust)
import Ledger qualified

import Ledger.Value (TokenName (..))
import Ledger.Value qualified as Value

import Ledger.CardanoWallet qualified as CardanoWallet
import Mlabs.NFT.Contract.Aux qualified as NFT
import Mlabs.NFT.Types (Content (..), NftAppInstance (..), NftAppSymbol (..), NftId (..))
import Mlabs.NFT.Validation qualified as NFT
import Plutus.V1.Ledger.Ada qualified as Ada
import PlutusTx.Prelude hiding ((<>))
import Wallet.Emulator.Wallet qualified as Emu

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
