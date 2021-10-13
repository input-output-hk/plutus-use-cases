module Test.NFT.Script.Values where

import Data.Aeson qualified as Aeson
import Data.Maybe (fromJust)
import Ledger qualified
import Ledger.Address qualified as Ledger
import Ledger.Value (TokenName (..))
import Ledger.Value qualified as Value
import Mlabs.NFT.Contract qualified as NFT
import Mlabs.NFT.Types (Content (..), NftId (..), Title (..))
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

testTxId :: Ledger.TxId
testTxId = fromJust $ Aeson.decode "{\"getTxId\" : \"61626364\"}"

testOref :: Ledger.TxOutRef
testOref = Ledger.TxOutRef testTxId 1

testTokenName :: TokenName
testTokenName = TokenName hData
  where
    hData = NFT.hashData $ Content "A painting."

testNftId :: NftId
testNftId =
  NftId
    { nftId'title = Title "Fiona Lisa"
    , nftId'token = testTokenName
    , nftId'outRef = testOref
    }

nftPolicy :: Ledger.MintingPolicy
nftPolicy = NFT.mintPolicy testStateAddr testOref testNftId

oneNft :: Value.Value
oneNft = Value.singleton nftCurrencySymbol testTokenName 1

nftCurrencySymbol :: Value.CurrencySymbol
nftCurrencySymbol = Ledger.scriptCurrencySymbol nftPolicy

oneAda :: Value.Value
oneAda = Ada.lovelaceValueOf 1_000_000

adaValue :: Integer -> Value.Value
adaValue = Ada.lovelaceValueOf . (* 1_000_000)

testStateAddr :: Ledger.Address
testStateAddr = NFT.txScrAddress
