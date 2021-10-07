module Test.NFT.Values
 ( authorWallet, authorAddr, userOneWallet, userTwoWallet, authorPkh
 ) where

import qualified Ledger
import qualified Ledger.Address         as Ledger
import qualified Wallet.Emulator.Wallet as Emu

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

-- User 2
userTwoWallet :: Emu.Wallet
userTwoWallet = Emu.fromWalletNumber (Emu.WalletNumber 3)
