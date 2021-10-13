module Marketplace.Fixtures.Wallet where

import           Wallet.Emulator.Types  (WalletNumber (..))
import           Wallet.Emulator.Wallet (Wallet (..), fromWalletNumber)

ownerWallet :: Wallet
ownerWallet = fromWalletNumber $ WalletNumber 1

userWallet :: Wallet
userWallet = fromWalletNumber $ WalletNumber 2

buyerWallet :: Wallet
buyerWallet = fromWalletNumber $ WalletNumber 3

userWallets :: [Wallet]
userWallets = [userWallet, buyerWallet]
