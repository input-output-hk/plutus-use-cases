module Marketplace.Fixtures.Wallet where

import           Wallet.Emulator.Wallet (Wallet (..))

ownerWallet :: Wallet
ownerWallet = Wallet 1

userWallet :: Wallet
userWallet = Wallet 2

buyerWallet :: Wallet
buyerWallet = Wallet 3

userWallets :: [Wallet]
userWallets = [userWallet, buyerWallet]
