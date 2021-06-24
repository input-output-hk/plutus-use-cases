module Fixtures.Wallet where

import           Wallet.Emulator.Wallet (Wallet (..))

ownerWallet :: Wallet
ownerWallet = Wallet 1

lenderWallet :: Wallet
lenderWallet = Wallet 2

borrowerWallet :: Wallet
borrowerWallet = Wallet 3

userWallets :: [Wallet]
userWallets = [lenderWallet, borrowerWallet]
