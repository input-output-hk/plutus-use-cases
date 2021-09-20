module Mlabs.Utils.Wallet (
  walletFromNumber,
) where

import PlutusTx.Prelude
import Wallet.Emulator.Wallet (Wallet, WalletNumber (..), fromWalletNumber)

walletFromNumber :: Integer -> Wallet
walletFromNumber = fromWalletNumber . WalletNumber
