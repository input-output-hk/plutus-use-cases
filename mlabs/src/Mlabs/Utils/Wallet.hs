module Mlabs.Utils.Wallet (
  walletFromNumber,
) where

import PlutusTx.Prelude
import Wallet.Emulator.Wallet (Wallet, fromWalletNumber)
import Ledger.CardanoWallet (WalletNumber (..))

walletFromNumber :: Integer -> Wallet
walletFromNumber = fromWalletNumber . WalletNumber
