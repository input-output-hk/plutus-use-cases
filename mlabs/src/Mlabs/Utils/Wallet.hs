module Mlabs.Utils.Wallet (
  walletFromNumber,
) where

import Ledger.CardanoWallet (WalletNumber (..))
import PlutusTx.Prelude
import Wallet.Emulator.Wallet (Wallet, fromWalletNumber)

walletFromNumber :: Integer -> Wallet
walletFromNumber = fromWalletNumber . WalletNumber
