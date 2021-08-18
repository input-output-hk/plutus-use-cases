module Utils.Data where

import           Plutus.V1.Ledger.Crypto (PubKeyHash, pubKeyHash)
import           Wallet.Emulator.Wallet  (Wallet, walletPubKey)

one :: (a -> Bool) -> [a] -> Bool
one f = foldr reducer False
    where
        reducer cur acc = if acc then not . f $ cur else f cur

walletPkh :: Wallet -> PubKeyHash
walletPkh = pubKeyHash . walletPubKey
