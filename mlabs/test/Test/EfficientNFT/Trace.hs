module Test.EfficientNFT.Trace where

import PlutusTx.Prelude
import Prelude qualified as Hask

import Control.Monad (void)
import Control.Monad.Freer.Extras.Log as Extra (logInfo)
import Data.Maybe (fromJust)
import Data.Monoid (Last (..))
import Plutus.Trace.Emulator (EmulatorTrace, activateContractWallet, callEndpoint, runEmulatorTraceIO)
import Plutus.Trace.Emulator qualified as Trace
import Wallet.Emulator (Wallet)
import Wallet.Emulator qualified as Emulator

import Mlabs.EfficientNFT.Api
import Mlabs.EfficientNFT.Types
import Mlabs.Utils.Wallet (walletFromNumber)

type AppTraceHandle a = Trace.ContractHandle NftId NFTAppSchema a

mintTrace :: Emulator.Wallet -> EmulatorTrace ()
mintTrace wallet = do
  h1 <- activateContractWallet wallet endpoints

  callEndpoint @"mint" h1 artwork
  void $ Trace.waitNSlots 5
  nft1 <- fromJust . getLast Hask.<$> Trace.observableState h1
  logInfo $ Hask.show nft1

  callEndpoint @"set-price" h1 $ SetPriceParams nft1 (toEnum 7_000_000)
  void $ Trace.waitNSlots 5
  nft2 <- fromJust . getLast Hask.<$> Trace.observableState h1
  logInfo $ Hask.show nft2

  callEndpoint @"marketplace-deposit" h1 nft2
  void $ Trace.waitNSlots 5

  -- callEndpoint @"marketplace-redeem" h1 nft2
  -- void $ Trace.waitNSlots 5

  callEndpoint @"marketplace-set-price" h1 $ SetPriceParams nft2 (toEnum 9_000_000)
  void $ Trace.waitNSlots 5
  nft3 <- fromJust . getLast Hask.<$> Trace.observableState h1
  logInfo $ Hask.show nft3
  where
    -- callEndpoint @"marketplace-redeem" h1 nft3
    -- void $ Trace.waitNSlots 5

    artwork =
      MintParams
        { mp'share = toEnum 10
        , mp'price = toEnum 5_000_000
        }

w1, w2, w3 :: Wallet
w1 = walletFromNumber 1
w2 = walletFromNumber 2
w3 = walletFromNumber 3

test = runEmulatorTraceIO $ mintTrace w1
