module Test.EfficientNFT.Trace where

import PlutusTx.Prelude
import Prelude qualified as Hask

import Data.Default (def)
import Data.Monoid (Last (..))
import Data.Text (Text)

import Control.Monad (void)
import Control.Monad.Freer.Extras.Log as Extra (logInfo)

import Ledger.TimeSlot (slotToBeginPOSIXTime)
import Plutus.Trace.Emulator (EmulatorTrace, activateContractWallet, callEndpoint, runEmulatorTraceIO)
import Plutus.Trace.Emulator qualified as Trace
import Wallet.Emulator qualified as Emulator

import Mlabs.EfficientNFT.Api
import Mlabs.EfficientNFT.Types
import Mlabs.Utils.Wallet (walletFromNumber)
import Wallet.Emulator (Wallet)
import Data.Maybe (fromJust)

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

  void $ Trace.waitNSlots 1
  where
    artwork =
      MintParams
        { mp'content = Content "A painting."
        , mp'share = toEnum 10
        , mp'price = toEnum 5_000_000
        }

w1, w2, w3 :: Wallet
w1 = walletFromNumber 1
w2 = walletFromNumber 2
w3 = walletFromNumber 3

test = runEmulatorTraceIO $ mintTrace w1
