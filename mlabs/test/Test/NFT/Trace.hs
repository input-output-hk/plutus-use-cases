module Test.NFT.Trace where

import PlutusTx.Prelude
import Prelude qualified as Hask

import Data.Monoid (Last (..))
import Data.Text (Text)

import Control.Monad (void)
import Control.Monad.Freer.Extras.Log as Extra (logInfo)

import Plutus.Trace.Emulator (EmulatorTrace, activateContractWallet, callEndpoint, runEmulatorTraceIO)
import Plutus.Trace.Emulator qualified as Trace
import Wallet.Emulator qualified as Emulator

import Mlabs.Utils.Wallet (walletFromNumber)

import Mlabs.NFT.Contract
import Mlabs.NFT.Types

-- | Generic application Trace Handle.
type AppTraceHandle = Trace.ContractHandle (Last NftId) NFTAppSchema Text

-- | Emulator Trace 1. Mints Some NFT.
eTrace1 :: EmulatorTrace ()
eTrace1 = do
  let wallet1 = walletFromNumber 1 :: Emulator.Wallet
      wallet2 = walletFromNumber 2 :: Emulator.Wallet
  h1 :: AppTraceHandle <- activateContractWallet wallet1 endpoints
  h2 :: AppTraceHandle <- activateContractWallet wallet2 endpoints
  callEndpoint @"mint" h1 artwork
  -- callEndpoint @"mint" h2 artwork2

  void $ Trace.waitNSlots 1
  oState <- Trace.observableState h1
  nftId <- case getLast oState of
    Nothing -> Trace.throwError (Trace.GenericError "NftId not found")
    Just nid -> return nid
  void $ Trace.waitNSlots 1
  callEndpoint @"buy" h2 (buyParams nftId)

  logInfo @Hask.String $ Hask.show oState
  where
    --  callEndpoint @"mint" h1 artwork
    artwork =
      MintParams
        { mp'content = Content "A painting."
        , mp'title = Title "Fiona Lisa"
        , mp'share = 1 % 10
        , mp'price = Just 5
        }
    -- artwork2 = artwork {mp'content = Content "Another Painting"}

    buyParams nftId = BuyRequestUser nftId 6 (Just 200)

setPriceTrace :: EmulatorTrace ()
setPriceTrace = do
  let wallet1 = walletFromNumber 1 :: Emulator.Wallet
      wallet2 = walletFromNumber 5 :: Emulator.Wallet
  authMintH <- activateContractWallet wallet1 endpoints
  callEndpoint @"mint" authMintH artwork
  void $ Trace.waitNSlots 2
  oState <- Trace.observableState authMintH
  nftId <- case getLast oState of
    Nothing -> Trace.throwError (Trace.GenericError "NftId not found")
    Just nid -> return nid
  logInfo $ Hask.show nftId
  void $ Trace.waitNSlots 1
  authUseH :: AppTraceHandle <- activateContractWallet wallet1 endpoints
  callEndpoint @"set-price" authUseH (SetPriceParams nftId (Just 20))
  void $ Trace.waitNSlots 1
  callEndpoint @"set-price" authUseH (SetPriceParams nftId (Just (-20)))
  void $ Trace.waitNSlots 1
  userUseH :: AppTraceHandle <- activateContractWallet wallet2 endpoints
  callEndpoint @"set-price" userUseH (SetPriceParams nftId Nothing)
  void $ Trace.waitNSlots 1
  callEndpoint @"set-price" userUseH (SetPriceParams nftId (Just 30))
  void $ Trace.waitNSlots 1
  where
    artwork =
      MintParams
        { mp'content = Content "A painting."
        , mp'title = Title "Fiona Lisa"
        , mp'share = 1 % 10
        , mp'price = Just 100
        }

-- | Test for prototyping.
test :: Hask.IO ()
test = runEmulatorTraceIO eTrace1

testSetPrice :: Hask.IO ()
testSetPrice = runEmulatorTraceIO setPriceTrace
