{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}

-- | Init blockchain state for tests
module Test.Nft.Init (
  Script,
  runScript,
  checkOptions,
  w1,
  w2,
  w3,
  userAct,
  adaCoin,
  initialDistribution,
  toUserId,
  nftContent,
) where

import Prelude

import Control.Lens ((&), (.~))
import Control.Monad.Freer (Eff)
import Control.Monad.Freer.Error (Error)
import Control.Monad.Freer.Extras.Log (LogMsg)
import Control.Monad.Reader (ReaderT, ask, lift, runReaderT)
import Data.Map qualified as M
import Plutus.Contract.Test (CheckOptions, Wallet (..), defaultCheckOptions, emulatorConfig, walletPubKey)
import Plutus.Trace.Effects.EmulatedWalletAPI (EmulatedWalletAPI)
import Plutus.Trace.Effects.EmulatorControl (EmulatorControl)
import Plutus.Trace.Effects.RunContract (RunContract)
import Plutus.Trace.Effects.Waiting (Waiting)
import Plutus.Trace.Emulator (EmulatorRuntimeError, EmulatorTrace, initialChainState)
import Plutus.V1.Ledger.Ada (adaSymbol, adaToken)
import Plutus.V1.Ledger.Contexts (pubKeyHash)
import Plutus.V1.Ledger.Value (Value, singleton)
import PlutusTx.Prelude (ByteString)
import Test.Utils (next)

import Mlabs.Emulator.Types (UserId (..), adaCoin)
import Mlabs.Nft.Contract qualified as N
import Mlabs.Nft.Contract.Emulator.Client qualified as N
import Mlabs.Nft.Logic.Types (NftId, UserAct (..))
import PlutusTx.Ratio qualified as R

checkOptions :: CheckOptions
checkOptions = defaultCheckOptions & emulatorConfig . initialChainState .~ Left initialDistribution

-- | Wallets that are used for testing.
w1, w2, w3 :: Wallet
w1 = Wallet 1
w2 = Wallet 2
w3 = Wallet 3

toUserId :: Wallet -> UserId
toUserId = UserId . pubKeyHash . walletPubKey

-- | Helper to run the scripts for NFT-contract
type ScriptM a = ReaderT NftId (Eff '[RunContract, Waiting, EmulatorControl, EmulatedWalletAPI, LogMsg String, Error EmulatorRuntimeError]) a

type Script = ScriptM ()

{- | Script runner. It inits NFT by user 1 and provides nft id to all sequent
 endpoint calls.
-}
runScript :: Script -> EmulatorTrace ()
runScript script = do
  nftId <-
    N.callStartNft w1 $
      N.StartParams
        { sp'content = nftContent
        , sp'share = 1 R.% 10
        , sp'price = Nothing
        }
  next
  runReaderT script nftId

-- | User action call.
userAct :: Wallet -> UserAct -> Script
userAct wal act = do
  nftId <- ask
  lift $ N.callUserAct nftId wal act >> next

-- | NFT content for testing.
nftContent :: ByteString
nftContent = "Mona Lisa"

{- | Initial distribution of wallets for testing.
 We have 3 users. All of them get 1000 lovelace at the start.
-}
initialDistribution :: M.Map Wallet Value
initialDistribution =
  M.fromList
    [ (w1, val 1000_000_000)
    , (w2, val 1000_000_000)
    , (w3, val 1000_000_000)
    ]
  where
    val x = singleton adaSymbol adaToken x
