module Test.NFT.Init where

import PlutusTx.Prelude hiding (foldMap, pure)
import Prelude (foldMap, Applicative(..))
import Plutus.Contract.Test (Wallet (..), checkPredicateOptions)
import Control.Lens ((&), (.~))
import Control.Monad.Freer (Eff)
import Control.Monad.Freer.Error (Error)
import Control.Monad.Freer.Extras.Log (LogMsg)
import Control.Monad.Reader (ReaderT, ask, lift, runReaderT, void)
import Data.Map qualified as M
import Ledger.Contexts (pubKeyHash)
import Plutus.Contract.Test (CheckOptions, TracePredicate, defaultCheckOptions, emulatorConfig, walletPubKey)
import Plutus.Trace.Effects.EmulatedWalletAPI (EmulatedWalletAPI)
import Plutus.Trace.Effects.EmulatorControl (EmulatorControl)
import Plutus.Trace.Effects.RunContract (RunContract)
import Plutus.Trace.Effects.Waiting (Waiting)
import Plutus.Trace.Emulator (EmulatorRuntimeError(GenericError), EmulatorTrace, initialChainState, throwError, waitNSlots, activateContractWallet, callEndpoint, observableState)
import Plutus.V1.Ledger.Ada (adaSymbol, adaToken)
import Plutus.V1.Ledger.Value (Value, singleton)
import Test.Utils (next)
import Prelude (String)
import PlutusTx.Ratio qualified as R
import Data.Monoid (Last (..))
import Test.Tasty (TestTree)

import Mlabs.Emulator.Types (adaCoin)
import Mlabs.Utils.Wallet (walletFromNumber)
import Mlabs.Emulator.Scene (Scene, owns)
import Mlabs.NFT.Validation
import Mlabs.NFT.Contract
import Mlabs.NFT.Types

-- | Calls user act
callUserAct :: NftId -> Wallet -> UserAct -> EmulatorTrace ()
callUserAct nid wal act = do
  hdl <- activateContractWallet wal endpoints
  void $ case act of
    BuyAct {..} -> callEndpoint @"buy" hdl (BuyRequestUser nid act'bid act'newPrice)
    SetPriceAct {..} -> callEndpoint @"set-price" hdl (SetPriceParams nid act'newPrice)

-- | Calls initialisation of state for Nft pool
callStartNft :: Wallet -> MintParams -> EmulatorTrace NftId
callStartNft wal sp = do
  hdl <- activateContractWallet wal endpoints
  void $ callEndpoint @"mint" hdl sp
  void $ waitNSlots 10
  Last nid <- observableState hdl
  maybe err pure nid
  where
    err = throwError $ GenericError "No NFT started in emulator"

type ScriptM a = ReaderT NftId (Eff '[RunContract, Waiting, EmulatorControl, EmulatedWalletAPI, LogMsg String, Error EmulatorRuntimeError]) a

type Script = ScriptM ()

checkOptions :: CheckOptions
checkOptions = defaultCheckOptions & emulatorConfig . initialChainState .~ Left initialDistribution

-- | Wallets that are used for testing.
w1, w2, w3 :: Wallet
w1 = walletFromNumber 1
w2 = walletFromNumber 2
w3 = walletFromNumber 3

toUserId :: Wallet -> UserId
toUserId = UserId . pubKeyHash . walletPubKey

{- | Script runner. It inits NFT by user 1 and provides nft id to all sequent
 endpoint calls.
-}
runScript :: Wallet -> Script -> EmulatorTrace ()
runScript wal script = do
  nftId <- callStartNft wal mp
  next
  runReaderT script nftId

-- | User action call.
userAct :: Wallet -> UserAct -> Script
userAct wal act = do
  nftId <- ask
  lift $ callUserAct nftId wal act >> next

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

-- | Check if wallet contains Ada
ownsAda :: Wallet -> Integer -> Scene
ownsAda wal amount = wal `owns` [(adaCoin, amount)]

check :: String -> TracePredicate -> Wallet -> Script -> TestTree
check msg assertions wal script = checkPredicateOptions checkOptions msg assertions (runScript wal script)

-- | Scene without any transfers
noChangesScene :: Scene
noChangesScene = foldMap (`ownsAda` 0) [w1, w2, w3]

mp :: MintParams
mp = MintParams
     { mp'content = Content "Mona Lisa"
     , mp'title = Title ""
     , mp'share = 1 R.% 10
     , mp'price = Nothing
     }
