module Test.NFT.Init (
  check,
  callStartNft,
  noChangesScene,
  checkOptions,
  w1,
  w2,
  w3,
  toUserId,
  userBuy,
  userMint,
  userSetPrice,
  artwork1,
  ownsAda,
) where

import Control.Lens ((&), (.~))
import Control.Monad.Freer (Eff)
import Control.Monad.Freer.Error (Error)
import Control.Monad.Freer.Extras.Log (LogMsg)
import Control.Monad.Reader (ReaderT, ask, lift, runReaderT, void)
import Data.Map qualified as M
import Data.Monoid (Last (..))
import Ledger.Contexts (pubKeyHash)
import Plutus.Contract.Test (CheckOptions, TracePredicate, Wallet (..), checkPredicateOptions, defaultCheckOptions, emulatorConfig, walletPubKey)
import Plutus.Trace.Effects.EmulatedWalletAPI (EmulatedWalletAPI)
import Plutus.Trace.Effects.EmulatorControl (EmulatorControl)
import Plutus.Trace.Effects.RunContract (RunContract)
import Plutus.Trace.Effects.Waiting (Waiting)
import Plutus.Trace.Emulator (EmulatorRuntimeError (GenericError), EmulatorTrace, activateContractWallet, callEndpoint, initialChainState, observableState, throwError, waitNSlots)
import Plutus.V1.Ledger.Ada (adaSymbol, adaToken)
import Plutus.V1.Ledger.Value (Value, singleton)
import PlutusTx.Prelude hiding (check, foldMap, pure)

import Test.Tasty (TestTree)
import Test.Utils (next)
import Prelude (Applicative (..), String, foldMap)

import Mlabs.Emulator.Scene (Scene, owns)
import Mlabs.Emulator.Types (adaCoin)
import Mlabs.NFT.Api
import Mlabs.NFT.Types (
  BuyRequestUser (..),
  Content (..),
  MintParams (..),
  NftAppSymbol (..),
  NftId (..),
  SetPriceParams (..),
  Title (..),
  UserId (..),
 )
import Mlabs.Utils.Wallet (walletFromNumber)

-- | Calls initialisation of state for Nft pool
callStartNft :: Wallet -> EmulatorTrace NftAppSymbol
callStartNft wal = do
  hAdmin <- activateContractWallet wal adminEndpoints
  callEndpoint @"app-init" hAdmin ()
  void $ waitNSlots 2
  oState <- observableState hAdmin
  aSymbol <- case getLast oState of
    Nothing -> throwError $ GenericError "App Symbol Could not be established."
    Just aS -> pure aS
  void $ waitNSlots 1
  pure aSymbol

type ScriptM a = ReaderT NftAppSymbol (Eff '[RunContract, Waiting, EmulatorControl, EmulatedWalletAPI, LogMsg String, Error EmulatorRuntimeError]) a

type Script = ScriptM ()

checkOptions :: CheckOptions
checkOptions = defaultCheckOptions & emulatorConfig . initialChainState .~ Left initialDistribution

-- | Wallets that are used for testing.
w1, w2, w3, w4 :: Wallet
w1 = walletFromNumber 1
w2 = walletFromNumber 2
w3 = walletFromNumber 3
w4 = walletFromNumber 4

toUserId :: Wallet -> UserId
toUserId = UserId . pubKeyHash . walletPubKey

{- | Script runner. It inits NFT by user 1 and provides nft id to all sequent
 endpoint calls.
-}
runScript :: Wallet -> Script -> EmulatorTrace ()
runScript wal script = do
  symbol <- callStartNft wal
  next
  runReaderT script symbol

userMint :: Wallet -> MintParams -> ScriptM NftId
userMint wal mp = do
  symbol <- ask
  lift $ do
    hdl <- activateContractWallet wal (endpoints symbol)
    callEndpoint @"mint" hdl mp
    next
    oState <- observableState hdl
    case getLast oState of
      Nothing -> throwError $ GenericError "Could not mint NFT"
      Just nftId -> pure nftId

userSetPrice :: Wallet -> SetPriceParams -> Script
userSetPrice wal sp = do
  symbol <- ask
  lift $ do
    hdl <- activateContractWallet wal (endpoints symbol)
    callEndpoint @"set-price" hdl sp
    next

userBuy :: Wallet -> BuyRequestUser -> Script
userBuy wal br = do
  symbol <- ask
  lift $ do
    hdl <- activateContractWallet wal (endpoints symbol)
    callEndpoint @"buy" hdl br
    next

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

artwork1 :: MintParams
artwork1 =
  MintParams
    { mp'content = Content "A painting."
    , mp'title = Title "Fiona Lisa"
    , mp'share = 1 % 10
    , mp'price = Nothing
    }
