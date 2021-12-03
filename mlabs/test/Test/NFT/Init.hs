module Test.NFT.Init (
  artwork1,
  artwork2,
  callStartNft,
  callStartNftFail,
  check,
  checkOptions,
  noChangesScene,
  ownsAda,
  runScript,
  toUserId,
  userBuy,
  userMint,
  userQueryPrice,
  userQueryOwner,
  userQueryListNfts,
  userQueryContent,
  userSetPrice,
  containsLog,
  w1,
  w2,
  w3,
  wA,
  userBidAuction,
  userStartAuction,
  userCloseAuction,
  userWait,
  waitInit,
  mkFreeGov,
  getFreeGov,
  appSymbol,
) where

import Control.Lens ((&), (.~))
import Control.Monad.Freer (Eff)
import Control.Monad.Freer.Error (Error)
import Control.Monad.Freer.Extras.Log (LogMsg)
import Control.Monad.Reader (ReaderT, ask, lift, runReaderT, void)
import Data.Aeson (Value (String))
import Data.Map qualified as M
import Data.Monoid (Last (..))
import Data.Text qualified as T
import Ledger (getPubKeyHash)
import Numeric.Natural (Natural)
import Plutus.Contract.Test (
  CheckOptions,
  TracePredicate,
  Wallet (..),
  assertInstanceLog,
  checkPredicateOptions,
  defaultCheckOptions,
  emulatorConfig,
  walletPubKeyHash,
 )
import Plutus.Trace.Effects.Assert (Assert)
import Plutus.Trace.Effects.EmulatedWalletAPI (EmulatedWalletAPI)
import Plutus.Trace.Effects.EmulatorControl (EmulatorControl)
import Plutus.Trace.Effects.RunContract (RunContract)
import Plutus.Trace.Effects.Waiting (Waiting)
import Plutus.Trace.Emulator (
  EmulatorRuntimeError (GenericError),
  EmulatorTrace,
  activateContractWallet,
  callEndpoint,
  initialChainState,
  observableState,
  throwError,
  waitNSlots,
 )
import Plutus.Trace.Emulator.Types (ContractInstanceLog (..), ContractInstanceMsg (..), walletInstanceTag)
import Plutus.V1.Ledger.Ada (adaSymbol, adaToken)
import Plutus.V1.Ledger.Value (AssetClass (..), TokenName (..), Value, assetClassValue, singleton, valueOf)
import PlutusTx.Prelude hiding (check, foldMap, pure)
import Wallet.Emulator.MultiAgent (EmulatorTimeEvent (..))
import Prelude (Applicative (..), String, foldMap)
import Prelude qualified as Hask

import Test.Tasty (TestTree)
import Test.Utils (next)

import Mlabs.Emulator.Scene (Scene, owns)
import Mlabs.Emulator.Types (adaCoin)
import Mlabs.NFT.Api (
  adminEndpoints,
  endpoints,
  queryEndpoints,
 )
import Mlabs.NFT.Types (
  AuctionBidParams,
  AuctionCloseParams,
  AuctionOpenParams,
  BuyRequestUser (..),
  Content (..),
  InitParams (..),
  MintParams (..),
  NftAppInstance (appInstance'UniqueToken),
  NftAppSymbol (..),
  NftId (..),
  SetPriceParams (..),
  Title (..),
  UniqueToken,
  UserId (..),
 )
import Mlabs.Utils.Wallet (walletFromNumber)

-- | Wallets that are used for testing.
w1, w2, w3, wA :: Wallet
w1 = walletFromNumber 1
w2 = walletFromNumber 2
w3 = walletFromNumber 3
wA = walletFromNumber 4 -- Admin Wallet

{- it was 2 before, but after switching
   `Plutus.Contracts.Currency.mintContract`
   to `Mlabs.Plutus.Contracts.Currency.mintContract`
   tests start to fail with 2 slots waiting
-}
waitInit :: EmulatorTrace ()
waitInit = void $ waitNSlots 3

-- | Calls initialisation of state for Nft pool
callStartNft :: Wallet -> EmulatorTrace NftAppInstance
callStartNft wal = do
  hAdmin <- activateContractWallet wal adminEndpoints
  let params =
        InitParams
          [UserId . walletPubKeyHash $ wal]
          (5 % 1000)
          (walletPubKeyHash wal)
  callEndpoint @"app-init" hAdmin params
  waitInit
  oState <- observableState hAdmin
  appInstance <- case getLast oState of
    Nothing -> throwError $ GenericError "App Symbol Could not be established."
    Just aS -> pure aS
  void $ waitNSlots 1
  pure appInstance

callStartNftFail :: Wallet -> ScriptM ()
callStartNftFail wal = do
  let w5 = walletFromNumber 5
      params =
        InitParams
          [UserId . walletPubKeyHash $ w5]
          (5 % 1000)
          (walletPubKeyHash wal)
  lift $ do
    hAdmin <- activateContractWallet wal adminEndpoints
    callEndpoint @"app-init" hAdmin params
    waitInit

type ScriptM a =
  ReaderT
    UniqueToken
    ( Eff
        '[ RunContract
         , Assert
         , Waiting
         , EmulatorControl
         , EmulatedWalletAPI
         , LogMsg String
         , Error EmulatorRuntimeError
         ]
    )
    a

type Script = ScriptM ()

checkOptions :: CheckOptions
checkOptions = defaultCheckOptions & emulatorConfig . initialChainState .~ Left initialDistribution

toUserId :: Wallet -> UserId
toUserId = UserId . walletPubKeyHash

{- | Script runner. It inits NFT by user 1 and provides nft id to all sequent
 endpoint calls.
-}
runScript :: Wallet -> Script -> EmulatorTrace ()
runScript wal script = do
  appInstance <- callStartNft wal
  next
  runReaderT script $ appInstance'UniqueToken appInstance

userMint :: Wallet -> MintParams -> ScriptM NftId
userMint wal mp = do
  symbol <- ask
  lift $ do
    hdl <- activateContractWallet wal (endpoints symbol)
    callEndpoint @"mint" hdl mp
    next
    oState <- observableState hdl
    case findNftId oState of
      Nothing -> throwError $ GenericError "Could not mint NFT"
      Just nftId -> pure nftId
  where
    findNftId :: forall a b. Last (Either a b) -> Maybe a
    findNftId x = case getLast x of
      Just (Left x') -> Just x'
      _ -> Nothing

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

userQueryPrice :: Wallet -> NftId -> Script
userQueryPrice wal nftId = do
  symbol <- ask
  lift $ do
    hdl <- activateContractWallet wal (queryEndpoints symbol)
    callEndpoint @"query-current-price" hdl nftId

userQueryOwner :: Wallet -> NftId -> Script
userQueryOwner wal nftId = do
  symbol <- ask
  lift $ do
    hdl <- activateContractWallet wal (queryEndpoints symbol)
    callEndpoint @"query-current-owner" hdl nftId

userQueryListNfts :: Wallet -> Script
userQueryListNfts wal = do
  symbol <- ask
  lift $ do
    hdl <- activateContractWallet wal (queryEndpoints symbol)
    callEndpoint @"query-list-nfts" hdl ()

userQueryContent :: Wallet -> Content -> Script
userQueryContent wal content = do
  symbol <- ask
  lift $ do
    hdl <- activateContractWallet wal (queryEndpoints symbol)
    callEndpoint @"query-content" hdl content

userStartAuction :: Wallet -> AuctionOpenParams -> Script
userStartAuction wal params = do
  symbol <- ask
  lift $ do
    hdl <- activateContractWallet wal (endpoints symbol)
    callEndpoint @"auction-open" hdl params
    next

userBidAuction :: Wallet -> AuctionBidParams -> Script
userBidAuction wal params = do
  symbol <- ask
  lift $ do
    hdl <- activateContractWallet wal (endpoints symbol)
    callEndpoint @"auction-bid" hdl params
    next

userCloseAuction :: Wallet -> AuctionCloseParams -> Script
userCloseAuction wal params = do
  symbol <- ask
  lift $ do
    hdl <- activateContractWallet wal (endpoints symbol)
    callEndpoint @"auction-close" hdl params
    next

userWait :: Natural -> Script
userWait = lift . void . waitNSlots

{- | Initial distribution of wallets for testing.
 We have 3 users. All of them get 1000 lovelace at the start.
-}
initialDistribution :: M.Map Wallet Plutus.V1.Ledger.Value.Value
initialDistribution =
  M.fromList
    [ (w1, val 1000_000_000)
    , (w2, val 1000_000_000)
    , (w3, val 1000_000_000)
    , (wA, val 1000_000_000)
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

containsLog :: Wallet -> String -> TracePredicate
containsLog wal expected = assertInstanceLog (walletInstanceTag wal) (any predicate)
  where
    predicate = \case
      (EmulatorTimeEvent _ (ContractInstanceLog (ContractLog (String actual)) _ _)) ->
        T.pack expected Hask.== actual
      _ -> False

artwork1 :: MintParams
artwork1 =
  MintParams
    { mp'content = Content "A painting."
    , mp'title = Title "Fiona Lisa"
    , mp'share = 1 % 10
    , mp'price = Nothing
    }

artwork2 :: MintParams
artwork2 =
  MintParams
    { mp'content = Content "Another painting."
    , mp'title = Title "Fiona Lisa"
    , mp'share = 1 % 10
    , mp'price = Just 300
    }

mkFreeGov :: Wallet -> Integer -> Plutus.V1.Ledger.Value.Value
mkFreeGov wal = assetClassValue (AssetClass (govCurrency, tn))
  where
    tn = TokenName . ("freeGov" <>) . getPubKeyHash . getUserId . toUserId $ wal

govCurrency = "8fe3a8799d69c2852500ccf31abc0a129f668cfd9e905b3d75447a32"

getFreeGov :: Wallet -> Plutus.V1.Ledger.Value.Value -> Integer
getFreeGov wal val = valueOf val govCurrency tn
  where
    tn = TokenName . ("freeGov" <>) . getPubKeyHash . getUserId . toUserId $ wal

appSymbol :: UniqueToken
appSymbol = AssetClass ("038ecf2f85dcb99b41d7ebfcbc0d988f4ac2971636c3e358aa8d6121", "Unique App Token")
