{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE MonoLocalBinds       #-}
{-# LANGUAGE MultiWayIf           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
module UniswapSpec
  (
    tests
  ) where

import           Control.Lens hiding (elements)
import           Control.Monad
import           Data.Monoid                        (Last (..))
import qualified Data.Semigroup                     as Semigroup
import           Data.String                        (IsString (..))
import           Data.Maybe
import           Data.Map                           (Map)
import qualified Data.Map                           as Map
import  Data.Text (Text)

import qualified Test.Tasty.HUnit                   as HUnit
import Test.Tasty ( TestTree, testGroup )
import Test.Tasty.QuickCheck hiding ((.&&.))
import Test.QuickCheck (quickCheck)

import qualified Ledger.Ada                         as Ada
import           Ledger.Value
import qualified Ledger.Typed.Scripts               as Scripts

import           Plutus.Contract           hiding (when)
import           Plutus.Contract.Test               hiding (not)
import           Plutus.Contract.Test.ContractModel
import           Plutus.Contracts.Uniswap           as U
import           Plutus.Contracts.Validators        as V
import           Plutus.Trace.Emulator              as Trace

import qualified Effects.Uniswap as US
import qualified Plutus.Contracts.Currency               as Currency
import           Plutus.Contracts.Uniswap           (start, ownerEndpoint, userEndpoints,)
import           Plutus.Contracts.Data              (Coin, UniswapContracts (UniswapStart), uniswapTokenName)
import           Plutus.Contracts.Helpers           (mkCoin)
-- import Test.QuickCheck
import PlutusTx.Builtins (ByteString, emptyByteString, encodeUtf8)
import           Data.Void                        (Void)

main :: IO ()
main = putStrLn "testing area, coming soon..."

genWallet :: Gen Wallet
genWallet = elements US.wallets

genNonNeg :: Gen Integer
genNonNeg = getNonNegative <$> arbitrary



-- tokenProperty :: Map Wallet TokenName
-- tokenCurrencies :: Map Wallet CurrencySymbol
-- tokenCurrencies = Map.fromList $ zip wallets tokenNames


-- coins :: Map Wallet Coin
-- coins = Map.fromList [(w, Coin (token))]


newtype UniswapState = UniswapState
     { _usCoin :: Coin }
     deriving Show

makeLenses ''UniswapState

newtype UniswapModel = UniswapModel
        {
          _usModel :: Map Wallet UniswapState
        } deriving Show

makeLenses ''UniswapModel

oneShotCurrencySymbol :: CurrencySymbol
oneShotCurrencySymbol = "4f3c205fda58ef457ba5d104cd432f169f30b1d90082997cf0e374964200b738"

mkUniswapCurrency :: AssetClass
mkUniswapCurrency = mkCoin oneShotCurrencySymbol uniswapTokenName

mkUniswapCoin :: Integer -> Value
mkUniswapCoin = coin mkUniswapCurrency

prop_US :: Actions UniswapModel -> Property
prop_US = withMaxSuccess 100 . propRunActionsWithOptions
         (defaultCheckOptions & emulatorConfig .~ EmulatorConfig (Left d))
         instanceSpec
         (const $ pure True)
    where
        d :: InitialDistribution
        d = Map.fromList $ [ (w, v) | w <- US.wallets]
        cs = oneShotCurrencySymbol
        v  = mconcat [Ledger.Value.singleton cs tn amount | tn <- US.tokenNames]
        amount = 1_000_000

tests :: TestTree
tests = testProperty "uniswap model" prop_US

test :: IO ()
test = quickCheck prop_US

-- data UniswapModel = UniswapModel
--   {
--     _coin   :: Coin,
--     _phase  :: Phase
--   }
--   deriving (Show)

data Phase = NotStarted | Initialized | Started | Closed
    deriving (Eq, Show)


instanceSpec :: [ContractInstanceSpec UniswapModel]
instanceSpec =
  -- [ContractInstanceSpec (InitKey w) w US.initContract | w <- US.wallets]
  [ContractInstanceSpec (StartKey w) w ownerEndpoint | w <- US.wallets]
  -- [ContractInstanceSpec (UserKey w) w userEndpoints | w <- wallets]
  -- [ContractInstanceSpec (UseKey v w) w $ useEndpoints $ tss Map.! v | v <- wallets, w <- wallets]

initErrorHandler = \case
  Just (Semigroup.Last cur) -> pure (Currency.currencySymbol cur)
  _                         -> Trace.throwError $ GenericError "failed to create currency"

startErrorHandler = \case
                Last (Just (Right v)) -> pure v
                _                            -> Trace.throwError $ GenericError "initialisation failed"

traceInitUniswap :: Trace.EmulatorTrace ()
traceInitUniswap = do
  cidInit <- Trace.activateContract (Wallet 1) US.initContract "init"
  _ <- Trace.observableState cidInit >>= initErrorHandler
  _ <- Trace.nextSlot
  void Trace.nextSlot
  -- pure ()

traceStartUniswap :: Trace.EmulatorTrace ()
traceStartUniswap = do
  cidStart <- Trace.activateContract (Wallet 1) ownerEndpoint "start"
  _ <- Trace.observableState cidStart >>= startErrorHandler
  _ <- Trace.nextSlot
  void Trace.nextSlot

-- uniswapTokenVal :: Value
-- uniswapTokenVal =
--     let sym = Scripts.monetaryPolicyHash V.uniswapInstance
--     in G.token sym "guess"

-- token :: MonetaryPolicyHash -> TokenName -> Value
-- token mps tn = V.singleton (V.mpsSymbol mps) tn 1

-- canInitAndStartUniswap :: TestTree
-- canInitAndStartUniswap = defaultCheck


-- initEndpoint :: Contract (Maybe (Semigroup.Last Currency.OneShotCurrency)) Currency.CurrencySchema Currency.CurrencyError ()
-- initEndpoint = do
--     e <- runError start
--     tell $ Last $ Just $ case e of
--         Left err -> Left err
--         Right us -> Right us

-- ownerEndpoint :: Contract (Last (Either Text Uniswap)) EmptySchema Void ()
-- ownerEndpoint = do
--     e <- runError start
--     tell $ Last $ Just $ case e of
--         Left err -> Left err
--

delay :: Int -> EmulatorTrace ()
delay = void . Trace.waitNSlots . fromIntegral

uss :: Map Wallet Uniswap
uss = Map.fromList
  [
    (w, U.uniswap oneShotCurrencySymbol)
    | w <- US.wallets
  ]

-- nft :: ByteString  -> Value
-- nft t = Value.singleton (CurrencySymbol t)  (TokenName emptyByteString)  1

-- configWithTokens :: EmulatorConfig
-- configWithTokens = EmulatorConfig $ Left $ Map.fromList distribution
--   where
--     distribution= [
--         (Wallet 1 , adaFunds <> nft "aa" <> nft "ab" <> nft "ac"),
--         (Wallet 2 , adaFunds <> nft "ba" <> nft "bb" <> nft "bc"),
--         (Wallet 3 , adaFunds <> nft "ca" <> nft "cb" <> nft "cc"),
--         (Wallet 4 , adaFunds <> nft "da" <> nft "db" <> nft "dc"),
--       ]
--     adaFunds :: Value
--     adaFunds = lovelaceValueOf 1_000_000_000

-- defaultCheck :: String -> TracePredicate -> EmulatorTrace () -> TestTree
-- defaultCheck = checkPredicateOptions (defaultCheckOptions & emulatorConfig .~ configurationWithNfts)


instance ContractModel UniswapModel where
  data Action UniswapModel =
        Start Wallet
        -- | User Wallet
      deriving (Show, Eq)

  instanceTag key _ = fromString $ "instance tag for: " ++ (show key)

  data ContractInstanceKey UniswapModel w s e where
    -- InitKey :: Wallet -> ContractInstanceKey UniswapModel () Currency.CurrencySchema Text
    StartKey :: Wallet -> ContractInstanceKey UniswapModel (Last (Either Text Uniswap)) UniswapOwnerSchema Void
    UserKey :: Wallet -> ContractInstanceKey UniswapModel (Last (Either Text UserContractState)) UniswapUserSchema Text

  arbitraryAction _ = oneof $
    -- ( Init  <$> genWallet ) :
    [(Start <$> genWallet)]
    -- [ User <$> genWallet ]
    -- [SetPrice <$> genWallet <*> genWallet <*> genNonNeg ]

  initialState = UniswapModel Map.empty

  -- nextState (Start w) = do
  --   withdraw w $ mkUniswapCoin 1
  --   wait 1

  -- nextState (Init w) = do
  --     phase $= NotStarted
  --     withdraw w (1000000 * 4)
  --     -- withdraw w $  nfts Map.! w
  --     -- (tsModel . at w) $= Just (TSState 0 0 0)
  --     (usModel . at w) $= Just (UniswapState {_usCoin=__usCoin} )
  --     wait 1

  perform h _ cmd = case cmd of
    -- (Init w) -> callEndpoint @"init" (h $ StartKey w) (Currency.OneShotCurrency)
    (Start w) -> callEndpoint @"start" (h $ StartKey w) () >> delay 1

deriving instance Eq (ContractInstanceKey UniswapModel w s e)
deriving instance Show (ContractInstanceKey UniswapModel w s e)

-- tests :: TestTree
-- tests = testGroup "uniswap" [
--     checkPredicate "should spawnxr a new pool"
--     (assertNotDone US.initContract
--                    (Trace.walletInstanceTag w1)
--                    "initContract"
--     .&&. assertNoFailedTransactions)
--     traceInitUniswap

--     , checkPredicate "hm..."
--     (assertNotDone start
--                    (Trace.walletInstanceTag w1)
--                    "start uniswap"
--     .&&. assertNoFailedTransactions)
--     traceStartUniswap
--   ]

