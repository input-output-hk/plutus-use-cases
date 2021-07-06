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
import           Plutus.Contracts.PoolForgery       (create)
import           Plutus.Contracts.Data
import           Plutus.Contracts.Helpers           (mkCoin)
-- import Test.QuickCheck
import PlutusTx.Builtins (ByteString, emptyByteString, encodeUtf8)
import           Data.Void                        (Void)
import           Ledger.Ada                              (adaSymbol, adaToken)

genWallet :: Gen Wallet
genWallet = elements US.wallets

genStartUSWallet :: Gen Wallet
genStartUSWallet = elements [Wallet 1]

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

-- mkUniswapCoin :: Integer -> Value
-- mkUniswapCoin = coin mkUniswapCurrency 
-- mkSwapCoin :: AssetClass -> Integer -> Value
-- mkSwapCoin a i = coin a i

usFactoryNFT :: Value
usFactoryNFT = Ledger.Value.singleton oneShotCurrencySymbol uniswapTokenName 1

swapToken :: TokenName -> Value
swapToken tn = Ledger.Value.singleton oneShotCurrencySymbol tn 1_000_000

coins :: Map TokenName AssetClass
coins = Map.fromList [(tn, mkCoin oneShotCurrencySymbol tn) | tn <- US.tokenNames]
ada   = mkCoin adaSymbol adaToken

prop_US :: Actions UniswapModel -> Property
prop_US = withMaxSuccess 20 . propRunActionsWithOptions
         (defaultCheckOptions & emulatorConfig .~ configWithTokens)
         instanceSpec
         (const $ pure True)
        -- d = Map.fromList $ [ (w, v) | w <- US.wallets]
        -- cs = oneShotCurrencySymbol
        -- v  = mconcat [Ledger.Value.singleton cs tn amount | tn <- US.tokenNames]
          
        -- amount = 1_000_000

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

-- initEndpoint :: Contract (Maybe (Semigroup.Last Currency.OneShotCurrency)) Currency.CurrencySchema Currency.CurrencyError ()
-- initEndpoint = do
--     e <- runError start
--     tell $ Last $ Just $ case e of
--         Left err -> Left err
--         Right us -> Right us

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

configWithTokens :: EmulatorConfig
configWithTokens = EmulatorConfig $ Left $ Map.fromList distribution
  where
    distribution= [
        (Wallet 1,  swapToken "A" <> swapToken "B" <> swapToken "C" <> swapToken "D"),
        (Wallet 2 , swapToken "A" <> swapToken "B" <> swapToken "C" <> swapToken "D"),
        (Wallet 3 , swapToken "A" <> swapToken "B" <> swapToken "C" <> swapToken "D"),
        (Wallet 4 , swapToken "A" <> swapToken "B" <> swapToken "C" <> swapToken "D")
      ]
    
    -- d = Map.fromList $ [ (w, v) | w <- US.wallets]
    -- cs = oneShotCurrencySymbol
    -- v  = mconcat [Ledger.Value.singleton cs tn amount | tn <- US.tokenNames]
          

-- defaultCheck :: String -> TracePredicate -> EmulatorTrace () -> TestTree
-- defaultCheck = checkPredicateOptions (defaultCheckOptions & emulatorConfig .~ configurationWithNfts)

instanceSpec :: [ContractInstanceSpec UniswapModel]
instanceSpec =
  -- [ContractInstanceSpec (InitKey w) w US.initContract | w <- US.wallets]
  [ContractInstanceSpec (StartKey w) w ownerEndpoint | w <- [Wallet 1]] ++ 
  -- [ContractInstanceSpec (CreatePoolKey w) w create | w <- US.wallets] ++ 
  [ContractInstanceSpec (UserKey w) w $ userEndpoints $ uss Map.! w | w <- US.wallets]
  -- [ContractInstanceSpec (UseKey v w) w $ useEndpoints $ tss Map.! v | v <- wallets, w <- wallets]

instance ContractModel UniswapModel where
  data Action UniswapModel =
        Start Wallet
        | User Wallet
        | CreatePool Wallet
      deriving (Show, Eq)

  instanceTag key _ = fromString $ "instance tag for: " ++ (show key)

  data ContractInstanceKey UniswapModel w s e where
    -- InitKey :: Wallet -> ContractInstanceKey UniswapModel () Currency.CurrencySchema Text
    StartKey :: Wallet -> ContractInstanceKey UniswapModel (Last (Either Text Uniswap)) UniswapOwnerSchema Void
    UserKey :: Wallet -> ContractInstanceKey UniswapModel (Last (Either Text UserContractState)) UniswapUserSchema Void
    -- CreatePoolKey :: Wallet -> ContractInstanceKey UniswapModel (Last (Either Text UserContractState)) UniswapUserSchema Text

  arbitraryAction _ = oneof $
    -- ( Init  <$> genWallet ) :
    [(Start <$> genStartUSWallet), CreatePool <$> genWallet]
    -- [ User <$> genWallet ]
    -- [SetPrice <$> genWallet <*> genWallet <*> genNonNeg ]

  initialState = UniswapModel Map.empty

  nextState (Start w) = do
    -- withdraw w usFactoryNFT
    (usModel . at w) $= Just (UniswapState mkUniswapCurrency)
    wait 1

  nextState (CreatePool w) = do
    -- withdraw w $ coin (coins Map.! "A") 500_000
    wait 1
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
    (CreatePool w) -> callEndpoint @"create" (h $ UserKey w) cp >> delay 1

    where
        cp = CreateParams ada (coins Map.! "A") 100000 500000
    -- (AddLiquidity) -> callEndpoint @"create" (h $ UserKey w) () >> delay 1
    -- (SwapTokens) -> callEndpoint @"create" (h $ UserKey w) () >> delay 1

  precondition s (Start w) = isNothing $ getUniswapState s w
  precondition s (CreatePool w)   = isJust $ getUniswapState s w

deriving instance Eq (ContractInstanceKey UniswapModel w s e)
deriving instance Show (ContractInstanceKey UniswapModel w s e)

getUniswapState :: ModelState UniswapModel -> Wallet -> Maybe UniswapState
getUniswapState s v = s ^. contractState . usModel . at v

traceTests :: [TestTree]
traceTests = [
    -- checkPredicate "can prepare token distribution for uniswap"
    -- (assertNotDone US.initContract
    --                (Trace.walletInstanceTag (Wallet 1))
    --                "initContract"
    -- .&&. assertNoFailedTransactions)
    -- traceInitUniswap

    -- , 
    checkPredicate "can start uniswap instance"
    (assertNotDone ownerEndpoint 
                   (Trace.walletInstanceTag (Wallet 1))
                   "start uniswap"
    .&&. assertNoFailedTransactions)
    traceStartUniswap
  ]

propTests :: TestTree
propTests = (testProperty "uniswap model" prop_US)

tests :: TestTree
tests = testGroup "uniswap" ([propTests] <> traceTests)
