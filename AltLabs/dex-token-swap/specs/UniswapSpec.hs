{-# LANGUAGE BlockArguments #-}
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
{-# LANGUAGE RecordWildCards #-}
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

import           Plutus.Contracts.Uniswap           (start', ownerEndpoint, userEndpoints,)
import           Plutus.Contracts.PoolForgery       (create)
import           Plutus.Contracts.Data
import           Plutus.Contracts.Helpers           (mkCoin, liquidityCurrency, poolStateCoin)
-- import Test.QuickCheck
import           PlutusTx.Builtins                  (ByteString, emptyByteString, encodeUtf8)
import           Data.Void                          (Void)
import           Ledger.Ada                         (adaSymbol, adaToken)
import           Plutus.Contracts.LiquidityPool     (calculateInitialLiquidity)

-- Currency mock helpers
oneShotCurrencySymbol :: CurrencySymbol
oneShotCurrencySymbol = "4f3c205fda58ef457ba5d104cd432f169f30b1d90082997cf0e374964200b738"

mkUniswapCurrency :: AssetClass
mkUniswapCurrency = mkCoin oneShotCurrencySymbol uniswapTokenName 

usFactoryNFT :: Value
usFactoryNFT = Ledger.Value.singleton oneShotCurrencySymbol uniswapTokenName 1

swapToken :: TokenName -> Value
swapToken tn = Ledger.Value.singleton oneShotCurrencySymbol tn 1_000_000

coins :: Map TokenName AssetClass
coins = Map.fromList [(tn, mkCoin oneShotCurrencySymbol tn) | tn <- US.tokenNames]

ada :: AssetClass
ada   = mkCoin adaSymbol adaToken

coinList :: [Coin]
coinList = [mkCoin oneShotCurrencySymbol tn | tn <- US.tokenNames]

-- GENERATORS
genWallet :: Gen Wallet
genWallet = elements US.wallets

genStartUSWallet :: Gen Wallet
genStartUSWallet = elements [Wallet 1]

genNonNeg :: Gen Integer
genNonNeg = elements [1000, 2000, 3000, 4132, 51890, 8400, 3200]

genCoin :: Gen Coin
genCoin = elements coinList

-- tokenProperty :: Map Wallet TokenName
-- tokenCurrencies :: Map Wallet CurrencySymbol
-- tokenCurrencies = Map.fromList $ zip wallets tokenNames

-- coins :: Map Wallet Coin
-- coins = Map.fromList [(w, Coin (token))]

newtype UniswapState = UniswapState
     { _usCoin :: Coin }
     deriving Show

makeLenses ''UniswapState

type PoolState = ((Coin, Integer),(Coin, Integer))

data UniswapModel = UniswapModel
        {
          _usModel :: Map Wallet UniswapState,
          _usPools  :: Map LiquidityPool PoolState
        } deriving Show

makeLenses ''UniswapModel

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

uniswap' :: Uniswap
uniswap' = U.uniswap oneShotCurrencySymbol

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
        (Wallet 1,  love <> usFactoryNFT  <> swapToken "A" <> swapToken "B" <> swapToken "C" <> swapToken "D"),
        (Wallet 2 , love <> swapToken "A" <> swapToken "B" <> swapToken "C" <> swapToken "D"),
        (Wallet 3 , love <> swapToken "A" <> swapToken "B" <> swapToken "C" <> swapToken "D"),
        (Wallet 4 , love <> swapToken "A" <> swapToken "B" <> swapToken "C" <> swapToken "D")
      ]
    love = Ada.lovelaceValueOf 1_000_000_000
    
instanceSpec :: [ContractInstanceSpec UniswapModel]
instanceSpec =
  [ContractInstanceSpec (StartKey w) w ownerEndpoint' | w <- [Wallet 1]] ++ 
  [ContractInstanceSpec (UserKey w) w $ userEndpoints $ uss Map.! w | w <- US.wallets]
  -- [ContractInstanceSpec (UseKey v w) w $ useEndpoints $ tss Map.! v | v <- wallets, w <- wallets]

type UniswapOwnerSchema' = Endpoint "start" CurrencySymbol

ownerEndpoint' :: Contract (Last (Either Text Uniswap)) UniswapOwnerSchema' Void ()
ownerEndpoint' = startHandler >> ownerEndpoint'
    where
      startHandler :: Contract (Last (Either Text Uniswap)) UniswapOwnerSchema' Void ()
      startHandler = do
           e <- runError $ (endpoint @"start" >>= start')
           tell $ Last $ Just e

instance ContractModel UniswapModel where
  data Action UniswapModel =
        Start Wallet
        | CreatePool Wallet Coin Coin Integer Integer
      deriving (Show, Eq)

  instanceTag key _ = fromString $ "instance tag for: " ++ show key

  data ContractInstanceKey UniswapModel w s e where
    StartKey :: Wallet -> ContractInstanceKey UniswapModel (Last (Either Text Uniswap)) UniswapOwnerSchema' Void
    UserKey :: Wallet -> ContractInstanceKey UniswapModel (Last (Either Text UserContractState)) UniswapUserSchema Void

  arbitraryAction _ = oneof
    [
      Start <$> genStartUSWallet, 
      CreatePool <$> genWallet <*> genCoin <*> genCoin <*> genNonNeg <*> genNonNeg
    ]
  initialState = UniswapModel Map.empty Map.empty

  nextState (Start w) = do
    withdraw w usFactoryNFT
    (usModel . at w) $= Just (UniswapState mkUniswapCurrency)
    wait 2

  nextState (CreatePool v coinA coinB aAmt bAmt) = do
    when (coinA /= coinB) $ do

      let pool@LiquidityPool{..} = LiquidityPool coinA coinB
      let pool'@LiquidityPool{..} = LiquidityPool coinB coinA
      sPool <- getPoolState pool
      sPool' <- getPoolState pool'
      sUniswap <- getUniswapState' v

      case (sUniswap, sPool, sPool') of
        (Just _, Nothing, Nothing) -> do
                              (usPools . at pool) $= Just (mkPool (coinA, coinB) aAmt bAmt)
                              deposit v $ coin lpC $ calculateInitialLiquidity aAmt bAmt
                              withdraw v $ coin coinA aAmt <> coin coinB bAmt
        _                  -> return ()
       
    wait 2

      where
        lpC = mkCoin (liquidityCurrency uniswap') (lpTicker lp)
        lp  = LiquidityPool { lpCoinA = coinA, lpCoinB = coinB }
    

  perform h _ cmd = case cmd of
    (Start w) -> callEndpoint @"start" (h $ StartKey w) oneShotCurrencySymbol >> delay 1
    (CreatePool v c1 c2 c1a c2a) -> callEndpoint @"create" 
                                        (h $ UserKey v) 
                                        CreateParams {
                                          cpCoinA = c1,
                                          cpCoinB = c2,
                                          cpAmountA = c1a,
                                          cpAmountB = c2a
                                        } >> delay 2
    where
        ap = AddParams { 
                          apCoinA = ada,
                          apCoinB = coins Map.! "A", 
                          apAmountA = 800, 
                          apAmountB = 1400 }


  precondition s (Start w) = isNothing $ getUniswapState s w
  precondition s (CreatePool v _ _ _ _) = isJust $ getUniswapState s v

deriving instance Eq (ContractInstanceKey UniswapModel w s e)
deriving instance Show (ContractInstanceKey UniswapModel w s e)

getUniswapState :: ModelState UniswapModel -> Wallet -> Maybe UniswapState
getUniswapState s v = s ^. contractState . usModel . at v

getUniswapState' :: Wallet -> Spec UniswapModel (Maybe UniswapState)
getUniswapState' w = do
    s <- getModelState
    return $ getUniswapState s w

getPoolState' :: ModelState UniswapModel -> LiquidityPool -> Maybe PoolState
getPoolState' s pool@LiquidityPool{..} = s ^. contractState . usPools . at pool

getPoolState :: LiquidityPool -> Spec UniswapModel (Maybe PoolState)
getPoolState pool = do
    s <- getModelState
    return $ getPoolState' s pool

mkPool :: (Coin, Coin) -> Integer -> Integer -> PoolState
mkPool (ca, cb) a b = ((ca, a), (cb, b))

traceTests :: [TestTree]
traceTests = [
    checkPredicate "can prepare token distribution for uniswap"
    (assertNotDone US.initContract
                   (Trace.walletInstanceTag (Wallet 1))
                   "initContract"
    .&&. assertNoFailedTransactions)
    traceInitUniswap

    , 
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
tests = testGroup "uniswap" ([propTests])
