{-# LANGUAGE NumericUnderscores #-}

module Test.Lending.QuickCheck where

import Mlabs.Emulator.Types (UserId(..), Coin, adaCoin)
import Mlabs.Lending.Logic.Types (UserAct(..))
import Mlabs.Lending.Logic.App (AppConfig(..), Script, runLendingApp, userAct)
import Mlabs.Emulator.Blockchain (BchWallet(..))
import Mlabs.Emulator.App (App(..), lookupAppWallet)
import Test.Lending.Logic (fromToken, testAppConfig, coin1, coin2, coin3, user1, user2, user3)
import qualified Plutus.V1.Ledger.Value as Value
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import qualified Test.QuickCheck as QC


allUsers :: [UserId]
allUsers = [Self, user1, user2, user3]

users :: [UserId]
users = drop 1 allUsers

coins :: [Coin]
coins = [adaCoin, coin1, coin2, coin3]

nonNativeCoins :: [Coin]
nonNativeCoins = drop 1 coins

aToken :: Coin -> Value.TokenName
aToken (Value.AssetClass (_, Value.TokenName tn)) = Value.TokenName ("a" <> tn)

aCoin :: Coin -> Coin
aCoin coin = fromToken (aToken coin)

-- Various integer generators
smallGenSize :: Int
smallGenSize = 100

bigGenSize :: Int
bigGenSize = 1_000_000_000_000_000_000

positiveSmallInteger :: QC.Gen Integer
positiveSmallInteger = fmap QC.getPositive (QC.resize smallGenSize QC.arbitrary)

positiveBigInteger :: QC.Gen Integer
positiveBigInteger = (*) <$> gen <*> gen
  where gen = fmap QC.getPositive (QC.resize bigGenSize QC.arbitrary)

nonPositiveSmallInteger :: QC.Gen Integer
nonPositiveSmallInteger = fmap (negate . abs) (QC.resize smallGenSize QC.arbitrary)

nonPositiveBigInteger :: QC.Gen Integer
nonPositiveBigInteger = (\x y -> negate (abs (x * y))) <$> gen <*> gen
  where gen = fmap negate (QC.resize bigGenSize QC.arbitrary)

positiveInteger :: QC.Gen Integer
positiveInteger = QC.frequency [(1, positiveSmallInteger), (1, positiveBigInteger)]

nonPositiveInteger :: QC.Gen Integer
nonPositiveInteger = QC.frequency [(1, nonPositiveSmallInteger), (1, nonPositiveBigInteger)]

-- | Contains parameters that deposit test cases can be generalized over
data DepositTestInput = DepositTestInput
  { deposits :: [(UserId, Coin, Integer)] }
  deriving Show

-- | Construct a `Script`
createDepositScript :: DepositTestInput -> Script
createDepositScript (DepositTestInput ds) =
  mapM_ (\(user, coin, amt) -> userAct user $ DepositAct amt coin) ds

noErrorsProp :: App st act -> Bool
noErrorsProp app = null (app.app'log)

someErrorsProp :: App st act -> Bool
someErrorsProp app = not (null (app.app'log))

hasWallet :: App st act -> UserId -> BchWallet -> Bool
hasWallet app uid wal = lookupAppWallet uid app == Just wal

checkWalletsProp :: (Show act, Show st) => [(UserId, BchWallet)] -> App st act -> Bool
checkWalletsProp wals app = all (uncurry $ hasWallet app) wals

-- Map maniplation helper functions
walletListToNestedMap :: [(UserId, BchWallet)] -> Map UserId (Map Coin Integer)
walletListToNestedMap wals =
  addNestedMaps $ map (\(user, BchWallet wal) -> Map.singleton user wal) wals

nestedMapToWalletList :: Map UserId (Map Coin Integer) -> [(UserId, BchWallet)]
nestedMapToWalletList m = Map.toAscList (Map.map BchWallet m)

addNestedMaps :: [Map UserId (Map Coin Integer)] -> Map UserId (Map Coin Integer)
addNestedMaps = Map.unionsWith (Map.unionWith (+))

-- | Calculate expected balances after running deposit script
expectedWalletsDeposit :: AppConfig -> DepositTestInput -> [(UserId, BchWallet)]
expectedWalletsDeposit appCfg (DepositTestInput ds) =
  let startingBalances = walletListToNestedMap (appConfig'users appCfg)
      depositedCoins = map (\(user, coin, amt) -> Map.singleton user (Map.singleton coin (negate amt))) ds
      aCoins = map (\(user, coin, amt) -> Map.singleton user (Map.singleton (aCoin coin) amt)) ds
      appCoins = Map.singleton Self $ Map.unionsWith (+) (map (\(_, coin, amt) -> Map.singleton coin amt) ds)
      appAcoins = Map.singleton Self $ Map.fromList $ map (\(_, coin, _) -> (aCoin (coin), 0)) ds
      allWallets = addNestedMaps ([startingBalances] ++ depositedCoins ++ aCoins ++ [appCoins] ++ [appAcoins])
  in Map.toAscList (Map.map BchWallet allWallets)

-- | Check that the balances after deposit script run correspond to the expected balances
testWalletsProp :: [(UserId, BchWallet)] -> Script -> Bool
testWalletsProp expectedWals script = 
  let app = runLendingApp testAppConfig script
  in noErrorsProp app && checkWalletsProp expectedWals app

testWalletsProp' :: DepositTestInput -> Bool
testWalletsProp' d =
  let script = createDepositScript d
  in testWalletsProp (expectedWalletsDeposit testAppConfig d) script

depositInputGen :: QC.Gen Integer -> QC.Gen DepositTestInput
depositInputGen integerGen =
  fmap (DepositTestInput . zip3 users nonNativeCoins) (QC.vectorOf n integerGen)
  where n = length users

testDepositLogic :: QC.Property
testDepositLogic = QC.forAll (depositInputGen (QC.choose (1, 100))) (testWalletsProp')

test :: TestTree
test = testGroup "QuickCheck" [testGroup "Logic" [testProperty "deposit" testDepositLogic]]
