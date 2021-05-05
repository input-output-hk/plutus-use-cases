-- | State transitions for Lending app
module Mlabs.Lending.Logic.State(
    St
  , showt
  , Error
  , initReserve
  , guardError
  , getWallet, getsWallet
  , getUser, getsUser
  , getReserve, getsReserve
  , toAda
  , getTotalCollateral
  , getTotalBorrow
  , getTotalDeposit
  , getLiquidationThreshold
  , getHealth
  , getHealthCheck
  , modifyReserve
  , modifyReserveWallet
  , modifyUser
  , modifyWallet
  , modifyWalletAndReserve
) where

import Prelude

import Control.Monad.Except
import Control.Monad.State.Strict

import Data.Maybe
import Data.Text
import Mlabs.Lending.Logic.Types

import qualified Data.Map.Strict as M
import qualified Data.Text as T

showt :: Show a => a -> Text
showt = T.pack . show

type Error = Text

-- | State update of lending pool
type St = StateT LendingPool (Either Error)

----------------------------------------------------
-- common functions

guardError :: Text -> Bool -> St ()
guardError msg isTrue
  | isTrue    = pure ()
  | otherwise = throwError msg

getsWallet :: UserId -> Coin -> (Wallet -> a) -> St a
getsWallet uid coin f = fmap f $ getWallet uid coin

getWallet :: UserId -> Coin -> St Wallet
getWallet uid coin =
  getsUser uid (fromMaybe defaultWallet . M.lookup coin . user'wallets)

getsUser :: UserId -> (User -> a) -> St a
getsUser uid f = fmap f $ getUser uid

getUser :: UserId -> St User
getUser uid = gets (fromMaybe defaultUser . M.lookup uid . lp'users)

getsReserve :: Coin -> (Reserve -> a) -> St a
getsReserve coin extract = fmap extract $ getReserve coin

getReserve :: Coin -> St Reserve
getReserve coin = do
  mReserve <- gets (M.lookup coin . lp'reserves)
  maybe err pure mReserve
  where
    err = throwError $ "Uknown coin " <> showt coin

-- | Convert given currency to base currency
toAda :: Coin -> Integer -> St Integer
toAda coin val = do
  ratio <- fmap reserve'rate $ getReserve coin
  pure $ ceiling $ fromInteger val * ratio

-- | Weigted total of currencies in base currency
weightedTotal :: [(Coin, Integer)] -> St Integer
weightedTotal = fmap sum . mapM (uncurry toAda)

-- | Collects cumulative value for given wallet field
walletTotal :: (Wallet -> Integer) -> User -> St Integer
walletTotal extract (User ws) = weightedTotal $ M.toList $ fmap extract ws

-- | Gets total collateral for a user.
getTotalCollateral :: User -> St Integer
getTotalCollateral = walletTotal wallet'collateral

-- | Gets total borrows for a user in base currency.
getTotalBorrow :: User -> St Integer
getTotalBorrow = walletTotal wallet'borrow

-- | Gets total deposit for a user in base currency.
getTotalDeposit :: User -> St Integer
getTotalDeposit = walletTotal wallet'deposit

getHealthCheck :: Integer -> Coin -> User -> St Bool
getHealthCheck addToBorrow coin user = fmap (> 1) $ getHealth addToBorrow coin user

-- | Check borrowing health for the user by given currency
getHealth :: Integer -> Coin -> User -> St Rational
getHealth addToBorrow coin user = do
  col <- getTotalCollateral user
  bor <- fmap (+ addToBorrow) $ getTotalBorrow user
  liq <- getLiquidationThreshold coin
  pure $ fromInteger col * liq / fromInteger bor

getLiquidationThreshold :: Coin -> St Rational
getLiquidationThreshold coin =
  gets (maybe 0 reserve'liquidationThreshold . M.lookup coin . lp'reserves)

modifyReserve :: Coin -> (Reserve -> Either Text Reserve) -> St ()
modifyReserve asset f = do
  LendingPool lp users <- get
  case M.lookup asset lp of
    Just reserve -> either throwError (\x -> put $ LendingPool (M.insert asset x lp) users) (f reserve)
    Nothing      -> throwError $ mconcat ["Asset is not supported: ", showt asset]

modifyUser :: UserId -> (User -> Either Text User) -> St ()
modifyUser uid f = do
  LendingPool lp users <- get
  case f $ fromMaybe defaultUser $ M.lookup uid users of
    Left msg   -> throwError msg
    Right user -> put $ LendingPool lp (M.insert uid user users)

-- | Applies the same modification function to the user and to the reserve wallet.
modifyWalletAndReserve :: UserId -> Coin -> (Wallet -> Either Text Wallet) -> St ()
modifyWalletAndReserve uid coin f = do
  modifyWallet uid coin f
  modifyReserveWallet coin f

modifyReserveWallet :: Coin -> (Wallet -> Either Text Wallet) -> St ()
modifyReserveWallet coin f =
  modifyReserve coin $ \r -> fmap (\w -> r { reserve'wallet = w }) $ f $ reserve'wallet r

modifyWallet :: UserId -> Coin -> (Wallet -> Either Text Wallet) -> St ()
modifyWallet uid coin f = modifyUser uid $ \(User ws) -> do
  wal <- f $ fromMaybe defaultWallet $ M.lookup coin ws
  pure $ User $ M.insert coin wal ws

