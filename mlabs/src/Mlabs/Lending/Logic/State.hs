{-# OPTIONS_GHC -fno-specialize #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | State transitions for Lending app
module Mlabs.Lending.Logic.State(
    St
  , showt
  , Error
  , isNonNegative
  , isPositive
  , isPositiveRational
  , isUnitRange
  , isAsset
  , aToken
  , updateReserveState
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
  , modifyReserve'
  , modifyReserveWallet'
  , modifyUser'
  , modifyWallet'
  , modifyWalletAndReserve'
  , getNormalisedIncome
  , getCumulativeBalance
) where

import qualified PlutusTx.Ratio as R
import qualified PlutusTx.Numeric as N
import PlutusTx.Prelude
import qualified PlutusTx.AssocMap as M

import Control.Monad.Except       hiding (Functor(..), mapM)
import Control.Monad.State.Strict hiding (Functor(..), mapM)

import qualified Mlabs.Lending.Logic.InterestRate as IR
import Mlabs.Lending.Logic.Types

-- | Type for errors
type Error = String

-- | State update of lending pool
type St = StateT LendingPool (Either Error)

instance Functor St where
  {-# INLINABLE fmap #-}
  fmap f (StateT a) = StateT $ fmap (\(v, st) -> (f v, st)) . a

instance Applicative St where
  {-# INLINABLE pure #-}
  pure a = StateT (\st -> Right (a, st))

  {-# INLINABLE (<*>) #-}
  (StateT f) <*> (StateT a) = StateT $ \st -> case f st of
    Left err -> Left err
    Right (f1, st1) -> fmap (\(a1, st2) -> (f1 a1, st2)) $ a st1

----------------------------------------------------
-- common functions
{-# INLINABLE isNonNegative #-}
isNonNegative :: String -> Integer -> St ()
isNonNegative msg val
  | val >= 0  = pure ()
  | otherwise = throwError $ msg <> " should be non-negative"

{-# INLINABLE isPositive #-}
isPositive :: String -> Integer -> St ()
isPositive msg val
  | val > 0   = pure ()
  | otherwise = throwError $ msg <> " should be positive"

{-# INLINABLE isPositiveRational #-}
isPositiveRational :: String -> Rational -> St ()
isPositiveRational msg val
  | val > R.fromInteger 0 = pure ()
  | otherwise             = throwError $ msg <> " should be positive"

{-# INLINABLE isUnitRange #-}
isUnitRange :: String -> Rational -> St ()
isUnitRange msg val
  | val >= R.fromInteger 0 && val <= R.fromInteger 1 = pure ()
  | otherwise                                        = throwError $ msg <> " should have unit range [0, 1]"

{-# INLINABLE isAsset #-}
isAsset :: Coin -> St ()
isAsset asset = do
  reserves <- gets lp'reserves
  if M.member asset reserves
    then pure ()
    else throwError "Asset not supported"

{-# INLINABLE updateReserveState #-}
updateReserveState :: Integer -> Coin -> St ()
updateReserveState currentTime asset =
  modifyReserve asset $ IR.updateReserveInterestRates currentTime

{-# INLINABLE aToken #-}
aToken :: Coin -> St Coin
aToken coin = do
  mCoin <- gets (\st -> toLendingToken st coin)
  maybe err pure mCoin
  where
    err = throwError "Coin not supported"

{-# INLINABLE guardError #-}
-- | Execute further if condition is True or throw error with
-- given error message.
guardError :: Error -> Bool -> St ()
guardError msg isTrue
  | isTrue    = pure ()
  | otherwise = throwError msg

{-# INLINABLE getsWallet #-}
-- | Read field from the internal wallet for user and on asset.
-- If there is no wallet empty wallet is allocated.
getsWallet :: UserId -> Coin -> (Wallet -> a) -> St a
getsWallet uid coin f = fmap f $ getWallet uid coin

-- | Get internal wallet for user on given asset.
{-# INLINABLE getWallet #-}
getWallet :: UserId -> Coin -> St Wallet
getWallet uid coin =
  getsUser uid (fromMaybe defaultWallet . M.lookup coin . user'wallets)

{-# INLINABLE getsUser #-}
-- | Get user info in the lending app by user id and apply extractor function to it.
getsUser :: UserId -> (User -> a) -> St a
getsUser uid f = fmap f $ getUser uid

{-# INLINABLE getUser #-}
-- | Get user info in the lending app by user id.
getUser :: UserId -> St User
getUser uid = gets (fromMaybe defaultUser . M.lookup uid . lp'users)

{-# INLINABLE getsReserve #-}
-- | Read reserve for a given asset and apply extractor function to it.
getsReserve :: Coin -> (Reserve -> a) -> St a
getsReserve coin extract = fmap extract $ getReserve coin

{-# INLINABLE getReserve #-}
-- | Read reserve for a given asset.
getReserve :: Coin -> St Reserve
getReserve coin = do
  mReserve <- gets (M.lookup coin . lp'reserves)
  maybe err pure mReserve
  where
    err = throwError "Uknown coin"

{-# INLINABLE toAda #-}
-- | Convert given currency to base currency
toAda :: Coin -> Integer -> St Integer
toAda coin val = do
  ratio <- fmap reserve'rate $ getReserve coin
  pure $ R.round $ R.fromInteger val N.* ratio

{-# INLINABLE weightedTotal #-}
-- | Weigted total of currencies in base currency
weightedTotal :: [(Coin, Integer)] -> St Integer
weightedTotal = fmap sum . mapM (uncurry toAda)

{-# INLINABLE walletTotal #-}
-- | Collects cumulative value for given wallet field
walletTotal :: (Wallet -> Integer) -> User -> St Integer
walletTotal extract (User ws) = weightedTotal $ M.toList $ fmap extract ws

{-# INLINABLE getTotalCollateral #-}
-- | Gets total collateral for a user.
getTotalCollateral :: User -> St Integer
getTotalCollateral = walletTotal wallet'collateral

{-# INLINABLE getTotalBorrow #-}
-- | Gets total borrows for a user in base currency.
getTotalBorrow :: User -> St Integer
getTotalBorrow = walletTotal wallet'borrow

{-# INLINABLE getTotalDeposit #-}
-- | Gets total deposit for a user in base currency.
getTotalDeposit :: User -> St Integer
getTotalDeposit = walletTotal wallet'deposit

{-# INLINABLE getHealthCheck #-}
-- | Check that user has enough health for the given asset.
getHealthCheck :: Integer -> Coin -> User -> St Bool
getHealthCheck addToBorrow coin user =
  fmap (> R.fromInteger 1) $ getHealth addToBorrow coin user

{-# INLINABLE getHealth #-}
-- | Check borrowing health for the user by given currency
getHealth :: Integer -> Coin -> User -> St Rational
getHealth addToBorrow coin user = do
  col <- getTotalCollateral user
  bor <- fmap (+ addToBorrow) $ getTotalBorrow user
  liq <- getLiquidationThreshold coin
  pure $ R.fromInteger col N.* liq N.* (R.recip $ R.fromInteger bor)

{-# INLINABLE getLiquidationThreshold #-}
-- | Reads liquidation threshold for a give asset.
getLiquidationThreshold :: Coin -> St Rational
getLiquidationThreshold coin =
  gets (maybe (R.fromInteger 0) reserve'liquidationThreshold . M.lookup coin . lp'reserves)

{-# INLINABLE modifyReserve #-}
-- | Modify reserve for a given asset.
modifyReserve :: Coin -> (Reserve -> Reserve) -> St ()
modifyReserve coin f = modifyReserve' coin (Right . f)

{-# INLINABLE modifyReserve' #-}
-- | Modify reserve for a given asset. It can throw errors.
modifyReserve' :: Coin -> (Reserve -> Either Error Reserve) -> St ()
modifyReserve' asset f = do
  LendingPool lp users curSym coinMap <- get
  case M.lookup asset lp of
    Just reserve -> either throwError (\x -> put $ LendingPool (M.insert asset x lp) users curSym coinMap) (f reserve)
    Nothing      -> throwError $ "Asset is not supported"

{-# INLINABLE modifyUser #-}
-- | Modify user info by id.
modifyUser :: UserId -> (User -> User) -> St ()
modifyUser uid f = modifyUser' uid (Right . f)

{-# INLINABLE modifyUser' #-}
-- | Modify user info by id. It can throw errors.
modifyUser' :: UserId -> (User -> Either Error User) -> St ()
modifyUser' uid f = do
  LendingPool lp users curSym coinMap <- get
  case f $ fromMaybe defaultUser $ M.lookup uid users of
    Left msg   -> throwError msg
    Right user -> put $ LendingPool lp (M.insert uid user users) curSym coinMap

{-# INLINABLE modifyWalletAndReserve #-}
-- | Modify user wallet and reserve wallet with the same function.
modifyWalletAndReserve :: UserId -> Coin -> (Wallet -> Wallet) -> St ()
modifyWalletAndReserve uid coin f = modifyWalletAndReserve' uid coin (Right . f)

{-# INLINABLE modifyWalletAndReserve' #-}
-- | Applies the same modification function to the user and to the reserve wallet. It can throw errors.
modifyWalletAndReserve' :: UserId -> Coin -> (Wallet -> Either Error Wallet) -> St ()
modifyWalletAndReserve' uid coin f = do
  modifyWallet' uid coin f
  modifyReserveWallet' coin f

{-# INLINABLE modifyReserveWallet #-}
-- | Modify reserve wallet for a given asset.
modifyReserveWallet :: Coin -> (Wallet -> Wallet) -> St ()
modifyReserveWallet coin f = modifyReserveWallet' coin (Right . f)

{-# INLINABLE modifyReserveWallet' #-}
-- | Modify reserve wallet for a given asset. It can throw errors.
modifyReserveWallet' :: Coin -> (Wallet -> Either Error Wallet) -> St ()
modifyReserveWallet' coin f =
  modifyReserve' coin $ \r -> fmap (\w -> r { reserve'wallet = w }) $ f $ reserve'wallet r

{-# INLINABLE modifyWallet #-}
-- | Modify internal user wallet that is allocated for a given user id and asset.
modifyWallet :: UserId -> Coin -> (Wallet -> Wallet) -> St ()
modifyWallet uid coin f = modifyWallet' uid coin (Right . f)

{-# INLINABLE modifyWallet' #-}
-- | Modify internal user wallet that is allocated for a given user id and asset.
-- It can throw errors.
modifyWallet' :: UserId -> Coin -> (Wallet -> Either Error Wallet) -> St ()
modifyWallet' uid coin f = modifyUser' uid $ \(User ws) -> do
  wal <- f $ fromMaybe defaultWallet $ M.lookup coin ws
  pure $ User $ M.insert coin wal ws

{-# INLINABLE getNormalisedIncome #-}
getNormalisedIncome :: Coin -> St Rational
getNormalisedIncome asset =
  getsReserve asset $ (ri'normalisedIncome . reserve'interest)

{-# INLINABLE getCumulativeBalance #-}
getCumulativeBalance :: UserId -> Coin -> St Rational
getCumulativeBalance uid asset = do
  ni <- getNormalisedIncome asset
  getsWallet uid asset (IR.getCumulativeBalance ni)

