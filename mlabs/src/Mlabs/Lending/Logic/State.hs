{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-specialize #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fobject-code #-}

-- | State transitions for Lending app
module Mlabs.Lending.Logic.State (
  St,
  Error,
  isAsset,
  aToken,
  isAdmin,
  isTrustedOracle,
  updateReserveState,
  initReserve,
  guardError,
  getWallet,
  getAllWallets,
  getsWallet,
  getsAllWallets,
  getUser,
  getsUser,
  getAllUsers,
  getsAllUsers,
  getUsers,
  getReserve,
  getsReserve,
  toAda,
  fromAda,
  Convert (..),
  reverseConvert,
  convertCoin,
  getTotalCollateral,
  getTotalBorrow,
  getTotalDeposit,
  getLiquidationThreshold,
  getLiquidationBonus,
  getHealth,
  getCurrentHealthCheck,
  getHealthCheck,
  getCurrentHealth,
  modifyUsers,
  modifyReserve,
  modifyReserveWallet,
  modifyUser,
  modifyWallet,
  modifyWalletAndReserve,
  modifyReserve',
  modifyReserveWallet',
  modifyUser',
  modifyWallet',
  modifyWalletAndReserve',
  modifyHealthReport,
  getNormalisedIncome,
  getCumulativeBalance,
  getWalletCumulativeBalance,
) where

import PlutusTx.Prelude
import Prelude qualified as Hask (Show, String, uncurry)

import Control.Monad.Except (MonadError (throwError))
import Control.Monad.State.Strict (MonadState (get, put), gets, modify')
import PlutusTx.AssocMap (Map)
import PlutusTx.AssocMap qualified as M
import PlutusTx.Numeric qualified as N

import Mlabs.Control.Monad.State (PlutusState, guardError)
import Mlabs.Lending.Logic.InterestRate qualified as IR
import Mlabs.Lending.Logic.Types (
  CoinRate (coinRate'value),
  LendingPool (
    lp'admins,
    lp'healthReport,
    lp'reserves,
    lp'trustedOracles,
    lp'users
  ),
  Reserve (
    reserve'interest,
    reserve'liquidationBonus,
    reserve'liquidationThreshold,
    reserve'rate,
    reserve'wallet
  ),
  ReserveInterest (ri'normalisedIncome),
  User (User, user'wallets),
  Wallet (wallet'borrow, wallet'collateral, wallet'deposit),
  defaultUser,
  defaultWallet,
  initReserve,
  toLendingToken,
 )
import Mlabs.Lending.Logic.Types qualified as Types
import PlutusTx.Ratio qualified as R
import System.Posix.Types qualified as Types

-- | Type for errors
type Error = BuiltinByteString

-- | State update of lending pool
type St = PlutusState LendingPool

----------------------------------------------------
-- common functions

{-# INLINEABLE isAsset #-}

-- | Check that lending pool supports given asset
isAsset :: Types.Coin -> St ()
isAsset asset = do
  reserves <- gets lp'reserves
  if M.member asset reserves
    then pure ()
    else throwError "Asset not supported"

{-# INLINEABLE updateReserveState #-}

{- | Updates all iterative parameters of reserve.
 Reserve state controls interest rates and health checks for all users.
-}
updateReserveState :: Integer -> Types.Coin -> St ()
updateReserveState currentTime asset =
  modifyReserve asset $ IR.updateReserveInterestRates currentTime

{-# INLINEABLE isTrustedOracle #-}

-- | check that user is allowed to do oracle actions
isTrustedOracle :: Types.UserId -> St ()
isTrustedOracle = checkRole "Is not trusted oracle" lp'trustedOracles

{-# INLINEABLE isAdmin #-}

-- | check that user is allowed to do admin actions
isAdmin :: Types.UserId -> St ()
isAdmin = checkRole "Is not admin" lp'admins

{-# INLINEABLE checkRole #-}
checkRole :: BuiltinByteString -> (LendingPool -> [Types.UserId]) -> Types.UserId -> St ()
checkRole msg extract uid = do
  users <- gets extract
  guardError msg $ elem uid users

{-# INLINEABLE aToken #-}
aToken :: Types.Coin -> St Types.Coin
aToken coin = do
  mCoin <- gets (`toLendingToken` coin)
  maybe err pure mCoin
  where
    err = throwError "Coin not supported"

{-# INLINEABLE getsWallet #-}

{- | Read field from the internal wallet for user and on asset.
 If there is no wallet empty wallet is allocated.
-}
getsWallet :: Types.UserId -> Types.Coin -> (Wallet -> a) -> St a
getsWallet uid coin f = fmap f $ getWallet uid coin

-- | Get internal wallet for user on given asset.
{-# INLINEABLE getWallet #-}
getWallet :: Types.UserId -> Types.Coin -> St Wallet
getWallet uid coin =
  getsUser uid (fromMaybe defaultWallet . M.lookup coin . user'wallets)

-- | Get all user internal wallets.
{-# INLINEABLE getsAllWallets #-}
getsAllWallets :: Types.UserId -> (Map Types.Coin Wallet -> a) -> St a
getsAllWallets uid f =
  f <$> getAllWallets uid

-- | Get all user internal wallets.
{-# INLINEABLE getAllWallets #-}
getAllWallets :: Types.UserId -> St (Map Types.Coin Wallet)
getAllWallets uid =
  getsUser uid user'wallets

{-# INLINEABLE getUsers #-}

-- | Get a list of all the users.
getUsers :: St [Types.UserId]
getUsers = M.keys <$> getAllUsers

{-# INLINEABLE getsUser #-}

-- | Get user info in the lending app by user id and apply extractor function to it.
getsUser :: Types.UserId -> (User -> a) -> St a
getsUser uid f = fmap f $ getUser uid

{-# INLINEABLE getUser #-}

-- | Get user info in the lending app by user id.
getUser :: Types.UserId -> St User
getUser uid = gets (fromMaybe defaultUser . M.lookup uid . lp'users)

{-# INLINEABLE getAllUsers #-}

-- | Get Map of all users.
getAllUsers :: St (Map Types.UserId User)
getAllUsers = gets lp'users

{-# INLINEABLE getsAllUsers #-}

-- | Gets all users given predicate.
getsAllUsers :: (User -> Bool) -> St (Map Types.UserId User)
getsAllUsers f = gets (M.filter f . lp'users)

{-# INLINEABLE getsReserve #-}

-- | Read reserve for a given asset and apply extractor function to it.
getsReserve :: Types.Coin -> (Reserve -> a) -> St a
getsReserve coin extract = fmap extract $ getReserve coin

{-# INLINEABLE getReserve #-}

-- | Read reserve for a given asset.
getReserve :: Types.Coin -> St Reserve
getReserve coin = do
  mReserve <- gets (M.lookup coin . lp'reserves)
  maybe err pure mReserve
  where
    err = throwError "Uknown coin"

{-# INLINEABLE toAda #-}

-- | Convert given currency to base currency
toAda :: Types.Coin -> Integer -> St Integer
toAda coin val = do
  ratio <- fmap (coinRate'value . reserve'rate) $ getReserve coin
  pure $ R.round $ R.fromInteger val N.* ratio

{-# INLINEABLE fromAda #-}

-- | Convert given currency from base currency
fromAda :: Types.Coin -> Integer -> St Integer
fromAda coin val = do
  ratio <- fmap (coinRate'value . reserve'rate) $ getReserve coin
  pure $ R.round $ R.fromInteger val N.* R.recip ratio

-- | Conversion between coins
data Convert = Convert
  { -- | convert from
    convert'from :: Types.Coin
  , -- | convert to
    convert'to :: Types.Coin
  }
  deriving (Hask.Show)

{-# INLINEABLE reverseConvert #-}
reverseConvert :: Convert -> Convert
reverseConvert Convert {..} =
  Convert
    { convert'from = convert'to
    , convert'to = convert'from
    }

{-# INLINEABLE convertCoin #-}

-- | Converts from  one currency to another
convertCoin :: Convert -> Integer -> St Integer
convertCoin Convert {..} amount =
  fromAda convert'to =<< toAda convert'from amount

{-# INLINEABLE weightedTotal #-}

-- | Weigted total of currencies in base currency
weightedTotal :: [(Types.Coin, Integer)] -> St Integer
weightedTotal = fmap sum . mapM (Hask.uncurry toAda)

{-# INLINEABLE walletTotal #-}

-- | Collects cumulative value for given wallet field
walletTotal :: (Wallet -> Integer) -> User -> St Integer
walletTotal extract (User ws _ _) = weightedTotal $ M.toList $ fmap extract ws

{-# INLINEABLE getTotalCollateral #-}

-- | Gets total collateral for a user.
getTotalCollateral :: User -> St Integer
getTotalCollateral = walletTotal wallet'collateral

{-# INLINEABLE getTotalBorrow #-}

-- | Gets total borrows for a user in base currency.
getTotalBorrow :: User -> St Integer
getTotalBorrow = walletTotal wallet'borrow

{-# INLINEABLE getTotalDeposit #-}

-- | Gets total deposit for a user in base currency.
getTotalDeposit :: User -> St Integer
getTotalDeposit = walletTotal wallet'deposit

{-# INLINEABLE getHealthCheck #-}

-- | Check if the user has enough health for the given asset.
getHealthCheck :: Integer -> Types.Coin -> User -> St Bool
getHealthCheck addToBorrow coin user =
  fmap (> R.fromInteger 1) $ getHealth addToBorrow coin user

{-# INLINEABLE getCurrentHealthCheck #-}

-- | Check if the user has currently enough health for the given asset.
getCurrentHealthCheck :: Types.Coin -> User -> St Bool
getCurrentHealthCheck = getHealthCheck 0

{-# INLINEABLE getHealth #-}

-- | Check borrowing health for the user by given currency
getHealth :: Integer -> Types.Coin -> User -> St Rational
getHealth addToBorrow coin user = do
  col <- getTotalCollateral user
  bor <- fmap (+ addToBorrow) $ getTotalBorrow user
  liq <- getLiquidationThreshold coin
  pure $ R.fromInteger col N.* liq N.* R.recip (R.fromInteger bor)

{-# INLINEABLE getCurrentHealth #-}

-- | Check immediate borrowing health for the user by given currency
getCurrentHealth :: Types.Coin -> User -> St Rational
getCurrentHealth = getHealth 0

{-# INLINEABLE getLiquidationThreshold #-}

-- | Reads liquidation threshold for a give asset.
getLiquidationThreshold :: Types.Coin -> St Rational
getLiquidationThreshold coin =
  gets (maybe (R.fromInteger 0) reserve'liquidationThreshold . M.lookup coin . lp'reserves)

{-# INLINEABLE getLiquidationBonus #-}

-- | Reads liquidation bonus for a give asset.
getLiquidationBonus :: Types.Coin -> St Rational
getLiquidationBonus coin =
  gets (maybe (R.fromInteger 0) reserve'liquidationBonus . M.lookup coin . lp'reserves)

{-# INLINEABLE modifyUsers #-}
modifyUsers :: (Map Types.UserId User -> Map Types.UserId User) -> St ()
modifyUsers f = modify' $ \lp -> lp {lp'users = f $ lp.lp'users}

{-# INLINEABLE modifyReserve #-}

-- | Modify reserve for a given asset.
modifyReserve :: Types.Coin -> (Reserve -> Reserve) -> St ()
modifyReserve coin f = modifyReserve' coin (Right . f)

{-# INLINEABLE modifyReserve' #-}

-- | Modify reserve for a given asset. It can throw errors.
modifyReserve' :: Types.Coin -> (Reserve -> Either Error Reserve) -> St ()
modifyReserve' asset f = do
  st <- get
  case M.lookup asset $ st.lp'reserves of
    Just reserve -> either throwError (\x -> put $ st {lp'reserves = M.insert asset x $ st.lp'reserves}) (f reserve)
    Nothing -> throwError "Asset is not supported"

{-# INLINEABLE modifyUser #-}

-- | Modify user info by id.
modifyUser :: Types.UserId -> (User -> User) -> St ()
modifyUser uid f = modifyUser' uid (Right . f)

{-# INLINEABLE modifyUser' #-}

-- | Modify user info by id. It can throw errors.
modifyUser' :: Types.UserId -> (User -> Either Error User) -> St ()
modifyUser' uid f = do
  st <- get
  case f $ fromMaybe defaultUser $ M.lookup uid $ lp'users st of
    Left msg -> throwError msg
    Right user -> put $ st {lp'users = M.insert uid user $ st.lp'users}

{-# INLINEABLE modifyHealthReport #-}
modifyHealthReport :: (Types.HealthReport -> Types.HealthReport) -> St ()
modifyHealthReport f = modify' $ \lp -> lp {lp'healthReport = f $ lp.lp'healthReport}

{-# INLINEABLE modifyWalletAndReserve #-}

-- | Modify user wallet and reserve wallet with the same function.
modifyWalletAndReserve :: Types.UserId -> Types.Coin -> (Wallet -> Wallet) -> St ()
modifyWalletAndReserve uid coin f = modifyWalletAndReserve' uid coin (Right . f)

{-# INLINEABLE modifyWalletAndReserve' #-}

-- | Applies the same modification function to the user and to the reserve wallet. It can throw errors.
modifyWalletAndReserve' :: Types.UserId -> Types.Coin -> (Wallet -> Either Error Wallet) -> St ()
modifyWalletAndReserve' uid coin f = do
  modifyWallet' uid coin f
  modifyReserveWallet' coin f

{-# INLINEABLE modifyReserveWallet #-}

-- | Modify reserve wallet for a given asset.
modifyReserveWallet :: Types.Coin -> (Wallet -> Wallet) -> St ()
modifyReserveWallet coin f = modifyReserveWallet' coin (Right . f)

{-# INLINEABLE modifyReserveWallet' #-}

-- | Modify reserve wallet for a given asset. It can throw errors.
modifyReserveWallet' :: Types.Coin -> (Wallet -> Either Error Wallet) -> St ()
modifyReserveWallet' coin f =
  modifyReserve' coin $ \r -> fmap (\w -> r {reserve'wallet = w}) $ f $ r.reserve'wallet

{-# INLINEABLE modifyWallet #-}

-- | Modify internal user wallet that is allocated for a given user id and asset.
modifyWallet :: Types.UserId -> Types.Coin -> (Wallet -> Wallet) -> St ()
modifyWallet uid coin f = modifyWallet' uid coin (Right . f)

{-# INLINEABLE modifyWallet' #-}

{- | Modify internal user wallet that is allocated for a given user id and asset.
 It can throw errors.
-}
modifyWallet' :: Types.UserId -> Types.Coin -> (Wallet -> Either Error Wallet) -> St ()
modifyWallet' uid coin f = modifyUser' uid $ \(User ws time health) -> do
  wal <- f $ fromMaybe defaultWallet $ M.lookup coin ws
  pure $ User (M.insert coin wal ws) time health

{-# INLINEABLE getNormalisedIncome #-}
getNormalisedIncome :: Types.Coin -> St Rational
getNormalisedIncome asset =
  getsReserve asset (ri'normalisedIncome . reserve'interest)

{-# INLINEABLE getCumulativeBalance #-}
getCumulativeBalance :: Types.UserId -> Types.Coin -> St Rational
getCumulativeBalance uid asset = do
  ni <- getNormalisedIncome asset
  getsWallet uid asset (IR.getCumulativeBalance ni)

{-# INLINEABLE getWalletCumulativeBalance #-}
getWalletCumulativeBalance :: Types.UserId -> St (Map Types.Coin Rational)
getWalletCumulativeBalance uid = do
  wallet <- getsAllWallets uid M.toList :: St [(Types.Coin, Wallet)]
  coins <- return $ fst <$> wallet :: St [Types.Coin]
  ni <- mapM getNormalisedIncome coins
  return . M.fromList $ zip coins ni
