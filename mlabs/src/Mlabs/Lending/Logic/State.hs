{-# OPTIONS_GHC -fno-specialize #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-specialize #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fobject-code #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | State transitions for Lending app
module Mlabs.Lending.Logic.State(
    St
  , Error
  , isAsset
  , aToken
  , isAdmin
  , isTrustedOracle
  , updateReserveState
  , initReserve
  , guardError
  , getWallet, getsWallet
  , getUser, getsUser
  , getReserve, getsReserve
  , toAda
  , fromAda
  , Convert(..)
  , reverseConvert
  , convertCoin
  , getTotalCollateral
  , getTotalBorrow
  , getTotalDeposit
  , getLiquidationThreshold
  , getLiquidationBonus
  , getHealth
  , getHealthCheck
  , modifyUsers
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
  , modifyHealthReport
  , getNormalisedIncome
  , getCumulativeBalance
) where

import PlutusTx.Prelude

import Control.Monad.Except (MonadError(throwError))
import Control.Monad.State.Strict (gets, MonadState(put, get), modify')
import PlutusTx.AssocMap (Map)
import qualified PlutusTx.AssocMap as M
import qualified PlutusTx.Numeric as N

import Mlabs.Control.Monad.State (guardError, PlutusState)
import Mlabs.Data.Ray (Ray)
import qualified Mlabs.Data.Ray as R
import qualified Mlabs.Lending.Logic.InterestRate as IR
import qualified Mlabs.Lending.Logic.Types as Types
import Mlabs.Lending.Logic.Types
    ( initReserve,
      CoinRate(coinRate'value),
      LendingPool(lp'trustedOracles, lp'admins, lp'reserves, lp'users,
                  lp'healthReport),
      User(User, user'wallets),
      Reserve(reserve'rate, reserve'liquidationThreshold,
              reserve'liquidationBonus, reserve'wallet, reserve'interest),
      Wallet(wallet'collateral, wallet'borrow, wallet'deposit),
      defaultUser,
      defaultWallet,
      toLendingToken,
      ReserveInterest(ri'normalisedIncome) )

-- | Type for errors
type Error = String

-- | State update of lending pool
type St = PlutusState LendingPool

----------------------------------------------------
-- common functions

{-# INLINABLE isAsset #-}
-- | Check that lending pool supports given asset
isAsset :: Types.Coin -> St ()
isAsset asset = do
  reserves <- gets lp'reserves
  if M.member asset reserves
    then pure ()
    else throwError "Asset not supported"

{-# INLINABLE updateReserveState #-}
-- | Updates all iterative parameters of reserve.
-- Reserve state controls interest rates and health checks for all users.
updateReserveState :: Integer -> Types.Coin -> St ()
updateReserveState currentTime asset =
  modifyReserve asset $ IR.updateReserveInterestRates currentTime

{-# INLINABLE isTrustedOracle #-}
-- | check that user is allowed to do oracle actions
isTrustedOracle :: Types.UserId -> St ()
isTrustedOracle = checkRole "Is not trusted oracle" lp'trustedOracles

{-# INLINABLE isAdmin #-}
-- | check that user is allowed to do admin actions
isAdmin :: Types.UserId -> St ()
isAdmin = checkRole "Is not admin" lp'admins

{-# INLINABLE checkRole #-}
checkRole :: String -> (LendingPool -> [Types.UserId]) -> Types.UserId -> St ()
checkRole msg extract uid = do
  users <- gets extract
  guardError msg $ elem uid users

{-# INLINABLE aToken #-}
aToken :: Types.Coin -> St Types.Coin
aToken coin = do
  mCoin <- gets (\st -> toLendingToken st coin)
  maybe err pure mCoin
  where
    err = throwError "Coin not supported"

{-# INLINABLE getsWallet #-}
-- | Read field from the internal wallet for user and on asset.
-- If there is no wallet empty wallet is allocated.
getsWallet :: Types.UserId -> Types.Coin -> (Wallet -> a) -> St a
getsWallet uid coin f = fmap f $ getWallet uid coin

-- | Get internal wallet for user on given asset.
{-# INLINABLE getWallet #-}
getWallet :: Types.UserId -> Types.Coin -> St Wallet
getWallet uid coin =
  getsUser uid (fromMaybe defaultWallet . M.lookup coin . user'wallets)

{-# INLINABLE getsUser #-}
-- | Get user info in the lending app by user id and apply extractor function to it.
getsUser :: Types.UserId -> (User -> a) -> St a
getsUser uid f = fmap f $ getUser uid

{-# INLINABLE getUser #-}
-- | Get user info in the lending app by user id.
getUser :: Types.UserId -> St User
getUser uid = gets (fromMaybe defaultUser . M.lookup uid . lp'users)

{-# INLINABLE getsReserve #-}
-- | Read reserve for a given asset and apply extractor function to it.
getsReserve :: Types.Coin -> (Reserve -> a) -> St a
getsReserve coin extract = fmap extract $ getReserve coin

{-# INLINABLE getReserve #-}
-- | Read reserve for a given asset.
getReserve :: Types.Coin -> St Reserve
getReserve coin = do
  mReserve <- gets (M.lookup coin . lp'reserves)
  maybe err pure mReserve
  where
    err = throwError "Uknown coin"

{-# INLINABLE toAda #-}
-- | Convert given currency to base currency
toAda :: Types.Coin -> Integer -> St Integer
toAda coin val = do
  ratio <- fmap (coinRate'value . reserve'rate) $ getReserve coin
  pure $ R.round $ R.fromInteger val N.* ratio

{-# INLINABLE fromAda #-}
-- | Convert given currency from base currency
fromAda :: Types.Coin -> Integer -> St Integer
fromAda coin val = do
  ratio <- fmap (coinRate'value . reserve'rate) $ getReserve coin
  pure $ R.round $ R.fromInteger val N.* R.recip ratio

-- | Conversion between coins
data Convert = Convert
  { convert'from :: Types.Coin   -- ^ convert from
  , convert'to   :: Types.Coin   -- ^ convert to
  }
  deriving (Show)

{-# INLINABLE reverseConvert #-}
reverseConvert :: Convert -> Convert
reverseConvert Convert{..} = Convert
  { convert'from = convert'to
  , convert'to   = convert'from
  }

{-# INLINABLE convertCoin #-}
-- | Converts from  one currency to another
convertCoin :: Convert -> Integer -> St Integer
convertCoin Convert{..} amount =
  fromAda convert'to =<< toAda convert'from amount

{-# INLINABLE weightedTotal #-}
-- | Weigted total of currencies in base currency
weightedTotal :: [(Types.Coin, Integer)] -> St Integer
weightedTotal = fmap sum . mapM (uncurry toAda)

{-# INLINABLE walletTotal #-}
-- | Collects cumulative value for given wallet field
walletTotal :: (Wallet -> Integer) -> User -> St Integer
walletTotal extract (User ws _ _) = weightedTotal $ M.toList $ fmap extract ws

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
getHealthCheck :: Integer -> Types.Coin -> User -> St Bool
getHealthCheck addToBorrow coin user =
  fmap (> R.fromInteger 1) $ getHealth addToBorrow coin user

{-# INLINABLE getHealth #-}
-- | Check borrowing health for the user by given currency
getHealth :: Integer -> Types.Coin -> User -> St Ray
getHealth addToBorrow coin user = do
  col <- getTotalCollateral user
  bor <- fmap (+ addToBorrow) $ getTotalBorrow user
  liq <- getLiquidationThreshold coin
  pure $ R.fromInteger col N.* liq N.* (R.recip $ R.fromInteger bor)

{-# INLINABLE getLiquidationThreshold #-}
-- | Reads liquidation threshold for a give asset.
getLiquidationThreshold :: Types.Coin -> St Ray
getLiquidationThreshold coin =
  gets (maybe (R.fromInteger 0) reserve'liquidationThreshold . M.lookup coin . lp'reserves)

{-# INLINABLE getLiquidationBonus #-}
-- | Reads liquidation bonus for a give asset.
getLiquidationBonus :: Types.Coin -> St Ray
getLiquidationBonus coin =
  gets (maybe (R.fromInteger 0) reserve'liquidationBonus . M.lookup coin . lp'reserves)

{-# INLINABLE modifyUsers #-}
modifyUsers :: (Map Types.UserId User -> Map Types.UserId User) -> St ()
modifyUsers f = modify' $ \lp -> lp { lp'users = f $ lp.lp'users }

{-# INLINABLE modifyReserve #-}
-- | Modify reserve for a given asset.
modifyReserve :: Types.Coin -> (Reserve -> Reserve) -> St ()
modifyReserve coin f = modifyReserve' coin (Right . f)

{-# INLINABLE modifyReserve' #-}
-- | Modify reserve for a given asset. It can throw errors.
modifyReserve' :: Types.Coin -> (Reserve -> Either Error Reserve) -> St ()
modifyReserve' asset f = do
  st <- get
  case M.lookup asset $ st.lp'reserves of
    Just reserve -> either throwError (\x -> put $ st { lp'reserves = M.insert asset x $ st.lp'reserves}) (f reserve)
    Nothing      -> throwError $ "Asset is not supported"

{-# INLINABLE modifyUser #-}
-- | Modify user info by id.
modifyUser :: Types.UserId -> (User -> User) -> St ()
modifyUser uid f = modifyUser' uid (Right . f)

{-# INLINABLE modifyUser' #-}
-- | Modify user info by id. It can throw errors.
modifyUser' :: Types.UserId -> (User -> Either Error User) -> St ()
modifyUser' uid f = do
  st <- get
  case f $ fromMaybe defaultUser $ M.lookup uid $ lp'users st of
    Left msg   -> throwError msg
    Right user -> put $ st { lp'users = M.insert uid user $ st.lp'users }

{-# INLINABLE modifyHealthReport #-}
modifyHealthReport :: (Types.HealthReport -> Types.HealthReport) -> St ()
modifyHealthReport f = modify' $ \lp -> lp { lp'healthReport = f $ lp.lp'healthReport }

{-# INLINABLE modifyWalletAndReserve #-}
-- | Modify user wallet and reserve wallet with the same function.
modifyWalletAndReserve :: Types.UserId -> Types.Coin -> (Wallet -> Wallet) -> St ()
modifyWalletAndReserve uid coin f = modifyWalletAndReserve' uid coin (Right . f)

{-# INLINABLE modifyWalletAndReserve' #-}
-- | Applies the same modification function to the user and to the reserve wallet. It can throw errors.
modifyWalletAndReserve' :: Types.UserId -> Types.Coin -> (Wallet -> Either Error Wallet) -> St ()
modifyWalletAndReserve' uid coin f = do
  modifyWallet' uid coin f
  modifyReserveWallet' coin f

{-# INLINABLE modifyReserveWallet #-}
-- | Modify reserve wallet for a given asset.
modifyReserveWallet :: Types.Coin -> (Wallet -> Wallet) -> St ()
modifyReserveWallet coin f = modifyReserveWallet' coin (Right . f)

{-# INLINABLE modifyReserveWallet' #-}
-- | Modify reserve wallet for a given asset. It can throw errors.
modifyReserveWallet' :: Types.Coin -> (Wallet -> Either Error Wallet) -> St ()
modifyReserveWallet' coin f =
  modifyReserve' coin $ \r -> fmap (\w -> r { reserve'wallet = w }) $ f $ r.reserve'wallet

{-# INLINABLE modifyWallet #-}
-- | Modify internal user wallet that is allocated for a given user id and asset.
modifyWallet :: Types.UserId -> Types.Coin -> (Wallet -> Wallet) -> St ()
modifyWallet uid coin f = modifyWallet' uid coin (Right . f)

{-# INLINABLE modifyWallet' #-}
-- | Modify internal user wallet that is allocated for a given user id and asset.
-- It can throw errors.
modifyWallet' :: Types.UserId -> Types.Coin -> (Wallet -> Either Error Wallet) -> St ()
modifyWallet' uid coin f = modifyUser' uid $ \(User ws time health) -> do
  wal <- f $ fromMaybe defaultWallet $ M.lookup coin ws
  pure $ User (M.insert coin wal ws) time health

{-# INLINABLE getNormalisedIncome #-}
getNormalisedIncome :: Types.Coin -> St Ray
getNormalisedIncome asset =
  getsReserve asset (ri'normalisedIncome . reserve'interest)

{-# INLINABLE getCumulativeBalance #-}
getCumulativeBalance :: Types.UserId -> Types.Coin -> St Ray
getCumulativeBalance uid asset = do
  ni <- getNormalisedIncome asset
  getsWallet uid asset (IR.getCumulativeBalance ni)

