{-# OPTIONS_GHC -fno-specialize #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-specialize #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fobject-code #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
-- | State transitions for Lending app
module Mlabs.Lending.Logic.State(
    St
  , showt
  , Error
  , isAsset
  , aToken
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

import qualified PlutusTx.Ratio as R
import qualified PlutusTx.Numeric as N
import PlutusTx.Prelude
import PlutusTx.AssocMap (Map)
import qualified PlutusTx.AssocMap as M

import Control.Monad.Except       hiding (Functor(..), mapM)
import Control.Monad.State.Strict hiding (Functor(..), mapM)

import qualified Mlabs.Lending.Logic.InterestRate as IR
import Mlabs.Lending.Logic.Types

import Mlabs.Control.Monad.State

-- | Type for errors
type Error = String

-- | State update of lending pool
type St = PlutusState LendingPool

----------------------------------------------------
-- common functions

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

{-# INLINABLE isTrustedOracle #-}
isTrustedOracle :: UserId -> St ()
isTrustedOracle uid = do
  oracles <- gets lp'trustedOracles
  guardError "Is not trusted oracle" $ elem uid oracles

{-# INLINABLE aToken #-}
aToken :: Coin -> St Coin
aToken coin = do
  mCoin <- gets (\st -> toLendingToken st coin)
  maybe err pure mCoin
  where
    err = throwError "Coin not supported"

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
  ratio <- fmap (coinRate'value . reserve'rate) $ getReserve coin
  pure $ R.round $ R.fromInteger val N.* ratio

{-# INLINABLE fromAda #-}
-- | Convert given currency from base currency
fromAda :: Coin -> Integer -> St Integer
fromAda coin val = do
  ratio <- fmap (coinRate'value . reserve'rate) $ getReserve coin
  pure $ R.round $ R.fromInteger val N.* R.recip ratio

-- | Conversion between coins
data Convert = Convert
  { convert'from :: Coin   -- ^ convert from
  , convert'to   :: Coin   -- ^ convert to
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
weightedTotal :: [(Coin, Integer)] -> St Integer
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

{-# INLINABLE getLiquidationBonus #-}
-- | Reads liquidation bonus for a give asset.
getLiquidationBonus :: Coin -> St Rational
getLiquidationBonus coin =
  gets (maybe (R.fromInteger 0) reserve'liquidationBonus . M.lookup coin . lp'reserves)

{-# INLINABLE modifyUsers #-}
modifyUsers :: (Map UserId User -> Map UserId User) -> St ()
modifyUsers f = modify' $ \lp -> lp { lp'users = f $ lp'users lp }

{-# INLINABLE modifyReserve #-}
-- | Modify reserve for a given asset.
modifyReserve :: Coin -> (Reserve -> Reserve) -> St ()
modifyReserve coin f = modifyReserve' coin (Right . f)

{-# INLINABLE modifyReserve' #-}
-- | Modify reserve for a given asset. It can throw errors.
modifyReserve' :: Coin -> (Reserve -> Either Error Reserve) -> St ()
modifyReserve' asset f = do
  LendingPool lp users curSym coinMap healthReport oracles <- get
  case M.lookup asset lp of
    Just reserve -> either throwError (\x -> put $ LendingPool (M.insert asset x lp) users curSym coinMap healthReport oracles) (f reserve)
    Nothing      -> throwError $ "Asset is not supported"

{-# INLINABLE modifyUser #-}
-- | Modify user info by id.
modifyUser :: UserId -> (User -> User) -> St ()
modifyUser uid f = modifyUser' uid (Right . f)

{-# INLINABLE modifyUser' #-}
-- | Modify user info by id. It can throw errors.
modifyUser' :: UserId -> (User -> Either Error User) -> St ()
modifyUser' uid f = do
  LendingPool lp users curSym coinMap healthReport oracles <- get
  case f $ fromMaybe defaultUser $ M.lookup uid users of
    Left msg   -> throwError msg
    Right user -> put $ LendingPool lp (M.insert uid user users) curSym coinMap healthReport oracles

{-# INLINABLE modifyHealthReport #-}
modifyHealthReport :: (HealthReport -> HealthReport) -> St ()
modifyHealthReport f = modify' $ \lp -> lp { lp'healthReport = f $ lp'healthReport lp }

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
modifyWallet' uid coin f = modifyUser' uid $ \(User ws time health) -> do
  wal <- f $ fromMaybe defaultWallet $ M.lookup coin ws
  pure $ User (M.insert coin wal ws) time health

{-# INLINABLE getNormalisedIncome #-}
getNormalisedIncome :: Coin -> St Rational
getNormalisedIncome asset =
  getsReserve asset $ (ri'normalisedIncome . reserve'interest)

{-# INLINABLE getCumulativeBalance #-}
getCumulativeBalance :: UserId -> Coin -> St Rational
getCumulativeBalance uid asset = do
  ni <- getNormalisedIncome asset
  getsWallet uid asset (IR.getCumulativeBalance ni)

