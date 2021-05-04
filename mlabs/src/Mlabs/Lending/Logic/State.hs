-- | State transitions for Lending app
module Mlabs.Lending.Logic.State(
    react
  , Error
  , Move(..)
  , Resp(..)
  , Wallet(..)
  , applyResp
  , BchState(..)
  , initReserve
) where

import Prelude

import Control.Monad.Except
import Control.Monad.State.Strict

import Data.Maybe
import Data.Text
import Mlabs.Lending.Logic.Types

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Text as T

showt :: Show a => a -> Text
showt = T.pack . show

type Error = Text

-- | State update of lending pool
type St = StateT LendingPool (Either Error)

-- | State transition for lending pool.
-- For a given action we update internal state of Lending pool and produce
-- list of responses to simulate change of the balances
react :: Act -> St [Resp]
react = \case
  UserAct uid act -> userAct uid act
  PriceAct    act -> priceAct act
  GovernAct   act -> governAct act
  where
    userAct uid = \case
      DepositAct{..}                    -> depositAct uid act'amount act'asset
      BorrowAct{..}                     -> borrowAct  uid act'asset act'amount act'rate
      RepayAct{..}                      -> repayAct   uid act'asset act'amount act'rate
      SwapBorrowRateModelAct{..}        -> swapBorrowRateModelAct uid act'asset act'rate
      SetUserReserveAsCollateralAct{..} -> setUserReserveAsCollateralAct uid act'asset act'useAsCollateral
      WithdrawAct{..}                   -> withdrawAct uid act'amount act'asset
      FlashLoanAct                      -> flashLoanAct uid
      LiquidationCallAct{..}            -> liquidationCallAct uid act'collateral act'debt act'user act'debtToCover act'receiveAToken

    -- TODO: ignores ratio of liquidity to borrowed totals
    depositAct uid amount asset = do
      modifyReserve asset    (Right . depositReserve)
      modifyWallet uid asset (Right . depositUser)
      let move a b = MoveTo $ Move uid a b
      pure
        [ move asset (negate amount)
        , move (aToken asset) amount
        ]
      where
        depositReserve r@Reserve{..} = r { reserve'deposit = amount + reserve'deposit }
        depositUser    w@Wallet{..}  = w { wallet'deposit  = amount + wallet'deposit }

    -- TODO: ignores rate strategy (stable vs variable), ratio of liquidity to borrowed totals, health-check
    -- For borrowing to be valid we check that
    --  * reserve has enough liquidity
    --  * user does not use collateral reserve to borrow (it's meaningless for the user)
    --  * user has enough collateral for the borrow
    borrowAct uid asset amount _rate = do
      hasEnoughLiquidity asset amount
      collateralNonBorrow uid asset
      hasEnoughCollateral uid asset amount
      updateReserveOnBorrow
      updateUserOnBorrow
      pure [ MoveTo $ Move uid asset amount  ]
      where
        updateReserveOnBorrow = modifyReserve asset $ \r -> Right $ r
          { reserve'deposit = reserve'deposit r - amount
          , reserve'borrow  = reserve'borrow  r + amount
          }

        updateUserOnBorrow = modifyWallet uid asset $ \w -> Right $ w
          { wallet'deposit = wallet'deposit w - amount
          , wallet'borrow  = wallet'borrow  w + amount
          }

    hasEnoughLiquidity asset amount = do
      liquidity <- getsReserve asset reserve'deposit
      guardError ("Not enough liquidity for asset " <> showt asset)
        (liquidity >= amount)

    collateralNonBorrow uid asset = do
      col <- getsWallet uid asset wallet'collateral
      guardError ("Collateral can not be used as borrow for user " <> showt uid <> " for asset " <> showt asset)
        (col == 0)

    hasEnoughCollateral uid asset amount = do
      bor <- toAda asset amount
      isOk <- getHealthCheck bor asset =<< getUser uid
      guardError msg isOk
      where
        msg = mconcat [ "Not enough collateral to borrow ", showt amount, " ", showt asset, " for user ", showt uid]


    repayAct _ _ _ _ = todo
    swapBorrowRateModelAct _ _ _ = todo
    setUserReserveAsCollateralAct _ _ _ = todo
    withdrawAct _ _ _ = todo
    flashLoanAct _ = todo
    liquidationCallAct _ _ _ _ _ _ = todo

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

    modifyWallet :: UserId -> Coin -> (Wallet -> Either Text Wallet) -> St ()
    modifyWallet uid coin f = modifyUser uid $ \(User ws) -> do
      wal <- f $ fromMaybe defaultWallet $ M.lookup coin ws
      pure $ User $ M.insert coin wal ws

    priceAct = \case
      SetAssetPrice coin rate -> setAssetPrice coin rate
      SetOracleAddr coin addr -> setOracleAddr coin addr

    setAssetPrice _ _ = todo
    setOracleAddr _ _ = todo

    governAct = \case
      AddReserve coin val -> addReserve coin val

    addReserve coin val = do
      LendingPool reserves users <- get
      if M.member coin reserves
        then throwError "Reserve is already present"
        else do
          put $ LendingPool (M.insert coin (initReserve val) reserves) users
          return []

    todo = return []

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

{-
-- | Gets total deposit for a user in base currency.
getTotalDeposit :: User -> St Integer
getTotalDeposit = walletTotal wallet'deposit
-}

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

----------------------------------------------------
-- simple emulation ob blockchain state

-- | Blockchain state is a set of wallets
newtype BchState = BchState (Map UserId BchWallet)

-- " For simplicity wallet is a map of coins to balances.
newtype BchWallet = BchWallet (Map Coin Integer)

-- | We can give money to vallets and take it from them
data Resp
  = MoveTo Move

-- | Moving funds
data Move = Move
  { move'addr   :: UserId    -- where move happens
  , move'coin   :: Coin      -- on which value
  , move'amount :: Integer   -- how many to add (can be negative)
  }

-- | Applies reponse to the blockchain state.
applyResp :: Resp -> BchState -> BchState
applyResp resp (BchState wallets) = BchState $ case resp of
  MoveTo act -> moveTo act wallets
  where
    moveTo Move{..} m = updateWallet move'addr move'coin move'amount m

    updateWallet addr coin amt m = M.update (Just . updateBalance coin amt) addr m
    updateBalance coin amt (BchWallet bals) = BchWallet $ M.update (\x -> Just (x + amt)) coin bals

