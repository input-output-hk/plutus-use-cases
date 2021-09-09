{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-specialize #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fobject-code #-}

-- | State transitions for Aave-like application
module Mlabs.Lending.Logic.React (
  react,
  qReact,
) where

import PlutusTx.Prelude

import Control.Monad.Except (MonadError (throwError))
import Control.Monad.State.Strict (MonadState (get, put), gets)
import Data.Semigroup (Last (..))
import PlutusTx.AssocMap qualified as M
import PlutusTx.These (these)
import Prelude qualified as Hask

import Mlabs.Control.Check (isNonNegative, isPositive, isPositiveRational, isUnitRange)
import Mlabs.Data.List qualified as L
import Mlabs.Emulator.Blockchain (Resp (Burn, Mint), moveFromTo)
import Mlabs.Lending.Logic.InterestRate (addDeposit)
import Mlabs.Lending.Logic.State qualified as State
import Mlabs.Lending.Logic.Types (
  BadBorrow (BadBorrow, badBorrow'userId),
  CoinCfg (coinCfg'aToken, coinCfg'coin, coinCfg'interestModel, coinCfg'liquidationBonus, coinCfg'rate),
  CoinRate (CoinRate, coinRate'lastUpdateTime),
  InterestModel (im'optimalUtilisation, im'slope1, im'slope2),
  LendingPool (lp'coinMap, lp'healthReport, lp'reserves, lp'users),
  Reserve (reserve'rate, reserve'wallet),
  User (user'health, user'lastUpdateTime, user'wallets),
  UserAct (..),
  -- UserAct(act'rate, act'portion, act'useAsCollateral, act'asset,
  --         act'amount, act'receiveAToken, act'debtToCover, act'debt,
  --         act'collateral),
  UserId (Self),
  Wallet (wallet'borrow, wallet'collateral, wallet'deposit),
  adaCoin,
  initReserve,
 )
import Mlabs.Lending.Logic.Types qualified as Types
import PlutusTx.Ratio qualified as R

{-# INLINEABLE qReact #-}

-- | React to query actions by using the State Machine.
qReact :: Types.Act -> State.St (Maybe (Last Types.QueryRes))
qReact input = do
  case input of
    Types.QueryAct uid t act -> queryAct uid t act
    _ -> pure Nothing -- does nothing for any other type of input
  where
    queryAct uid time = \case
      Types.QueryCurrentBalanceAct () -> queryCurrentBalance uid time

    ---------------------------------------------------------------------------------------------------------
    -- Current Balance Query
    queryCurrentBalance :: Types.UserId -> Integer -> State.St (Maybe (Last Types.QueryRes))
    queryCurrentBalance uid cTime = do
      user <- State.getUser uid
      tDeposit <- State.getTotalDeposit user
      pure . Just . Last . Types.QueryResCurrentBalance $ Types.UserBalance uid tDeposit

{-# INLINEABLE react #-}

{- | State transitions for lending pool.
 For a given action we update internal state of Lending pool and produce
 list of responses to simulate change of the balances on blockchain.
-}
react :: Types.Act -> State.St [Resp]
react input = do
  checkInput input
  case input of
    Types.UserAct t uid act -> withHealthCheck t $ userAct t uid act
    Types.PriceAct t uid act -> withHealthCheck t $ priceAct t uid act
    Types.GovernAct uid act -> governAct uid act
    Types.QueryAct {} -> pure [] -- A query should produce no state modifying actions
  where
    userAct time uid = \case
      Types.DepositAct {..} -> depositAct time uid act'amount act'asset
      Types.BorrowAct {..} -> borrowAct time uid act'asset act'amount act'rate
      Types.RepayAct {..} -> repayAct time uid act'asset act'amount act'rate
      Types.SwapBorrowRateModelAct {..} -> swapBorrowRateModelAct uid act'asset act'rate
      Types.AddCollateralAct {..} -> addCollateral uid add'asset add'amount
      Types.RemoveCollateralAct {..} -> removeCollateral uid remove'asset remove'amount
      Types.WithdrawAct {..} -> withdrawAct time uid act'amount act'asset
      Types.FlashLoanAct -> flashLoanAct uid
      Types.LiquidationCallAct {..} -> liquidationCallAct time uid act'collateral act'debt act'debtToCover act'receiveAToken

    ---------------------------------------------------
    -- deposit

    depositAct currentTime uid amount asset = do
      ni <- State.getNormalisedIncome asset
      State.modifyWalletAndReserve' uid asset (addDeposit ni amount)
      aCoin <- State.aToken asset
      State.updateReserveState currentTime asset
      pure $
        mconcat
          [ [Mint aCoin amount]
          , moveFromTo Self uid aCoin amount
          , moveFromTo uid Self asset amount
          ]

    ---------------------------------------------------
    -- borrow

    -- TODO: ignores rate strategy (stable vs variable), ratio of liquidity to borrowed totals, health-check
    -- For borrowing to be valid we check that
    --  * reserve has enough liquidity
    --  * user does not use collateral reserve to borrow (it's meaningless for the user)
    --  * user has enough collateral for the borrow
    borrowAct currentTime uid asset amount _rate = do
      hasEnoughLiquidityToBorrow asset amount
      collateralNonBorrow uid asset
      hasEnoughCollateral uid asset amount
      updateOnBorrow
      State.updateReserveState currentTime asset
      pure $ moveFromTo Self uid asset amount
      where
        updateOnBorrow = do
          ni <- State.getNormalisedIncome asset
          State.modifyWallet uid asset $ \w -> w {wallet'borrow = wallet'borrow w + amount}
          State.modifyReserveWallet' asset $ addDeposit ni (negate amount)

    hasEnoughLiquidityToBorrow asset amount = do
      liquidity <- State.getsReserve asset (wallet'deposit . reserve'wallet)
      State.guardError "Not enough liquidity for asset" (liquidity >= amount)

    collateralNonBorrow uid asset = do
      col <- State.getsWallet uid asset wallet'collateral
      State.guardError
        "Collateral can not be used as borrow for user"
        (col == 0)

    hasEnoughCollateral uid asset amount = do
      bor <- State.toAda asset amount
      isOk <- State.getHealthCheck bor asset =<< State.getUser uid
      State.guardError msg isOk
      where
        msg = "Not enough collateral to borrow"

    ---------------------------------------------------
    -- repay (also called redeem in whitepaper)

    repayAct currentTime uid asset amount _rate = do
      ni <- State.getNormalisedIncome asset
      bor <- State.getsWallet uid asset wallet'borrow
      let newBor = bor - amount
      if newBor >= 0
        then State.modifyWallet uid asset $ \w -> w {wallet'borrow = newBor}
        else State.modifyWallet' uid asset $ \w -> do
          w1 <- addDeposit ni (negate newBor) w
          pure $ w1 {wallet'borrow = 0}
      State.modifyReserveWallet' asset $ addDeposit ni amount
      State.updateReserveState currentTime asset
      pure $ moveFromTo uid Self asset amount

    ---------------------------------------------------
    -- swap borrow model

    swapBorrowRateModelAct _ _ _ = todo

    ---------------------------------------------------
    -- todo docs
    -- set user reserve as collateral

    addCollateral uid asset desiredAmount
      | desiredAmount <= 0 =
        pure []
      | otherwise =
        do
          ni <- State.getNormalisedIncome asset
          amount <- calcAmountFor wallet'deposit uid asset desiredAmount
          State.modifyWallet' uid asset $ \w -> do
            w1 <- addDeposit ni (negate amount) w
            pure $ w1 {wallet'collateral = wallet'collateral w + amount}
          aCoin <- State.aToken asset
          pure $ moveFromTo uid Self aCoin amount

    removeCollateral uid asset desiredAmount
      | desiredAmount <= 0 =
        pure []
      | otherwise =
        do
          ni <- State.getNormalisedIncome asset
          amount <- calcAmountFor wallet'collateral uid asset desiredAmount
          State.modifyWalletAndReserve' uid asset $ \w -> do
            w1 <- addDeposit ni amount w
            pure $ w1 {wallet'collateral = wallet'collateral w - amount}
          aCoin <- State.aToken asset
          pure $ moveFromTo Self uid aCoin amount

    calcAmountFor extract uid asset desiredAmount = do
      availableAmount <- State.getsWallet uid asset extract
      pure $ min availableAmount desiredAmount

    ---------------------------------------------------
    -- withdraw

    withdrawAct currentTime uid amount asset = do
      -- validate withdraw
      hasEnoughDepositToWithdraw uid amount asset
      -- update state on withdraw
      ni <- State.getNormalisedIncome asset
      State.modifyWalletAndReserve' uid asset $ addDeposit ni (negate amount)
      aCoin <- State.aToken asset
      State.updateReserveState currentTime asset
      pure $
        mconcat
          [ moveFromTo Self uid asset amount
          , moveFromTo uid Self aCoin amount
          , Hask.pure $ Burn aCoin amount
          ]

    hasEnoughDepositToWithdraw uid amount asset = do
      dep <- State.getCumulativeBalance uid asset
      State.guardError "Not enough deposit to withdraw" (dep >= R.fromInteger amount)

    ---------------------------------------------------
    -- flash loan

    flashLoanAct _ = todo

    ---------------------------------------------------
    -- liquidation call

    liquidationCallAct currentTime uid collateralAsset debt amountCovered receiveATokens = do
      isBadBorrow debt
      wals <- State.getsUser (badBorrow'userId debt) user'wallets
      bor <- getDebtValue wals
      col <- getCollateralValue wals
      isPositive "liquidation collateral" col
      debtAmountIsLessThanHalf bor amountCovered
      colCovered <- min col <$> getCollateralCovered amountCovered
      adaBonus <- getBonus colCovered
      aCollateralAsset <- State.aToken collateralAsset
      updateBorrowUser colCovered
      pure $
        mconcat
          [ moveFromTo uid Self borrowAsset amountCovered
          , moveFromTo Self uid (receiveAsset aCollateralAsset) colCovered
          , moveFromTo Self uid adaCoin adaBonus
          ]
      where
        borrowAsset = debt.badBorrow'asset
        borrowUserId = debt.badBorrow'userId

        receiveAsset aCoin
          | receiveATokens = aCoin
          | otherwise = collateralAsset

        getDebtValue wals = case M.lookup borrowAsset wals of
          Just wal -> pure $ wallet'borrow wal
          Nothing -> throwError "Wallet does not have the debt to liquidate"

        getCollateralValue wals = case M.lookup collateralAsset wals of
          Just wal -> pure $ wallet'collateral wal
          Nothing -> throwError "Wallet does not have collateral for liquidation asset"

        debtToColateral =
          State.convertCoin
            State.Convert
              { convert'from = borrowAsset
              , convert'to = collateralAsset
              }

        getCollateralCovered amount = debtToColateral amount

        getBonus amount = do
          rate <- State.getLiquidationBonus collateralAsset
          State.toAda collateralAsset $ R.round $ R.fromInteger amount * rate

        debtAmountIsLessThanHalf userDebt amount
          | userDebt >= 2 * amount = pure ()
          | otherwise = throwError "Can not cover more than half of the borrow"

        -- we remove part of the borrow from the user and part of the collateral
        updateBorrowUser colCovered = do
          State.modifyWalletAndReserve borrowUserId collateralAsset $ \w ->
            w {wallet'collateral = wallet'collateral w - colCovered}
          State.modifyWalletAndReserve borrowUserId borrowAsset $ \w ->
            w {wallet'borrow = wallet'borrow w - amountCovered}
          updateSingleUserHealth currentTime borrowUserId

        isBadBorrow bor = do
          isOk <- M.member bor <$> gets lp'healthReport
          State.guardError "Bad borrow not present" isOk

    ---------------------------------------------------
    priceAct currentTime uid act = do
      State.isTrustedOracle uid
      case act of
        Types.SetAssetPriceAct coin rate -> setAssetPrice currentTime coin rate

    ---------------------------------------------------
    -- update on market price change

    setAssetPrice currentTime asset rate = do
      State.modifyReserve asset $ \r -> r {reserve'rate = CoinRate rate currentTime}
      pure []

    ---------------------------------------------------
    -- Govern acts

    governAct uid act = do
      State.isAdmin uid
      case act of
        Types.AddReserveAct cfg -> addReserve cfg

    ---------------------------------------------------
    -- Adds new reserve (new coin/asset)

    addReserve cfg@Types.CoinCfg {..} = do
      st <- get
      if M.member coinCfg'coin (st.lp'reserves)
        then throwError "Reserve is already present"
        else do
          let newReserves = M.insert coinCfg'coin (initReserve cfg) $ st.lp'reserves
              newCoinMap = M.insert coinCfg'aToken coinCfg'coin $ st.lp'coinMap
          put $ st {lp'reserves = newReserves, lp'coinMap = newCoinMap}
          return []

    ---------------------------------------------------
    -- health checks

    withHealthCheck time act = do
      res <- act
      updateHealthChecks time
      return res

    updateHealthChecks currentTime = do
      us <- getUsersForUpdate
      newUsers <- M.fromList <$> mapM (updateUserHealth currentTime) us
      State.modifyUsers $ \users -> batchInsert newUsers users
      where
        getUsersForUpdate = do
          us <- fmap setTimestamp . M.toList <$> gets lp'users
          pure $ fmap snd $ L.take userUpdateSpan $ L.sortOn fst us

        setTimestamp (uid, user) = (user.user'lastUpdateTime - currentTime, (uid, user))

    updateSingleUserHealth currentTime uid = do
      user <- State.getUser uid
      newUser <- snd <$> updateUserHealth currentTime (uid, user)
      State.modifyUser uid $ const newUser

    updateUserHealth currentTime (uid, user) = do
      health <- mapM (\asset -> (asset,) <$> State.getHealth 0 asset user) userBorrows
      L.mapM_ (reportUserHealth uid) health
      pure
        ( uid
        , user
            { user'lastUpdateTime = currentTime
            , user'health = M.fromList health
            }
        )
      where
        userBorrows = M.keys $ M.filter ((> 0) . wallet'borrow) $ user.user'wallets

    reportUserHealth uid (asset, health)
      | health >= R.fromInteger 1 = State.modifyHealthReport $ M.delete (BadBorrow uid asset)
      | otherwise = State.modifyHealthReport $ M.insert (BadBorrow uid asset) health

    -- insert m1 to m2
    batchInsert m1 m2 = fmap (these id id const) $ M.union m1 m2

    -- how many users to update per iteration of update health checks
    userUpdateSpan = 10

    todo = return []

{-# INLINEABLE checkInput #-}

-- | Check if input is valid
checkInput :: Types.Act -> State.St ()
checkInput = \case
  Types.UserAct time _uid act -> do
    isNonNegative "timestamp" time
    checkUserAct act
  Types.PriceAct time _uid act -> checkPriceAct time act
  Types.GovernAct _uid act -> checkGovernAct act
  Types.QueryAct {} -> pure () -- TODO think of input checks for query
  where
    checkUserAct = \case
      Types.DepositAct amount asset -> do
        isPositive "deposit" amount
        State.isAsset asset
      Types.BorrowAct amount asset _rate -> do
        isPositive "borrow" amount
        State.isAsset asset
      Types.RepayAct amount asset _rate -> do
        isPositive "repay" amount
        State.isAsset asset
      Types.SwapBorrowRateModelAct asset _rate -> State.isAsset asset
      Types.AddCollateralAct asset amount -> do
        State.isAsset asset
        isPositive "add collateral" amount
      Types.RemoveCollateralAct asset amount -> do
        State.isAsset asset
        isPositive "remove collateral" amount
      Types.WithdrawAct asset amount -> do
        isPositive "withdraw" amount
        State.isAsset asset
      Types.FlashLoanAct -> pure ()
      Types.LiquidationCallAct collateral _debt debtToCover _recieveAToken -> do
        State.isAsset collateral
        isPositive "Debt to cover" debtToCover

    checkPriceAct time act = do
      isNonNegative "price rate timestamp" time
      case act of
        Types.SetAssetPriceAct asset price -> do
          checkCoinRateTimeProgress time asset
          isPositiveRational "price" price
          State.isAsset asset

    checkGovernAct = \case
      Types.AddReserveAct cfg -> checkCoinCfg cfg

    checkCoinCfg Types.CoinCfg {..} = do
      isPositiveRational "coin price config" coinCfg'rate
      checkInterestModel coinCfg'interestModel
      isUnitRange "liquidation bonus config" coinCfg'liquidationBonus

    checkInterestModel Types.InterestModel {..} = do
      isUnitRange "optimal utilisation" im'optimalUtilisation
      isPositiveRational "slope 1" im'slope1
      isPositiveRational "slope 2" im'slope2

    checkCoinRateTimeProgress time asset = do
      lastUpdateTime <- coinRate'lastUpdateTime . reserve'rate <$> State.getReserve asset
      isNonNegative "Timestamps for price update should grow" (time - lastUpdateTime)
