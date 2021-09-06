{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module Plutus.Contracts.LendingPool.OffChain.User where

import qualified Control.Lens                                 as Lens
import           Control.Monad                                hiding (fmap)
import qualified Data.ByteString                              as BS
import qualified Data.Map                                     as Map
import           Data.Monoid                                  (Last (..))
import           Data.Proxy                                   (Proxy (..))
import           Data.Text                                    (Text, pack)
import qualified Data.Text                                    as Text
import           Data.Void                                    (Void)
import           Ext.Plutus.Ledger.Value                      (utxoValue)
import           Ledger                                       hiding (singleton)
import           Ledger.Constraints                           as Constraints
import           Ledger.Constraints.OnChain                   as Constraints
import           Ledger.Constraints.TxConstraints             as Constraints
import qualified Ledger.Scripts                               as Scripts
import qualified Ledger.Typed.Scripts                         as Scripts
import           Playground.Contract
import           Plutus.Abstract.ContractResponse             (ContractResponse,
                                                               withContractResponse)
import           Plutus.Abstract.IncentivizedAmount           (IncentivizedAmount (..),
                                                               _iaAmount,
                                                               accrue)
import           Plutus.Abstract.OutputValue                  (OutputValue (..))
import qualified Plutus.Abstract.TxUtils                      as TxUtils
import           Plutus.Contract                              hiding (when)
import           Plutus.Contracts.Currency                    as Currency
import qualified Plutus.Contracts.LendingPool.InterestRate    as InterestRate
import qualified Plutus.Contracts.LendingPool.OffChain.AToken as AToken
import qualified Plutus.Contracts.LendingPool.OffChain.State  as State
import           Plutus.Contracts.LendingPool.OnChain.Core    (Aave,
                                                               AaveDatum (..),
                                                               AaveNewState (..),
                                                               AaveRedeemer (..),
                                                               Reserve (..),
                                                               UserConfig (..),
                                                               _ucCollateralizedInvestment,
                                                               _ucDebt)
import qualified Plutus.Contracts.LendingPool.OnChain.Core    as Core
import           Plutus.Contracts.LendingPool.Shared          (UpdateConfigParams (..),
                                                               getAverageStableBorrowRate,
                                                               updateConfigAmounts,
                                                               updateReserveOnBorrow,
                                                               updateReserveOnLiquidityChange,
                                                               updateReserveOnRepay)
import qualified Plutus.Contracts.Service.FungibleToken       as FungibleToken
import qualified Plutus.Contracts.Service.Oracle              as Oracle
import           Plutus.V1.Ledger.Ada                         (adaValueOf,
                                                               lovelaceValueOf)
import qualified Plutus.V1.Ledger.Address                     as Addr
import qualified Plutus.V1.Ledger.Interval                    as Interval
import           Plutus.V1.Ledger.Value                       as Value
import qualified PlutusTx
import qualified PlutusTx.AssocMap                            as AssocMap
import           PlutusTx.Prelude                             hiding
                                                              (Monoid (..),
                                                               Semigroup (..),
                                                               mconcat, unless)
import           Prelude                                      (Monoid (..),
                                                               Semigroup (..),
                                                               show, subtract)
import qualified Prelude
import           Text.Printf                                  (printf)

data DepositParams =
  DepositParams {
    dpAsset      :: AssetClass,
    dpOnBehalfOf :: PubKeyHash,
    dpAmount     :: Integer
  }
    deriving stock    (Prelude.Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON, ToSchema)

PlutusTx.unstableMakeIsData ''DepositParams
PlutusTx.makeLift ''DepositParams

-- | The user puts N amount of his asset into a corresponding reserve, in exchange he gets N equivalent aTokens
deposit :: Aave -> DepositParams -> Contract w s Text ()
deposit aave DepositParams {..} = do
    reserve <- State.findAaveReserve aave dpAsset
    forgeTx <- AToken.forgeATokensFrom aave reserve dpOnBehalfOf dpAmount

    let userConfigId = (rCurrency reserve, dpOnBehalfOf)
    (stateTx, _) <- State.modifyAaveState aave (Core.DepositRedeemer userConfigId) $
        \oldState@AaveNewState{..} ->
            case AssocMap.lookup userConfigId ansUserConfigs of
                Nothing -> do
                    slot <- currentSlot
                    State.addUserConfig
                        userConfigId
                        UserConfig {
                            ucDebt = IncentivizedAmount slot (rCurrentStableBorrowRate reserve) (fromInteger 0),
                            ucCollateralizedInvestment = IncentivizedAmount slot (fromInteger 1) (fromInteger 0)
                        }
                        oldState
                Just userConfig -> pure oldState
            >>= State.updateReserveNew dpAsset reserve { rAmount = rAmount reserve + dpAmount }

    ledgerTx <- TxUtils.submitTxPair $ forgeTx <> stateTx
    _ <- awaitTxConfirmed $ txId ledgerTx
    pure ()

data WithdrawParams =
    WithdrawParams {
        wpAsset  :: AssetClass,
        wpUser   :: PubKeyHash,
        wpAmount :: Integer
    }
    deriving stock    (Prelude.Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON, ToSchema)

PlutusTx.unstableMakeIsData ''WithdrawParams
PlutusTx.makeLift ''WithdrawParams

-- | The user withdraws N amount of a specific asset from the corresponding reserve, N aTokens are taken from his wallet and burned
withdraw :: Aave -> WithdrawParams -> Contract w s Text ()
withdraw aave WithdrawParams {..} = do
    reserve <- State.findAaveReserve aave wpAsset
    let userConfigId = (wpAsset, wpUser)

    burnTx <- AToken.burnATokensFrom aave reserve wpUser wpAmount

    (stateTx, _) <- State.modifyAaveState aave (Core.WithdrawRedeemer userConfigId) $
        State.updateReserveNew wpAsset (reserve { rAmount = rAmount reserve - wpAmount })

    ledgerTx <- TxUtils.submitTxPair $ burnTx <> stateTx
    _ <- awaitTxConfirmed $ txId ledgerTx
    pure ()

data BorrowParams =
    BorrowParams {
        bpAsset      :: AssetClass,
        bpAmount     :: Integer,
        bpOnBehalfOf :: PubKeyHash
    }
    deriving stock    (Prelude.Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON, ToSchema)

PlutusTx.unstableMakeIsData ''BorrowParams
PlutusTx.makeLift ''BorrowParams

-- | The user borrows N amount of a needed asset from the corresponding reserve, his debt entry state is increased by N
borrow :: Aave -> BorrowParams -> Contract w s Text ()
borrow aave BorrowParams {..} = do
    oldStateOutput <- State.findAaveState aave
    let oldState@AaveNewState {..} = ovValue oldStateOutput
    reserve <- maybe (throwError "Reserve not found") pure $ AssocMap.lookup bpAsset ansReserves
    let userConfigId = (rCurrency reserve, bpOnBehalfOf)
    slot <- currentSlot

    availableLiquidity <- flip assetClassValueOf bpAsset <$> State.getAaveCollateralValue aave
    let reserveConfigs = fmap snd . filter (\((asset, _), _) -> asset == bpAsset) . AssocMap.toList $ ansUserConfigs
    let updatedReserve = updateReserveOnBorrow reserveConfigs availableLiquidity bpAmount slot reserve

    userConfigs <- do
        case AssocMap.lookup userConfigId ansUserConfigs of
            Nothing -> do
                pure $ AssocMap.insert
                    userConfigId
                    UserConfig {
                        ucDebt = IncentivizedAmount slot (rCurrentStableBorrowRate reserve) (fromInteger bpAmount),
                        ucCollateralizedInvestment = IncentivizedAmount slot (fromInteger 0) (fromInteger 0)
                    }
                    ansUserConfigs
            Just userConfig -> do
                pure $ AssocMap.insert
                    userConfigId
                    (
                        Lens.over (_ucDebt . _iaAmount) (+ fromInteger bpAmount)
                        . updateConfigAmounts
                            UpdateConfigParams {
                                ucpUpdatedReserve = updatedReserve,
                                ucpPreviousReserveUpdated = rLastUpdated reserve,
                                ucpCurrentSlot = slot
                            }
                        $ userConfig
                    )
                    ansUserConfigs
    oracles <- either throwError pure $ findOraclesForUser bpOnBehalfOf ansReserves userConfigs
    let redeemer = Core.BorrowRedeemer bpAmount userConfigId oracles slot

    utxos <-
        Map.filter ((> 0) . flip assetClassValueOf bpAsset . txOutValue . txOutTxOut)
        <$> utxoAt (Core.aaveAddress aave)
    let inputs = (\(ref, tx) -> OutputValue ref tx redeemer) <$> Map.toList utxos
    let payment = assetClassValue (rCurrency reserve) bpAmount
    let remainder = assetClassValue (rCurrency reserve) (rAmount reserve - bpAmount)
    let disbursementTx =  TxUtils.mustSpendFromScript (Core.aaveInstance aave) inputs bpOnBehalfOf payment <>
                            TxUtils.mustPayToScript (Core.aaveInstance aave) bpOnBehalfOf Core.ReserveFundsDatum remainder

    newState <- State.updateReserveNew bpAsset updatedReserve oldState { ansUserConfigs = userConfigs }
    stateTx <- (<> (mempty, mustValidateIn (Interval.from slot))) . fst <$> do
        State.updateAaveState aave redeemer (newState Prelude.<$ oldStateOutput)

    oraclesTx <- mconcat <$> forM oracles Oracle.useOracle

    ledgerTx <- TxUtils.submitTxPair $ disbursementTx <> stateTx <> oraclesTx
    _ <- awaitTxConfirmed $ txId ledgerTx
    pure ()

findOraclesForUser
        :: PubKeyHash
           -> AssocMap.Map AssetClass Reserve
           -> AssocMap.Map (AssetClass, PubKeyHash) UserConfig
           -> Either Text [(CurrencySymbol, PubKeyHash, Integer, AssetClass)]
findOraclesForUser actor reserves userConfigs =
  foldrM findOracle [] $ AssocMap.keys userConfigs
  where
    findOracle (asset, user) oracles
      | user == actor =
        maybe (Left "findOraclesForUser: User reserve not found")
        (\reserve -> Right $ rTrustedOracle reserve : oracles) $
        AssocMap.lookup asset reserves
      | otherwise = Right oracles

data RepayParams =
    RepayParams {
        rpAsset      :: AssetClass,
        rpAmount     :: Integer,
        rpOnBehalfOf :: PubKeyHash
    }
    deriving stock    (Prelude.Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON, ToSchema)

PlutusTx.unstableMakeIsData ''RepayParams
PlutusTx.makeLift ''RepayParams

-- | The user repays N amount of a specific asset to the corresponding reserve, his debt entry state is decreased by N
repay :: Aave -> RepayParams -> Contract w s Text ()
repay aave RepayParams {..} = do
    reserve <- State.findAaveReserve aave rpAsset

    let payment = assetClassValue (rCurrency reserve) rpAmount
    let reimbursementTx = TxUtils.mustPayToScript (Core.aaveInstance aave) rpOnBehalfOf Core.ReserveFundsDatum payment
    let userConfigId = (rCurrency reserve, rpOnBehalfOf)

    slot <- currentSlot
    stateTx <- (<> (mempty, mustValidateIn (Interval.from slot))) . fst <$> (
        State.modifyAaveState aave (Core.RepayRedeemer rpAmount userConfigId slot) $
            \oldState@AaveNewState{..} -> do
                availableLiquidity <- flip assetClassValueOf rpAsset <$> State.getAaveCollateralValue aave
                let reserveConfigs = fmap snd . filter (\((asset, _), _) -> asset == rpAsset) . AssocMap.toList $ ansUserConfigs
                let updatedReserve = updateReserveOnRepay reserveConfigs availableLiquidity rpAmount slot reserve

                s <- case AssocMap.lookup userConfigId ansUserConfigs of
                    Nothing ->
                        throwError "User does not have any debt."
                    Just userConfig -> do
                        State.updateUserConfig
                            userConfigId
                            (
                                Lens.over (_ucDebt . _iaAmount) (\e -> e - fromInteger rpAmount)
                                . updateConfigAmounts
                                    UpdateConfigParams {
                                        ucpUpdatedReserve = updatedReserve,
                                        ucpPreviousReserveUpdated = rLastUpdated reserve,
                                        ucpCurrentSlot = slot
                                    }
                                $ userConfig
                            )
                            oldState
                State.updateReserveNew rpAsset (updatedReserve { rAmount = rAmount reserve + rpAmount }) s
        )

    ledgerTx <- TxUtils.submitTxPair $ reimbursementTx <> stateTx
    _ <- awaitTxConfirmed $ txId ledgerTx
    pure ()

data ProvideCollateralParams =
    ProvideCollateralParams {
        pcpUnderlyingAsset :: AssetClass,
        pcpAmount          :: Integer,
        pcpOnBehalfOf      :: PubKeyHash
    }
    deriving stock    (Prelude.Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON, ToSchema)

PlutusTx.unstableMakeIsData ''ProvideCollateralParams
PlutusTx.makeLift ''ProvideCollateralParams

-- | Gets all UTxOs belonging to a user and concats them into one Value
fundsAt :: PubKeyHash -> Contract w s Text Value
fundsAt pkh = utxoValue <$> utxoAt (pubKeyHashAddress pkh)

balanceAt :: PubKeyHash -> AssetClass -> Contract w s Text Integer
balanceAt pkh asset = flip assetClassValueOf asset <$> fundsAt pkh

-- | User deposits N amount of aToken as collateral, his investment entry state is increased by N
provideCollateral :: Aave -> ProvideCollateralParams -> Contract w s Text ()
provideCollateral aave ProvideCollateralParams {..} = do
    reserve <- State.findAaveReserve aave pcpUnderlyingAsset
    let aTokenAsset = rAToken reserve
    userOwnedAtokenAmount <- balanceAt pcpOnBehalfOf aTokenAsset
    let payment = assetClassValue aTokenAsset pcpAmount
    let remainder = assetClassValue aTokenAsset (userOwnedAtokenAmount - pcpAmount)
    let fundsLockingTx = TxUtils.mustPayToScript (Core.aaveInstance aave) pcpOnBehalfOf (Core.UserCollateralFundsDatum pcpOnBehalfOf aTokenAsset) payment
                         <> (Prelude.mempty, mustPayToPubKey pcpOnBehalfOf remainder)
    let userConfigId = (rCurrency reserve, pcpOnBehalfOf)

    slot <- currentSlot
    stateTx <- (<> (mempty, mustValidateIn (Interval.from slot))) . fst <$>
        (State.modifyAaveState aave (Core.ProvideCollateralRedeemer userConfigId slot) $
            \oldState@AaveNewState{..} -> do
                availableLiquidity <- flip assetClassValueOf pcpUnderlyingAsset <$> State.getAaveCollateralValue aave
                let reserveConfigs = fmap snd . filter (\((asset, _), _) -> asset == pcpUnderlyingAsset) . AssocMap.toList $ ansUserConfigs
                let updatedReserve = updateReserveOnLiquidityChange reserveConfigs (availableLiquidity + pcpAmount) slot reserve
                logInfo @Prelude.String $
                    " total available - "
                    <> show availableLiquidity
                    <> " liq rate - " <> show (rLiquidityRate updatedReserve)
                    <> " average borrow rate - " <> show (getAverageStableBorrowRate reserveConfigs)

                case AssocMap.lookup userConfigId ansUserConfigs of
                    Nothing -> do
                        let normalizedIncome = InterestRate.getNormalizedIncome updatedReserve (rLastUpdated reserve) slot
                        State.addUserConfig
                            userConfigId
                            UserConfig {
                                ucDebt = IncentivizedAmount slot (rCurrentStableBorrowRate reserve) (fromInteger 0),
                                ucCollateralizedInvestment = IncentivizedAmount slot normalizedIncome (fromInteger pcpAmount)
                            }
                            oldState
                    Just userConfig ->
                        State.updateUserConfig
                            userConfigId
                            (
                                Lens.over (_ucCollateralizedInvestment . _iaAmount) (+ fromInteger pcpAmount)
                                . updateConfigAmounts
                                    UpdateConfigParams {
                                        ucpUpdatedReserve = updatedReserve,
                                        ucpPreviousReserveUpdated = rLastUpdated reserve,
                                        ucpCurrentSlot = slot
                                    }
                                $ userConfig
                            )
                            oldState
                    >>= State.updateReserveNew pcpUnderlyingAsset updatedReserve)

    ledgerTx <- TxUtils.submitTxPair $ fundsLockingTx <> stateTx
    _ <- awaitTxConfirmed $ txId ledgerTx
    pure ()

data RevokeCollateralParams =
    RevokeCollateralParams {
        rcpUnderlyingAsset :: AssetClass,
        rcpAmount          :: Integer,
        rcpOnBehalfOf      :: PubKeyHash
    }
    deriving stock    (Prelude.Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON, ToSchema)

PlutusTx.unstableMakeIsData ''RevokeCollateralParams
PlutusTx.makeLift ''RevokeCollateralParams

-- | User withdraws N amount of collateralized aToken, his investment entry state is decreased by N
revokeCollateral :: Aave -> RevokeCollateralParams -> Contract w s Text ()
revokeCollateral aave RevokeCollateralParams {..} = do
    oldStateOutput <- State.findAaveState aave
    let oldState = ovValue oldStateOutput
    reserve <- maybe (throwError "Reserve not found") pure $ AssocMap.lookup rcpUnderlyingAsset (ansReserves oldState)
    let userConfigId = (rCurrency reserve, rcpOnBehalfOf)

    slot <- currentSlot
    newState <- do
        case AssocMap.lookup userConfigId (ansUserConfigs oldState) of
            Nothing ->
                throwError "User does not have any collateral."
            Just userConfig -> do
                availableLiquidity <- flip assetClassValueOf rcpUnderlyingAsset <$> State.getAaveCollateralValue aave
                let reserveConfigs = fmap snd . filter (\((asset, _), _) -> asset == rcpUnderlyingAsset) . AssocMap.toList $ (ansUserConfigs oldState)
                let updatedReserve = updateReserveOnLiquidityChange reserveConfigs (availableLiquidity - rcpAmount) slot reserve
                State.updateUserConfig
                    userConfigId
                    (
                        Lens.over (_ucCollateralizedInvestment . _iaAmount) (\e -> e - fromInteger rcpAmount)
                        . updateConfigAmounts
                            UpdateConfigParams {
                                ucpUpdatedReserve = updatedReserve,
                                ucpPreviousReserveUpdated = rLastUpdated reserve,
                                ucpCurrentSlot = slot
                            }
                        $ userConfig
                    )
                    oldState
                    >>= State.updateReserveNew rcpUnderlyingAsset updatedReserve

    oracles <- either throwError pure $ findOraclesForUser rcpOnBehalfOf (ansReserves newState) (ansUserConfigs newState)
    let aTokenAsset = rAToken reserve
    let redeemer = Core.RevokeCollateralRedeemer userConfigId aTokenAsset oracles slot

    utxos <-
        Map.filter (getUsersCollateral aTokenAsset)
        <$> utxoAt (Core.aaveAddress aave)
    let usersCollateralValue = Prelude.foldMap (txOutValue . txOutTxOut) utxos
    let inputs = (\(ref, tx) -> OutputValue ref tx redeemer) <$> Map.toList utxos
    let payment = assetClassValue aTokenAsset rcpAmount
    let remainder = assetClassValue aTokenAsset (assetClassValueOf usersCollateralValue aTokenAsset - rcpAmount)
    let fundsUnlockingTx =  TxUtils.mustSpendFromScript (Core.aaveInstance aave) inputs rcpOnBehalfOf payment <>
                            TxUtils.mustPayToScript (Core.aaveInstance aave) rcpOnBehalfOf (userDatum aTokenAsset) remainder

    stateTx <- (<> (mempty, mustValidateIn (Interval.from slot))) . fst <$>
        State.updateAaveState aave redeemer (newState Prelude.<$ oldStateOutput)

    oraclesTx <- mconcat <$> forM oracles Oracle.useOracle

    ledgerTx <- TxUtils.submitTxPair $ fundsUnlockingTx <> stateTx <> oraclesTx
    _ <- awaitTxConfirmed $ txId ledgerTx
    pure ()
    where
        userDatum = Core.UserCollateralFundsDatum rcpOnBehalfOf
        getUsersCollateral :: AssetClass -> TxOutTx -> Bool
        getUsersCollateral asset tx = ((> 0) . flip assetClassValueOf asset . txOutValue . txOutTxOut $ tx) &&
                                      (txOutDatumHash . txOutTxOut $ tx) == Just (datumHash . Datum . PlutusTx.toData $ userDatum asset)

getOwnPubKey :: Contract w s Text PubKeyHash
getOwnPubKey = pubKeyHash <$> ownPubKey

ownPubKeyBalance :: Contract w s Text Value
ownPubKeyBalance = getOwnPubKey >>= fundsAt

type AaveUserSchema =
    Endpoint "deposit" DepositParams
    .\/ Endpoint "withdraw" WithdrawParams
    .\/ Endpoint "borrow" BorrowParams
    .\/ Endpoint "repay" RepayParams
    .\/ Endpoint "provideCollateral" ProvideCollateralParams
    .\/ Endpoint "revokeCollateral" RevokeCollateralParams
    .\/ Endpoint "ownPubKey" ()
    .\/ Endpoint "ownPubKeyBalance" ()

data UserContractState =
    Deposited
    | Withdrawn
    | Borrowed
    | Repaid
    | CollateralProvided
    | CollateralRevoked
    | GetPubKey PubKeyHash
    | GetPubKeyBalance Value
    deriving (Prelude.Eq, Show, Generic, FromJSON, ToJSON)

Lens.makeClassyPrisms ''UserContractState

-- TODO ? add repayWithCollateral
userEndpoints :: Aave -> Contract (ContractResponse Text UserContractState) AaveUserSchema Void ()
userEndpoints aave = forever $
    withContractResponse (Proxy @"deposit") (const Deposited) (deposit aave)
    `select` withContractResponse (Proxy @"withdraw") (const Withdrawn) (withdraw aave)
    `select` withContractResponse (Proxy @"borrow") (const Borrowed) (borrow aave)
    `select` withContractResponse (Proxy @"repay") (const Repaid) (repay aave)
    `select` withContractResponse (Proxy @"provideCollateral") (const CollateralProvided) (provideCollateral aave)
    `select` withContractResponse (Proxy @"revokeCollateral") (const CollateralRevoked) (revokeCollateral aave)
    `select` withContractResponse (Proxy @"ownPubKey") GetPubKey (const getOwnPubKey)
    `select` withContractResponse (Proxy @"ownPubKeyBalance") GetPubKeyBalance (const ownPubKeyBalance)
