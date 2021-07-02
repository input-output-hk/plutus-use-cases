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
import           Plutus.Abstract.OutputValue                  (OutputValue (..))
import qualified Plutus.Abstract.TxUtils                      as TxUtils
import           Plutus.Contract                              hiding (when)
import           Plutus.Contracts.Currency                    as Currency
import qualified Plutus.Contracts.LendingPool.OffChain.AToken as AToken
import qualified Plutus.Contracts.LendingPool.OffChain.State  as State
import           Plutus.Contracts.LendingPool.OnChain.Core    (Aave,
                                                               AaveDatum (..),
                                                               AaveRedeemer (..),
                                                               Reserve (..),
                                                               UserConfig (..))
import qualified Plutus.Contracts.LendingPool.OnChain.Core    as Core
import qualified Plutus.Contracts.Service.FungibleToken       as FungibleToken
import qualified Plutus.Contracts.Service.Oracle              as Oracle
import           Plutus.V1.Ledger.Ada                         (adaValueOf,
                                                               lovelaceValueOf)
import qualified Plutus.V1.Ledger.Address                     as Addr
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
deposit :: (HasBlockchainActions s) => Aave -> DepositParams -> Contract w s Text ()
deposit aave DepositParams {..} = do
    reserve <- State.findAaveReserve aave dpAsset
    forgeTx <- AToken.forgeATokensFrom aave reserve dpOnBehalfOf dpAmount

    let userConfigId = (rCurrency reserve, dpOnBehalfOf)
    (userConfigsTx, _) <- do
        userConfigs <- ovValue <$> State.findAaveUserConfigs aave
        case AssocMap.lookup userConfigId userConfigs of
            Nothing ->
                State.addUserConfig
                    aave
                    (Core.DepositRedeemer userConfigId)
                    userConfigId
                    UserConfig { ucDebt = 0, ucCollateralizedInvestment = 0 }
            Just userConfig ->
                pure (mempty, userConfigs)

    (reservesTx, _) <- State.updateReserve aave (Core.DepositRedeemer userConfigId) dpAsset (reserve { rAmount = rAmount reserve + dpAmount })

    ledgerTx <- TxUtils.submitTxPair $ forgeTx <> reservesTx <> userConfigsTx
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
withdraw :: (HasBlockchainActions s) => Aave -> WithdrawParams -> Contract w s Text ()
withdraw aave WithdrawParams {..} = do
    reserve <- State.findAaveReserve aave wpAsset
    let userConfigId = (wpAsset, wpUser)

    burnTx <- AToken.burnATokensFrom aave reserve wpUser wpAmount

    (reservesTx, _) <- State.updateReserve aave (Core.WithdrawRedeemer userConfigId) wpAsset (reserve { rAmount = rAmount reserve - wpAmount })

    ledgerTx <- TxUtils.submitTxPair $ burnTx <> reservesTx
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

-- | The user borrows N amount of a needed asset from the corresponding reserve, his debt entry state is encreased by N
borrow :: (HasBlockchainActions s) => Aave -> BorrowParams -> Contract w s Text ()
borrow aave BorrowParams {..} = do
    reserves <- ovValue <$> State.findAaveReserves aave
    reserve <- maybe (throwError "Reserve not found") pure $ AssocMap.lookup bpAsset reserves
    let userConfigId = (rCurrency reserve, bpOnBehalfOf)
    userConfigs <- do
        userConfigs <- ovValue <$> State.findAaveUserConfigs aave
        pure $ case AssocMap.lookup userConfigId userConfigs of
            Nothing ->
                AssocMap.insert userConfigId UserConfig { ucDebt = bpAmount, ucCollateralizedInvestment = 0 } userConfigs
            Just userConfig ->
                AssocMap.insert userConfigId userConfig { ucDebt = ucDebt userConfig + bpAmount} userConfigs
    oracles <- either throwError pure $ findOraclesForUser bpOnBehalfOf reserves userConfigs
    let redeemer = Core.BorrowRedeemer userConfigId oracles

    utxos <-
        Map.filter ((> 0) . flip assetClassValueOf bpAsset . txOutValue . txOutTxOut)
        <$> utxoAt (Core.aaveAddress aave)
    let inputs = (\(ref, tx) -> OutputValue ref tx redeemer) <$> Map.toList utxos
    let payment = assetClassValue (rCurrency reserve) bpAmount
    let remainder = assetClassValue (rCurrency reserve) (rAmount reserve - bpAmount)
    let disbursementTx =  TxUtils.mustSpendFromScript (Core.aaveInstance aave) inputs bpOnBehalfOf payment <>
                            TxUtils.mustPayToScript (Core.aaveInstance aave) bpOnBehalfOf Core.ReserveFundsDatum remainder

    (userConfigsTx, _) <- do
        configsOutput <- State.findAaveUserConfigs aave
        State.updateUserConfigs aave redeemer $ userConfigs Prelude.<$ configsOutput

    (reservesTx, _) <- State.updateReserve aave redeemer bpAsset (reserve { rAmount = rAmount reserve - bpAmount })

    oraclesTx <- mconcat <$> forM oracles Oracle.useOracle

    ledgerTx <- TxUtils.submitTxPair $ disbursementTx <> reservesTx <> userConfigsTx <> oraclesTx
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
repay :: (HasBlockchainActions s) => Aave -> RepayParams -> Contract w s Text ()
repay aave RepayParams {..} = do
    reserve <- State.findAaveReserve aave rpAsset

    let payment = assetClassValue (rCurrency reserve) rpAmount
    let reimbursementTx = TxUtils.mustPayToScript (Core.aaveInstance aave) rpOnBehalfOf Core.ReserveFundsDatum payment

    let userConfigId = (rCurrency reserve, rpOnBehalfOf)
    (userConfigsTx, _) <- do
        userConfigs <- ovValue <$> State.findAaveUserConfigs aave
        case AssocMap.lookup userConfigId userConfigs of
            Nothing ->
                throwError "User does not have any debt."
            Just userConfig ->
                State.updateUserConfig aave (Core.RepayRedeemer userConfigId) userConfigId $ userConfig { ucDebt = ucDebt userConfig - rpAmount }

    (reservesTx, _) <- State.updateReserve aave (Core.RepayRedeemer userConfigId) rpAsset (reserve { rAmount = rAmount reserve + rpAmount })

    ledgerTx <- TxUtils.submitTxPair $ reimbursementTx <> reservesTx <> userConfigsTx
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
fundsAt :: HasBlockchainActions s => PubKeyHash -> Contract w s Text Value
fundsAt pkh = utxoValue <$> utxoAt (pubKeyHashAddress pkh)

balanceAt :: HasBlockchainActions s => PubKeyHash -> AssetClass -> Contract w s Text Integer
balanceAt pkh asset = flip assetClassValueOf asset <$> fundsAt pkh

-- | User deposits N amount of aToken as collateral, his investment entry state is increased by N
provideCollateral :: (HasBlockchainActions s) => Aave -> ProvideCollateralParams -> Contract w s Text ()
provideCollateral aave ProvideCollateralParams {..} = do
    reserve <- State.findAaveReserve aave pcpUnderlyingAsset

    let aTokenAsset = rAToken reserve
    userOwnedAtokenAmount <- balanceAt pcpOnBehalfOf aTokenAsset
    let payment = assetClassValue aTokenAsset pcpAmount
    let remainder = assetClassValue aTokenAsset (userOwnedAtokenAmount - pcpAmount)
    let fundsLockingTx = TxUtils.mustPayToScript (Core.aaveInstance aave) pcpOnBehalfOf (Core.UserCollateralFundsDatum pcpOnBehalfOf aTokenAsset) payment
                         <> (Prelude.mempty, mustPayToPubKey pcpOnBehalfOf remainder)

    let userConfigId = (rCurrency reserve, pcpOnBehalfOf)
    (userConfigsTx, _) <- do
        userConfigs <- ovValue <$> State.findAaveUserConfigs aave
        case AssocMap.lookup userConfigId userConfigs of
            Nothing ->
                State.addUserConfig
                    aave
                    (Core.ProvideCollateralRedeemer userConfigId)
                    userConfigId
                    UserConfig { ucDebt = 0, ucCollateralizedInvestment = pcpAmount }
            Just userConfig ->
                State.updateUserConfig aave (Core.ProvideCollateralRedeemer userConfigId) userConfigId $
                userConfig { ucCollateralizedInvestment = ucCollateralizedInvestment userConfig + pcpAmount }

    ledgerTx <- TxUtils.submitTxPair $ fundsLockingTx <> userConfigsTx
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
revokeCollateral :: (HasBlockchainActions s) => Aave -> RevokeCollateralParams -> Contract w s Text ()
revokeCollateral aave RevokeCollateralParams {..} = do
    reserves <- ovValue <$> State.findAaveReserves aave
    reserve <- maybe (throwError "Reserve not found") pure $ AssocMap.lookup rcpUnderlyingAsset reserves
    let userConfigId = (rCurrency reserve, rcpOnBehalfOf)
    userConfigs <- do
        userConfigs <- ovValue <$> State.findAaveUserConfigs aave
        case AssocMap.lookup userConfigId userConfigs of
            Nothing ->
                throwError "User does not have any collateral."
            Just userConfig -> pure $
                AssocMap.insert userConfigId userConfig { ucCollateralizedInvestment = ucCollateralizedInvestment userConfig - rcpAmount } userConfigs
    oracles <- either throwError pure $ findOraclesForUser rcpOnBehalfOf reserves userConfigs
    let aTokenAsset = rAToken reserve
    let redeemer = Core.RevokeCollateralRedeemer userConfigId aTokenAsset oracles

    utxos <-
        Map.filter (getUsersCollateral aTokenAsset)
        <$> utxoAt (Core.aaveAddress aave)
    let usersCollateralValue = Prelude.foldMap (txOutValue . txOutTxOut) utxos
    let inputs = (\(ref, tx) -> OutputValue ref tx redeemer) <$> Map.toList utxos
    let payment = assetClassValue aTokenAsset rcpAmount
    let remainder = assetClassValue aTokenAsset (assetClassValueOf usersCollateralValue aTokenAsset - rcpAmount)
    let fundsUnlockingTx =  TxUtils.mustSpendFromScript (Core.aaveInstance aave) inputs rcpOnBehalfOf payment <>
                            TxUtils.mustPayToScript (Core.aaveInstance aave) rcpOnBehalfOf (userDatum aTokenAsset) remainder

    (userConfigsTx, _) <- do
        configsOutput <- State.findAaveUserConfigs aave
        State.updateUserConfigs aave redeemer $ userConfigs Prelude.<$ configsOutput

    reservesTx <- State.roundtripReserves aave redeemer

    oraclesTx <- mconcat <$> forM oracles Oracle.useOracle

    ledgerTx <- TxUtils.submitTxPair $ fundsUnlockingTx <> userConfigsTx <> reservesTx <> oraclesTx
    _ <- awaitTxConfirmed $ txId ledgerTx
    pure ()
    where
        userDatum = Core.UserCollateralFundsDatum rcpOnBehalfOf
        getUsersCollateral :: AssetClass -> TxOutTx -> Bool
        getUsersCollateral asset tx = ((> 0) . flip assetClassValueOf asset . txOutValue . txOutTxOut $ tx) &&
                                      (txOutDatumHash . txOutTxOut $ tx) == Just (datumHash . Datum . PlutusTx.toData $ userDatum asset)

getOwnPubKey :: HasBlockchainActions s => Contract w s Text PubKeyHash
getOwnPubKey = pubKeyHash <$> ownPubKey

ownPubKeyBalance :: HasBlockchainActions s => Contract w s Text Value
ownPubKeyBalance = getOwnPubKey >>= fundsAt

type AaveUserSchema =
    BlockchainActions
        .\/ Endpoint "deposit" DepositParams
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
