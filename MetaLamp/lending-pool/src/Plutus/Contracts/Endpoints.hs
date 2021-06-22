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

module Plutus.Contracts.Endpoints where

import           Control.Monad                    hiding (fmap)
import qualified Data.ByteString                  as BS
import qualified Data.Map                         as Map
import           Data.Monoid                      (Last (..))
import           Data.Proxy                       (Proxy (..))
import           Data.Text                        (Text, pack)
import qualified Data.Text                        as Text
import           Data.Void                        (Void)
import           Ledger                           hiding (singleton)
import           Ledger.Constraints               as Constraints
import           Ledger.Constraints.OnChain       as Constraints
import           Ledger.Constraints.TxConstraints as Constraints
import qualified Ledger.Scripts                   as Scripts
import qualified Ledger.Typed.Scripts             as Scripts
import           Playground.Contract
import           Plutus.Contract                  hiding (when)
import qualified Plutus.Contracts.AToken          as AToken
import           Plutus.Contracts.Core            (Aave, AaveDatum (..),
                                                   AaveRedeemer (..),
                                                   Reserve (..),
                                                   UserConfig (..))
import qualified Plutus.Contracts.Core            as Core
import           Plutus.Contracts.Currency        as Currency
import qualified Plutus.Contracts.FungibleToken   as FungibleToken
import qualified Plutus.Contracts.State           as State
import qualified Plutus.Contracts.TxUtils         as TxUtils
import           Plutus.OutputValue               (OutputValue (..))
import           Plutus.V1.Ledger.Ada             (adaValueOf, lovelaceValueOf)
import qualified Plutus.V1.Ledger.Address         as Addr
import           Plutus.V1.Ledger.Value           as Value
import qualified PlutusTx
import qualified PlutusTx.AssocMap                as AssocMap
import           PlutusTx.Prelude                 hiding (Monoid (..),
                                                   Semigroup (..), mconcat,
                                                   unless)
import           Prelude                          (Monoid (..), Semigroup (..),
                                                   show, subtract)
import qualified Prelude
import           Text.Printf                      (printf)
import qualified Plutus.Contracts.Oracle as Oracle

data CreateParams =
    CreateParams
        { cpAsset :: AssetClass,
          cpOracle :: Oracle.Oracle
         }
    deriving stock (Prelude.Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema)

PlutusTx.makeLift ''CreateParams

createReserve :: Aave -> CreateParams -> Reserve
createReserve aave CreateParams {..} =
    Reserve
        { rCurrency = cpAsset,
          rAmount = 0,
          rAToken = AToken.makeAToken (Core.aaveHash aave) cpAsset,
          rLiquidityIndex = 1,
          rCurrentStableBorrowRate = 11 % 10, -- TODO configure borrow rate when lending core will be ready
          rTrustedOracle = Oracle.toTuple cpOracle
           }

-- | Starts the Lending Pool protocol: minting pool NFTs, creating empty user configuration state and all specified liquidity reserves
start :: HasBlockchainActions s => [CreateParams] -> Contract w s Text Aave
start params = do
    pkh <- pubKeyHash <$> ownPubKey
    aaveToken  <- fmap Currency.currencySymbol $
           mapError (pack . show @Currency.CurrencyError) $
           Currency.forgeContract pkh [(Core.aaveProtocolName, 1)]
    let aave = Core.aave aaveToken
        payment = assetClassValue (Core.aaveProtocolInst aave) 1
    let aaveTokenTx = TxUtils.mustPayToScript (Core.aaveInstance aave) pkh (Core.LendingPoolDatum pkh) payment
    -- TODO how to ensure that newly minted owner token is paid to the script before someone else spends it?
    ledgerTx <- TxUtils.submitTxPair aaveTokenTx
    void $ awaitTxConfirmed $ txId ledgerTx

    let reserveMap = AssocMap.fromList $ fmap (\params -> (cpAsset params, createReserve aave params)) params
    reservesTx <- State.putReserves aave Core.StartRedeemer reserveMap
    ledgerTx <- TxUtils.submitTxPair reservesTx
    void $ awaitTxConfirmed $ txId ledgerTx
    userConfigsTx <- State.putUserConfigs aave Core.StartRedeemer AssocMap.empty
    ledgerTx <- TxUtils.submitTxPair userConfigsTx
    void $ awaitTxConfirmed $ txId ledgerTx

    logInfo @Prelude.String $ printf "started Aave %s at address %s" (show aave) (show $ Core.aaveAddress aave)
    pure aave

ownerEndpoint :: [CreateParams] -> Contract (Last (Either Text Aave)) BlockchainActions Void ()
ownerEndpoint params = do
    e <- runError $ start params
    tell $ Last $ Just $ case e of
        Left err -> Left err
        Right aa -> Right aa

type AaveOwnerSchema =
    BlockchainActions
        .\/ Endpoint "start" ()

-- | Gets current Lending Pool reserves state
reserves :: HasBlockchainActions s => Aave -> Contract w s Text (AssocMap.Map AssetClass Reserve)
reserves aave = ovValue <$> State.findAaveReserves aave

-- | Gets current Lending Pool user configs state
users :: HasBlockchainActions s => Aave -> Contract w s Text (AssocMap.Map (AssetClass, PubKeyHash) UserConfig)
users aave = ovValue <$> State.findAaveUserConfigs aave

valueAt :: HasBlockchainActions s => Address -> Contract w s Text Value
valueAt address = do
    os <- map snd . Map.toList <$> utxoAt address
    pure $ mconcat [txOutValue $ txOutTxOut o | o <- os]

getOwnPubKey :: HasBlockchainActions s => Contract w s Text PubKeyHash
getOwnPubKey = pubKeyHash <$> ownPubKey

-- | Gets all UTxOs belonging to a user and concats them into one Value
fundsAt :: HasBlockchainActions s => PubKeyHash -> Contract w s Text Value
fundsAt pkh = valueAt (pubKeyHashAddress pkh)

balanceAt :: HasBlockchainActions s => PubKeyHash -> AssetClass -> Contract w s Text Integer
balanceAt pkh asset = flip assetClassValueOf asset <$> fundsAt pkh

-- | Gets all UTxOs belonging to the Lending Pool script and concats them into one Value
poolFunds :: HasBlockchainActions s => Aave -> Contract w s Text Value
poolFunds aave = valueAt (Core.aaveAddress aave)

ownPubKeyBalance :: HasBlockchainActions s => Contract w s Text Value
ownPubKeyBalance = getOwnPubKey >>= fundsAt

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
    reserve <- State.findAaveReserve aave bpAsset

    utxos <-
        Map.filter ((> 0) . flip assetClassValueOf bpAsset . txOutValue . txOutTxOut)
        <$> utxoAt (Core.aaveAddress aave)
    let userConfigId = (rCurrency reserve, bpOnBehalfOf)
    let inputs = (\(ref, tx) -> OutputValue ref tx (Core.BorrowRedeemer userConfigId)) <$> Map.toList utxos
    let payment = assetClassValue (rCurrency reserve) bpAmount
    let remainder = assetClassValue (rCurrency reserve) (rAmount reserve - bpAmount)
    let disbursementTx =  TxUtils.mustSpendFromScript (Core.aaveInstance aave) inputs bpOnBehalfOf payment <>
                            TxUtils.mustPayToScript (Core.aaveInstance aave) bpOnBehalfOf Core.ReserveFundsDatum remainder

    (userConfigsTx, userConfigs) <- do
        userConfigs <- ovValue <$> State.findAaveUserConfigs aave
        case AssocMap.lookup userConfigId userConfigs of
            Nothing ->
                State.addUserConfig
                aave
                (Core.BorrowRedeemer userConfigId)
                userConfigId
                UserConfig { ucDebt = bpAmount, ucCollateralizedInvestment = 0 }
            Just userConfig ->
                State.updateUserConfig aave (Core.BorrowRedeemer userConfigId) userConfigId $
                userConfig { ucDebt = ucDebt userConfig + bpAmount}

    (reservesTx, reserves) <- State.updateReserve aave (Core.BorrowRedeemer userConfigId) bpAsset (reserve { rAmount = rAmount reserve - bpAmount })

    oracles <- either throwError pure $ findOraclesForUser bpOnBehalfOf reserves userConfigs
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
        pcpUnderlyingAsset      :: AssetClass,
        pcpAmount     :: Integer,
        pcpOnBehalfOf :: PubKeyHash
    }
    deriving stock    (Prelude.Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON, ToSchema)

PlutusTx.unstableMakeIsData ''ProvideCollateralParams
PlutusTx.makeLift ''ProvideCollateralParams

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
        rcpUnderlyingAsset      :: AssetClass,
        rcpAmount     :: Integer,
        rcpOnBehalfOf :: PubKeyHash
    }
    deriving stock    (Prelude.Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON, ToSchema)

PlutusTx.unstableMakeIsData ''RevokeCollateralParams
PlutusTx.makeLift ''RevokeCollateralParams

revokeCollateral :: (HasBlockchainActions s) => Aave -> RevokeCollateralParams -> Contract w s Text ()
revokeCollateral aave RevokeCollateralParams {..} = do
    reserve <- State.findAaveReserve aave rcpUnderlyingAsset

    let aTokenAsset = rAToken reserve
    let userConfigId = (rCurrency reserve, rcpOnBehalfOf)
    let redeemer = Core.RevokeCollateralRedeemer userConfigId aTokenAsset
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
        userConfigs <- ovValue <$> State.findAaveUserConfigs aave
        case AssocMap.lookup userConfigId userConfigs of
            Nothing ->
                throwError "User does not have any collateral."
            Just userConfig ->
                State.updateUserConfig aave redeemer userConfigId $
                userConfig { ucCollateralizedInvestment = ucCollateralizedInvestment userConfig - rcpAmount }

    ledgerTx <- TxUtils.submitTxPair $ fundsUnlockingTx <> userConfigsTx
    _ <- awaitTxConfirmed $ txId ledgerTx
    pure ()
    where
        userDatum = Core.UserCollateralFundsDatum rcpOnBehalfOf
        getUsersCollateral :: AssetClass -> TxOutTx -> Bool
        getUsersCollateral asset tx = ((> 0) . flip assetClassValueOf asset . txOutValue . txOutTxOut $ tx) &&
                                      (txOutDatumHash . txOutTxOut $ tx) == Just (datumHash . Datum . PlutusTx.toData $ userDatum asset)

data ContractResponse e a = ContractSuccess a | ContractError e | ContractPending
    deriving stock    (Prelude.Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

handleContract :: forall l a p r s.
    HasEndpoint l p s
    => Proxy l
    -> (a -> r)
    -> (p -> Contract (Last (ContractResponse Text r)) s Text a)
    -> Contract (Last (ContractResponse Text r)) s Void ()
handleContract _ g c = do
    e <- runError $ do
        p <- endpoint @l
        _ <- tell $ Last $ Just ContractPending
        errorHandler `handleError` c p
    tell $ Last $ Just $ case e of
        Left err -> ContractError err
        Right a  -> ContractSuccess $ g a
        where
        errorHandler e = do
            logInfo @Text ("Error submiting the transaction: " <> e)
            throwError e

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

-- TODO ? add repayWithCollateral
userEndpoints :: Aave -> Contract (Last (ContractResponse Text UserContractState)) AaveUserSchema Void ()
userEndpoints aave = forever $
    handleContract (Proxy @"deposit") (const Deposited) (deposit aave)
    `select` handleContract (Proxy @"withdraw") (const Withdrawn) (withdraw aave)
    `select` handleContract (Proxy @"borrow") (const Borrowed) (borrow aave)
    `select` handleContract (Proxy @"repay") (const Repaid) (repay aave)
    `select` handleContract (Proxy @"provideCollateral") (const CollateralProvided) (provideCollateral aave)
    `select` handleContract (Proxy @"revokeCollateral") (const CollateralRevoked) (revokeCollateral aave)
    `select` handleContract (Proxy @"ownPubKey") GetPubKey (const getOwnPubKey)
    `select` handleContract (Proxy @"ownPubKeyBalance") GetPubKeyBalance (const ownPubKeyBalance)

type AaveInfoSchema =
    BlockchainActions
    .\/ Endpoint "fundsAt" PubKeyHash
    .\/ Endpoint "poolFunds" ()
    .\/ Endpoint "reserves" ()
    .\/ Endpoint "users" ()

data InfoContractState =
    FundsAt Value
    | PoolFunds Value
    | Reserves (AssocMap.Map AssetClass Reserve)
    | Users (AssocMap.Map (AssetClass, PubKeyHash) UserConfig)
    deriving (Prelude.Eq, Show, Generic, FromJSON, ToJSON)

infoEndpoints :: Aave -> Contract (Last (ContractResponse Text InfoContractState)) AaveInfoSchema Void ()
infoEndpoints aave = forever $
    handleContract (Proxy @"fundsAt") FundsAt fundsAt
    `select` handleContract (Proxy @"poolFunds") PoolFunds (const $ poolFunds aave)
    `select` handleContract (Proxy @"reserves") Reserves (const $ reserves aave)
    `select` handleContract (Proxy @"users") Users (const $ users aave)
