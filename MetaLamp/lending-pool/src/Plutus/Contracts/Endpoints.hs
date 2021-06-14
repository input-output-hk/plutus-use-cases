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
import           Prelude                          (Monoid (..), Semigroup (..), show, subtract)
import qualified Prelude
import           Text.Printf                      (printf)

newtype CreateParams =
    CreateParams
        { cpAsset :: AssetClass }
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
          rCurrentStableBorrowRate = 11 % 10 -- TODO configure borrow rate when lending core will be ready
           }

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

reserves :: HasBlockchainActions s => Aave -> Contract w s Text (AssocMap.Map AssetClass Reserve)
reserves aave = ovValue <$> State.findAaveReserves aave

users :: HasBlockchainActions s => Aave -> Contract w s Text (AssocMap.Map (AssetClass, PubKeyHash) UserConfig)
users aave = ovValue <$> State.findAaveUserConfigs aave

valueAt :: HasBlockchainActions s => Address -> Contract w s Text Value
valueAt address = do
    os <- map snd . Map.toList <$> utxoAt address
    pure $ mconcat [txOutValue $ txOutTxOut o | o <- os]

getOwnPubKey :: HasBlockchainActions s => Contract w s Text PubKeyHash
getOwnPubKey = pubKeyHash <$> ownPubKey

fundsAt :: HasBlockchainActions s => PubKeyHash -> Contract w s Text Value
fundsAt pkh = valueAt (pubKeyHashAddress pkh)

balanceAt :: HasBlockchainActions s => PubKeyHash -> AssetClass -> Contract w s Text Integer
balanceAt pkh asset = flip assetClassValueOf asset <$> fundsAt pkh

poolFunds :: HasBlockchainActions s => Aave -> Contract w s Text Value
poolFunds aave = valueAt (Core.aaveAddress aave)

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

deposit :: (HasBlockchainActions s) => Aave -> DepositParams -> Contract w s Text ()
deposit aave DepositParams {..} = do
    reserve <- State.findAaveReserve aave dpAsset
    forgeTx <- AToken.forgeATokensFrom aave reserve dpOnBehalfOf dpAmount

    let userConfigId = (rCurrency reserve, dpOnBehalfOf)
    wasZeroBalance <- (== 0) <$> balanceAt dpOnBehalfOf (rAToken reserve)
    userConfigsTx <- if wasZeroBalance then do
        userConfigs <- ovValue <$> State.findAaveUserConfigs aave
        case AssocMap.lookup userConfigId userConfigs of
            Nothing ->
                State.addUserConfig
                    aave
                    (Core.DepositRedeemer userConfigId)
                    userConfigId
                    UserConfig { ucUsingAsCollateral = True, ucDebt = Nothing }
            Just userConfig ->
                State.updateUserConfig aave (Core.DepositRedeemer userConfigId) userConfigId $ userConfig { ucUsingAsCollateral = True }
        else pure mempty

    reservesTx <- State.updateReserve aave (Core.DepositRedeemer userConfigId) dpAsset (reserve { rAmount = rAmount reserve + dpAmount })

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

withdraw :: (HasBlockchainActions s) => Aave -> WithdrawParams -> Contract w s Text ()
withdraw aave WithdrawParams {..} = do
    reserve <- State.findAaveReserve aave wpAsset

    let userConfigId = (wpAsset, wpUser)
    balance <- balanceAt wpUser (rAToken reserve)
    userConfigsTx <- if wpAmount == balance then do
        userConfig <- State.findAaveUserConfig aave userConfigId
        State.updateUserConfig aave (Core.WithdrawRedeemer userConfigId) userConfigId $ userConfig { ucUsingAsCollateral = False }
        else pure mempty

    burnTx <- AToken.burnATokensFrom aave reserve wpUser wpAmount

    reservesTx <- State.updateReserve aave (Core.WithdrawRedeemer userConfigId) wpAsset (reserve { rAmount = rAmount reserve - wpAmount })

    ledgerTx <- TxUtils.submitTxPair $ burnTx <> reservesTx <> userConfigsTx
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

    userConfigs <- ovValue <$> State.findAaveUserConfigs aave
    userConfigsTx <- case AssocMap.lookup userConfigId userConfigs of
        Nothing ->
            State.addUserConfig
            aave
            (Core.BorrowRedeemer userConfigId)
            userConfigId
            UserConfig {ucUsingAsCollateral = False, ucDebt = Just bpAmount}
        Just userConfig ->
            State.updateUserConfig aave (Core.BorrowRedeemer userConfigId) userConfigId $
            userConfig {ucDebt = Just $ maybe bpAmount (+ bpAmount) $ ucDebt userConfig}

    reservesTx <- State.updateReserve aave (Core.BorrowRedeemer userConfigId) bpAsset (reserve { rAmount = rAmount reserve - bpAmount })

    ledgerTx <- TxUtils.submitTxPair $ disbursementTx <> reservesTx <> userConfigsTx
    _ <- awaitTxConfirmed $ txId ledgerTx
    pure ()

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

repay :: (HasBlockchainActions s) => Aave -> RepayParams -> Contract w s Text ()
repay aave RepayParams {..} = do
    reserve <- State.findAaveReserve aave rpAsset

    let payment = assetClassValue (rCurrency reserve) rpAmount
    let reimbursementTx = TxUtils.mustPayToScript (Core.aaveInstance aave) rpOnBehalfOf Core.ReserveFundsDatum payment

    userConfigs <- ovValue <$> State.findAaveUserConfigs aave
    let userConfigId = (rCurrency reserve, rpOnBehalfOf)
    userConfigsTx <- case AssocMap.lookup userConfigId userConfigs of
            Nothing ->
                throwError "User does not have any debt."
            Just userConfig ->
                State.updateUserConfig aave (Core.RepayRedeemer userConfigId) userConfigId $ userConfig { ucDebt = subtract rpAmount <$> ucDebt userConfig }

    reservesTx <- State.updateReserve aave (Core.RepayRedeemer userConfigId) rpAsset (reserve { rAmount = rAmount reserve + rpAmount })

    ledgerTx <- TxUtils.submitTxPair $ reimbursementTx <> reservesTx <> userConfigsTx
    _ <- awaitTxConfirmed $ txId ledgerTx
    pure ()

type AaveUserSchema =
    BlockchainActions
        .\/ Endpoint "deposit" DepositParams
        .\/ Endpoint "withdraw" WithdrawParams
        .\/ Endpoint "borrow" BorrowParams
        .\/ Endpoint "repay" RepayParams
        .\/ Endpoint "fundsAt" PubKeyHash
        .\/ Endpoint "poolFunds" ()
        .\/ Endpoint "reserves" ()
        .\/ Endpoint "users" ()
        .\/ Endpoint "ownPubKey" ()

data UserContractState =
    Pending
    | Created
    | Closed
    | Stopped
    | Deposited
    | Withdrawn
    | Borrowed
    | Repaid
    | FundsAt Value
    | PoolFunds Value
    | Reserves (AssocMap.Map AssetClass Reserve)
    | Users (AssocMap.Map (AssetClass, PubKeyHash) UserConfig)
    | GetPubKey PubKeyHash
    deriving (Prelude.Eq, Show, Generic, FromJSON, ToJSON)

userEndpoints :: Aave -> Contract (Last (Either Text UserContractState)) AaveUserSchema Void ()
userEndpoints aa = forever $
    f (Proxy @"deposit") (const Deposited) deposit
    `select` f (Proxy @"withdraw") (const Withdrawn) withdraw
    `select` f (Proxy @"borrow") (const Borrowed) borrow
    `select` f (Proxy @"repay") (const Repaid) repay
    `select` f (Proxy @"fundsAt") FundsAt (\_ pkh -> fundsAt pkh)
    `select` f (Proxy @"poolFunds") PoolFunds (\aave () -> poolFunds aave)
    `select` f (Proxy @"reserves") Reserves (\aave () -> reserves aave)
    `select` f (Proxy @"users") Users (\aave () -> users aave)
    `select` f (Proxy @"ownPubKey") GetPubKey (\_ () -> getOwnPubKey)
  where
    f :: forall l a p.
        HasEndpoint l p AaveUserSchema
      => Proxy l
      -> (a -> UserContractState)
      -> (Aave -> p -> Contract (Last (Either Text UserContractState)) AaveUserSchema Text a)
      -> Contract (Last (Either Text UserContractState)) AaveUserSchema Void ()
    f _ g c = do
        e <- runError $ do
            p <- endpoint @l
            _ <- tell $ Last $ Just $ Right $ Pending
            errorHandler `handleError` c aa p
        tell $ Last $ Just $ case e of
            Left err -> Left err
            Right a  -> Right $ g a
    errorHandler e = do
        logInfo @Text ("Error submiting the transaction: " <> e)
        throwError e
