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
import qualified Data.Map                         as Map
import           Data.Monoid                      (Last (..))
import           Data.Proxy                       (Proxy (..))
import           Data.Text                        (Text, pack)
import           Data.Void                        (Void)
import           Ledger                           hiding (singleton)
import           Ledger.Constraints               as Constraints
import           Ledger.Constraints.OnChain       as Constraints
import           Ledger.Constraints.TxConstraints as Constraints
import qualified Ledger.Scripts                   as Scripts
import qualified Ledger.Typed.Scripts             as Scripts
import           Playground.Contract
import           Plutus.Contract                  hiding (when)
-- TODO remove that dep Plutus.Contracts.Currency (?)
import qualified Data.ByteString                  as BS
import qualified Plutus.Contracts.AToken          as AToken
import           Plutus.Contracts.Core            (Aave, AaveAction (..),
                                                   AaveDatum (..), Factory,
                                                   LendingPool (..),
                                                   LendingPoolId,
                                                   UserConfig (..))
import qualified Plutus.Contracts.Core            as Core
import           Plutus.Contracts.Currency        as Currency
import qualified Plutus.Contracts.FungibleToken   as FungibleToken
import           Plutus.Contracts.State           (StateOutput (..))
import qualified Plutus.Contracts.State           as State
import           Plutus.V1.Ledger.Ada             (adaValueOf, lovelaceValueOf)
import qualified Plutus.V1.Ledger.Address         as Addr
import           Plutus.V1.Ledger.Value           as Value
import qualified PlutusTx
import           PlutusTx.Prelude                 hiding (Semigroup (..),
                                                   unless)
import           Prelude                          (Semigroup (..))
import qualified Prelude
import           Text.Printf                      (printf)

errorHandler t = logInfo @Text ("Error submiting the transaction!: " <> t)

newtype CreateParams =
    CreateParams
        { cpAsset :: AssetClass }
    deriving (Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema)

PlutusTx.makeLift ''CreateParams

createPool :: CreateParams -> LendingPool
createPool CreateParams {..} =
    LendingPool
        { lpCurrency = cpAsset,
          lpAmount = 0,
          lpAToken = AToken.makeAToken cpAsset,
          lpDebtToken = cpAsset,
          lpLiquidityIndex = 1 }

start :: HasBlockchainActions s => [CreateParams] -> Contract w s Text Aave
start params = do
    pkh <- pubKeyHash <$> ownPubKey
    aaveToken  <- fmap Currency.currencySymbol $
           mapError (pack . show @Currency.CurrencyError) $
           Currency.forgeContract pkh [(Core.aaveProtocolName, 1)]
    let aave = Core.aave aaveToken
    let pools = fmap createPool params
    let factoryCoin = assetClass aaveToken Core.aaveProtocolName
        inst = Core.aaveInstance aave
        tx = mustPayToTheScript (Factory (fmap lpCurrency pools)) $ assetClassValue factoryCoin 1
    ledgerTx <- submitTxConstraints inst tx
    void $ awaitTxConfirmed $ txId ledgerTx
    traverse_ (State.putPool aave) pools
    logInfo @String $ printf "started Aave %s at address %s" (show aave) (show $ Core.aaveAddress aave)
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

factory :: forall w s. HasBlockchainActions s => Aave -> Contract w s Text Factory
factory = fmap soDatum . State.findAaveFactory

pools :: forall w s. HasBlockchainActions s => Aave -> Contract w s Text [LendingPool]
pools aave = Prelude.fmap soDatum <$> State.findOutputsBy aave (Core.poolStateToken aave) State.pickPool

users :: forall w s. HasBlockchainActions s => Aave -> Contract w s Text [UserConfig]
users aave = Prelude.fmap soDatum <$> State.findOutputsBy aave (Core.userStateToken aave) State.pickUser

valueAt :: HasBlockchainActions s => Address -> Contract w s Text Value
valueAt address = do
    os <- map snd . Map.toList <$> utxoAt address
    pure $ mconcat [txOutValue $ txOutTxOut o | o <- os]

fundsAt :: HasBlockchainActions s => PubKeyHash -> Contract w s Text Value
fundsAt pkh = valueAt (pubKeyHashAddress pkh)

balanceAt :: HasBlockchainActions s => PubKeyHash -> AssetClass -> Contract w s Text Integer
balanceAt pkh asset = flip assetClassValueOf asset <$> fundsAt pkh

poolFunds :: forall w s. HasBlockchainActions s => Aave -> Contract w s Text Value
poolFunds aave = valueAt (Core.aaveAddress aave)

data DepositParams =
  DepositParams {
    dpAsset      :: AssetClass,
    dpOnBehalfOf :: PubKeyHash,
    dpAmount     :: Integer
  }
    deriving stock    (Show, Generic)
    deriving anyclass (ToJSON, FromJSON, ToSchema)

PlutusTx.unstableMakeIsData ''DepositParams
PlutusTx.makeLift ''DepositParams

deposit :: (HasBlockchainActions s) => Aave -> DepositParams -> Contract w s Text ()
deposit aave DepositParams {..} = do
    reserveOutput <- State.findAavePool aave dpAsset
    let reserve = soDatum reserveOutput
        lookups = Constraints.ownPubKeyHash dpOnBehalfOf
            <> Constraints.scriptInstanceLookups (Core.aaveInstance aave)
        outValue = assetClassValue (lpCurrency reserve) dpAmount
        tx = mustPayToTheScript Core.Deposit outValue
    ledgerTx <- submitTxConstraintsWith lookups tx
    _ <- awaitTxConfirmed $ txId ledgerTx

    logInfo @String $ "HASH " <> show dpOnBehalfOf

    wasZeroBalance <- (== 0) <$> balanceAt dpOnBehalfOf (lpAToken reserve)
    _ <- AToken.forgeATokensFrom aave reserve dpOnBehalfOf dpAmount
    when wasZeroBalance $ do
        userOutputs <- State.findOutputsBy aave (Core.userStateToken aave) State.pickUser
        case userOutputs of
            [] -> void $
                State.putUser aave $ UserConfig { ucAddress = dpOnBehalfOf, ucReserveId = lpCurrency reserve, ucUsingAsCollateral = True }
            [userOutput] -> void $
                State.updateUser aave $ Prelude.fmap (\u -> u { ucUsingAsCollateral = True }) userOutput
            _ -> throwError "Invalid state: multiple users"

    void $ State.updatePool aave $ Prelude.fmap (\r -> r { lpAmount = lpAmount r + dpAmount }) reserveOutput

data WithdrawParams =
    WithdrawParams {
        wpAsset  :: AssetClass,
        wpTo     :: PubKeyHash,
        wpFrom   :: PubKeyHash,
        wpAmount :: Integer
    }
    deriving stock    (Show, Generic)
    deriving anyclass (ToJSON, FromJSON, ToSchema)

PlutusTx.unstableMakeIsData ''WithdrawParams
PlutusTx.makeLift ''WithdrawParams

withdraw :: (HasBlockchainActions s) => Aave -> WithdrawParams -> Contract w s Text ()
withdraw aave WithdrawParams {..} = do
    reserveOutput <- State.findAavePool aave wpAsset
    let reserve = soDatum reserveOutput

    balance <- balanceAt wpFrom (lpAToken reserve)
    when (wpAmount == balance) $ do
        userOutput <- State.findAaveUser aave wpFrom wpAsset
        void $
            State.updateUser aave $ Prelude.fmap (\u -> u { ucUsingAsCollateral = False }) userOutput

    _ <- AToken.burnATokensFrom aave reserve wpTo wpAmount

    void $ State.updatePool aave $ Prelude.fmap (\r -> r { lpAmount = lpAmount r - wpAmount }) reserveOutput

type AaveUserSchema =
    BlockchainActions
        .\/ Endpoint "deposit" DepositParams
        .\/ Endpoint "withdraw" WithdrawParams
        .\/ Endpoint "fundsAt" PubKeyHash
        .\/ Endpoint "poolFunds" ()
        .\/ Endpoint "factory" ()
        .\/ Endpoint "pools" ()
        .\/ Endpoint "users" ()

data UserContractState = Created
    | Closed
    | Stopped
    | Deposited
    | Withdrawn
    | FundsAt Value
    | PoolFunds Value
    | FactoryEndpoint Factory
    | Pools [LendingPool]
    | Users [UserConfig]
    deriving (Show, Generic, FromJSON, ToJSON)

userEndpoints :: Aave -> Contract (Last (Either Text UserContractState)) AaveUserSchema Void ()
userEndpoints aa = forever $
    f (Proxy @"deposit") (const Deposited) deposit
    `select` f (Proxy @"withdraw") (const Withdrawn) withdraw
    `select` f (Proxy @"fundsAt") FundsAt (\_ pkh -> fundsAt pkh)
    `select` f (Proxy @"poolFunds") PoolFunds (\aave () -> poolFunds aave)
    `select` f (Proxy @"factory") FactoryEndpoint (\aave () -> factory aave)
    `select` f (Proxy @"pools") Pools (\aave () -> pools aave)
    `select` f (Proxy @"users") Users (\aave () -> users aave)
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
            c aa p
        tell $ Last $ Just $ case e of
            Left err -> Left err
            Right a  -> Right $ g a
