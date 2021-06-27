{-# LANGUAGE OverloadedStrings #-}

module Fixtures.Init where

import           Control.Monad              (forM, void)
import qualified Data.Map                   as Map
import           Data.Monoid                (Last (..))
import           Data.Text                  (Text)
import           Data.Void                  (Void)
import qualified Fixtures.Aave              as AaveMock
import           Fixtures.Asset             (defaultAssets)
import           Fixtures.Symbol            (forgeSymbol, getSymbol)
import           Fixtures.Wallet            (ownerWallet, userWallets)
import           Plutus.Contract
import qualified Plutus.Contracts.Core      as Aave
import           Plutus.Contracts.Endpoints (ContractResponse (..))
import qualified Plutus.Contracts.Endpoints as Aave
import qualified Plutus.Contracts.Oracle    as Oracle
import           Plutus.PAB.Simulation      (distributeFunds)
import qualified Plutus.Trace.Emulator      as Trace
import           Plutus.V1.Ledger.Ada       (lovelaceValueOf)
import           Plutus.V1.Ledger.Crypto    (PubKeyHash (..))
import           Plutus.V1.Ledger.Value     (AssetClass (..), Value,
                                             assetClassValue)
import qualified PlutusTx.AssocMap          as AssocMap
import           Wallet.Emulator.Wallet     (Wallet)

oracles :: [Oracle.Oracle]
oracles = fmap
    (\asset ->
        Oracle.Oracle
        {
            Oracle.oSymbol = getSymbol Oracle.oracleTokenName,
            Oracle.oOperator = PubKeyHash "mock",
            Oracle.oFee = 0,
            Oracle.oAsset = asset })
    defaultAssets

startParams :: [Aave.CreateParams]
startParams = fmap (\o -> Aave.CreateParams (Oracle.oAsset o) o) oracles

initialUsers :: AssocMap.Map (AssetClass, PubKeyHash) Aave.UserConfig
initialUsers = AssocMap.empty

initialReserves :: AssocMap.Map AssetClass Aave.Reserve
initialReserves = AssocMap.fromList (fmap (\params -> (Aave.cpAsset params, Aave.createReserve AaveMock.aave params)) startParams)

initialFunds :: Value
initialFunds = lovelaceValueOf 1000000 <> mconcat ((`assetClassValue` 1000) <$> defaultAssets)

startContract :: Contract () Aave.AaveOwnerSchema Text ()
startContract = void $ AaveMock.start startParams

userContract :: Contract (Last (ContractResponse Text Aave.UserContractState)) Aave.AaveUserSchema Void ()
userContract = void $ Aave.userEndpoints AaveMock.aave

distributeTrace :: Trace.EmulatorTrace ()
distributeTrace = do
    _ <- Trace.activateContractWallet ownerWallet $ distributeFunds userWallets defaultAssets
    _ <- Trace.waitNSlots 5
    pure ()

startTrace :: Trace.EmulatorTrace ()
startTrace = do
    _ <- Trace.activateContractWallet ownerWallet startContract
    _ <- Trace.waitNSlots 5
    pure ()

startOracles ::  Contract () BlockchainActions Text ()
startOracles = void $ forM oracles
    (\oracle -> do
        _ <- forgeSymbol Oracle.oracleTokenName
        Oracle.updateOracle oracle 1000000
    )

oracleTrace :: Trace.EmulatorTrace ()
oracleTrace = do
    _ <- Trace.activateContractWallet ownerWallet startOracles
    _ <- Trace.waitNSlots 5
    pure ()

type UserHandle = Trace.ContractHandle (Last (ContractResponse Text Aave.UserContractState)) Aave.AaveUserSchema Void

defaultTrace :: Trace.EmulatorTrace (Map.Map Wallet UserHandle)
defaultTrace = do
    _ <- distributeTrace
    _ <- oracleTrace
    _ <- startTrace
    fmap Map.fromList $ forM userWallets $ \wallet -> do
        handle <- Trace.activateContractWallet wallet userContract
        pure (wallet, handle)
