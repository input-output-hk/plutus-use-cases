module Fixtures.Init where

import           Control.Monad              (forM, void)
import qualified Data.Map                   as Map
import           Data.Monoid                (Last (..))
import           Data.Text                  (Text)
import           Data.Void                  (Void)
import qualified Fixtures.Aave              as AaveMock
import           Fixtures.Asset             (defaultAssets)
import           Fixtures.Wallet            (ownerWallet, userWallets)
import           Plutus.Contract
import qualified Plutus.Contracts.Core      as Aave
import           Plutus.Contracts.Endpoints (ContractResponse (..))
import qualified Plutus.Contracts.Endpoints as Aave
import           Plutus.PAB.Simulation      (initContract)
import qualified Plutus.Trace.Emulator      as Trace
import           Plutus.V1.Ledger.Ada       (lovelaceValueOf)
import           Plutus.V1.Ledger.Crypto    (PubKeyHash)
import           Plutus.V1.Ledger.Value     (AssetClass, Value, assetClassValue)
import qualified PlutusTx.AssocMap          as AssocMap
import           Wallet.Emulator.Wallet     (Wallet)

initialReserves :: AssocMap.Map AssetClass Aave.Reserve
initialReserves = AssocMap.fromList (fmap (\params -> (Aave.cpAsset params, Aave.createReserve AaveMock.aave params)) startParams)

initialUsers :: AssocMap.Map (AssetClass, PubKeyHash) Aave.UserConfig
initialUsers = AssocMap.empty

initialFunds :: Value
initialFunds = lovelaceValueOf 1000000 <> mconcat ((`assetClassValue` 1000) <$> defaultAssets)

startParams :: [Aave.CreateParams]
startParams = fmap Aave.CreateParams defaultAssets

startContract :: Contract () Aave.AaveOwnerSchema Text ()
startContract = void $ AaveMock.start startParams

userContract :: Contract (Last (ContractResponse Text Aave.UserContractState)) Aave.AaveUserSchema Void ()
userContract = void $ Aave.userEndpoints AaveMock.aave

startTrace :: Trace.EmulatorTrace ()
startTrace = do
  _ <- Trace.activateContractWallet ownerWallet startContract
  _ <- Trace.waitNSlots 5
  pure ()

type UserHandle = Trace.ContractHandle (Last (ContractResponse Text Aave.UserContractState)) Aave.AaveUserSchema Void

initTrace :: Trace.EmulatorTrace (Map.Map Wallet UserHandle)
initTrace = do
    _ <- startTrace
    _ <- Trace.activateContractWallet ownerWallet $ initContract userWallets defaultAssets
    _ <- Trace.waitNSlots 5
    fmap Map.fromList $ forM userWallets $ \wallet -> do
        handle <- Trace.activateContractWallet wallet userContract
        pure (wallet, handle)
