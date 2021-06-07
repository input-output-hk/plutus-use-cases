{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}

module Plutus.PAB.Simulation
    ( runLendingPoolSimulation,
      AaveContracts(..)
    ) where

import           Control.Monad                       (forM, forM_, void, when)
import           Control.Monad.Freer                 (Eff, Member, interpret,
                                                      type (~>))
import           Control.Monad.Freer.Error           (Error)
import           Control.Monad.Freer.Extras.Log      (LogMsg)
import           Control.Monad.IO.Class              (MonadIO (..))
import           Data.Aeson                          (FromJSON, Result (..),
                                                      ToJSON, encode, fromJSON)
import qualified Data.ByteString                     as BS
import qualified Data.Map.Strict                     as Map
import qualified Data.Monoid                         as Monoid
import qualified Data.Semigroup                      as Semigroup
import           Data.Text                           (Text)
import           Data.Text.Prettyprint.Doc           (Pretty (..), viaShow)
import           GHC.Generics                        (Generic)
import           Ledger
import           Ledger.Ada                          (adaSymbol, adaToken,
                                                      adaValueOf,
                                                      lovelaceValueOf)
import           Ledger.Constraints
import qualified Ledger.Constraints.OffChain         as Constraints
import qualified Ledger.Typed.Scripts                as Scripts
import           Ledger.Value                        as Value
import           Plutus.Contract                     hiding (when)
import qualified Plutus.Contracts.Core               as Aave
import           Plutus.Contracts.Currency           as Currency
import qualified Plutus.Contracts.Endpoints          as Aave
import qualified Plutus.Contracts.FungibleToken      as FungibleToken
import           Plutus.PAB.Effects.Contract         (ContractEffect (..))
import           Plutus.PAB.Effects.Contract.Builtin (Builtin, SomeBuiltin (..),
                                                      type (.\\))
import qualified Plutus.PAB.Effects.Contract.Builtin as Builtin
import           Plutus.PAB.Monitoring.PABLogMsg     (PABMultiAgentMsg)
import           Plutus.PAB.Simulator                (SimulatorEffectHandlers)
import qualified Plutus.PAB.Simulator                as Simulator
import           Plutus.PAB.Types                    (PABError (..))
import qualified Plutus.PAB.Webserver.Server         as PAB.Server
import           Plutus.V1.Ledger.Crypto             (getPubKeyHash, pubKeyHash)
import           Prelude                             hiding (init)
import           Wallet.Emulator.Types               (Wallet (..), walletPubKey)

wallets :: [Wallet]
wallets = [Wallet i | i <- [1 .. 4]]

testCurrencyNames :: [TokenName]
testCurrencyNames = ["MOGUS", "USD"]

toAsset :: TokenName -> AssetClass
toAsset tokenName =
   assetClass (scriptCurrencySymbol . FungibleToken.makeLiquidityPolicy $ tokenName) tokenName

testAssets :: [AssetClass]
testAssets = fmap toAsset testCurrencyNames

initContract :: Contract (Maybe (Semigroup.Last Currency.OneShotCurrency)) Currency.CurrencySchema Currency.CurrencyError ()
initContract = do
    ownPK <- pubKeyHash <$> ownPubKey
    let testCurrenciesValue = mconcat $ fmap (`assetClassValue` 1000) testAssets
        policyLookups = mconcat $
            fmap (Constraints.monetaryPolicy . FungibleToken.makeLiquidityPolicy . Prelude.snd . unAssetClass) testAssets
        adaValue = lovelaceValueOf amount
    forM_ wallets $ \w -> do
        let pkh = pubKeyHash $ walletPubKey w
            lookups = policyLookups
            tx = mustForgeValue testCurrenciesValue <> mustPayToPubKey pkh (adaValue <> testCurrenciesValue)
        when (pkh /= ownPK) $ do
            ledgerTx <- submitTxConstraintsWith @Scripts.Any lookups tx
            void $ awaitTxConfirmed $ txId ledgerTx
  where
    amount = 1000000

runLendingPoolSimulation :: IO ()
runLendingPoolSimulation = void $ Simulator.runSimulationWith handlers $ do
    Simulator.logString @(Builtin AaveContracts) "Starting Aave PAB webserver on port 8080. Press enter to exit."
    shutdown <- PAB.Server.startServerDebug

    cidInit  <- Simulator.activateContract (Wallet 1) Init
    _        <- Simulator.waitUntilFinished cidInit

    Simulator.logString @(Builtin AaveContracts) "Initialization finished."

    let params = fmap Aave.CreateParams testAssets
    cidStart <- Simulator.activateContract (Wallet 1) (AaveStart params)
    aa       <- flip Simulator.waitForState cidStart $ \json -> case (fromJSON json :: Result (Monoid.Last (Either Text Aave.Aave))) of
                    Success (Monoid.Last (Just (Right aa))) -> Just aa
                    _                                       -> Nothing
    Simulator.logString @(Builtin AaveContracts) $ "Aave instance created: " ++ show aa

    cids <- fmap Map.fromList $ forM wallets $ \w -> do
        cid <- Simulator.activateContract w $ AaveUser aa
        Simulator.logString @(Builtin AaveContracts) $ "Aave user contract started for " ++ show w
        return (w, cid)

    _ <- liftIO getLine
    shutdown

data AaveContracts =
      Init
    | AaveStart [Aave.CreateParams]
    | AaveUser Aave.Aave
    deriving (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

instance Pretty AaveContracts where
    pretty = viaShow

handleAaveContract ::
    ( Member (Error PABError) effs
    , Member (LogMsg (PABMultiAgentMsg (Builtin AaveContracts))) effs
    )
    => ContractEffect (Builtin AaveContracts)
    ~> Eff effs
handleAaveContract = Builtin.handleBuiltin getSchema getContract where
  getSchema = \case
    AaveUser _ -> Builtin.endpointsToSchemas @(Aave.AaveUserSchema .\\ BlockchainActions)
    AaveStart _  -> Builtin.endpointsToSchemas @(Aave.AaveOwnerSchema .\\ BlockchainActions)
    Init          -> Builtin.endpointsToSchemas @Empty
  getContract = \case
    AaveUser us      -> SomeBuiltin $ Aave.userEndpoints us
    AaveStart params -> SomeBuiltin $ Aave.ownerEndpoint params
    Init             -> SomeBuiltin initContract

handlers :: SimulatorEffectHandlers (Builtin AaveContracts)
handlers =
    Simulator.mkSimulatorHandlers @(Builtin AaveContracts) []
    $ interpret handleAaveContract
