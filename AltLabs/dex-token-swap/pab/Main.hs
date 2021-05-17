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

module Main
    ( main
    ) where

import           Control.Monad                            (forM, forM_, void, when)
import           Control.Monad.Freer                      (Eff, Member, reinterpret, type (~>))
import           Control.Monad.Freer.Error                (Error)
import           Control.Monad.Freer.Extras.Log           (LogMsg)
import           Control.Monad.IO.Class                   (MonadIO (..))
import           Data.Aeson                               (FromJSON, Result (..), ToJSON, encode, fromJSON)
import           Data.Bifunctor                           (Bifunctor (first))
import qualified Data.Map.Strict                          as Map
import qualified Data.Monoid                              as Monoid
import           Data.Row
import qualified Data.Semigroup                           as Semigroup
import           Data.Text                                (Text)
import           Data.Text.Extras                         (tshow)
import           Data.Text.Prettyprint.Doc                (Pretty (..), viaShow)
import           GHC.Generics                             (Generic)
import           Ledger
import           Ledger.Ada
import           Ledger.Constraints
import           Ledger.Value                             as Value
import           Playground.Schema                        (endpointsToSchemas)
import           Plutus.Contract                          hiding (when)
import qualified Plutus.Contracts.Currency                as Currency
import qualified Plutus.Contracts.Uniswap                 as Uniswap
import           Plutus.PAB.Effects.Contract              (ContractEffect (..), PABContract (..))
import           Plutus.PAB.Effects.Contract.ContractTest (doContractInit, doContractUpdate)
import           Plutus.PAB.Events.Contract               (ContractPABRequest)
import           Plutus.PAB.Events.ContractInstanceState  (PartiallyDecodedResponse)
import           Plutus.PAB.Monitoring.PABLogMsg          (ContractEffectMsg (..))
import           Plutus.PAB.Simulator                     (SimulatorContractHandler, SimulatorEffectHandlers)
import qualified Plutus.PAB.Simulator                     as Simulator
import           Plutus.PAB.Types                         (PABError (..))
import qualified Plutus.PAB.Webserver.Server              as PAB.Server
import           Prelude                                  hiding (init)
import           Wallet.Emulator.Types                    (Wallet (..), walletPubKey)

main :: IO ()
main = void $ Simulator.runSimulationWith handlers $ do
    Simulator.logString @Uniswap "Starting Uniswap PAB webserver on port 8080. Press enter to exit."
    shutdown <- PAB.Server.startServerDebug

    cidInit  <- Simulator.activateContract (Wallet 1) Init
    cs       <- flip Simulator.waitForState cidInit $ \json -> case fromJSON json of
                    Success (Just (Semigroup.Last cur)) -> Just $ Currency.currencySymbol cur
                    _                                   -> Nothing
    _        <- Simulator.waitUntilFinished cidInit
    Simulator.logString @Uniswap $ "Initialization finished. Minted: " ++ show cs
    let coins = Map.fromList [(tn, Uniswap.Coin cs tn) | tn <- tokenNames]
        ada   = Uniswap.Coin adaSymbol adaToken

    cidStart <- Simulator.activateContract (Wallet 1) UniswapStart
    us       <- flip Simulator.waitForState cidStart $ \json -> case (fromJSON json :: Result (Monoid.Last (Either Text Uniswap.Uniswap))) of
                    Success (Monoid.Last (Just (Right us))) -> Just us
                    _                                       -> Nothing
    Simulator.logString @Uniswap $ "Uniswap instance created: " ++ show us

    cids <- fmap Map.fromList $ forM wallets $ \w -> do
        cid <- Simulator.activateContract w $ UniswapUser us
        Simulator.logString @Uniswap $ "Uniswap user contract started for " ++ show w
        _ <- Simulator.callEndpointOnInstance cid "funds" ()
        v <- flip Simulator.waitForState cid $ \json -> case (fromJSON json :: Result (Monoid.Last (Either Text Uniswap.UserContractState))) of
                Success (Monoid.Last (Just (Right (Uniswap.Funds v)))) -> Just v
                _                                                      -> Nothing
        Simulator.logString @Uniswap $ "initial funds in wallet " ++ show w ++ ": " ++ show v
        return (w, cid)

    let cp = Uniswap.CreateParams ada (coins Map.! "A") 100000 500000
    Simulator.logString @Uniswap $ "creating liquidity pool: " ++ show (encode cp)
    _  <- Simulator.callEndpointOnInstance (cids Map.! Wallet 2) "create" cp
    flip Simulator.waitForState (cids Map.! Wallet 2) $ \json -> case (fromJSON json :: Result (Monoid.Last (Either Text Uniswap.UserContractState))) of
        Success (Monoid.Last (Just (Right Uniswap.Created))) -> Just ()
        _                                                    -> Nothing
    Simulator.logString @Uniswap "liquidity pool created"

    _ <- liftIO getLine
    shutdown

data Uniswap

data UniswapContracts =
      Init
    | UniswapStart
    | UniswapUser Uniswap.Uniswap
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

instance PABContract Uniswap where
    type ContractDef Uniswap = UniswapContracts
    type State Uniswap = PartiallyDecodedResponse ContractPABRequest
    serialisableState _ = id

instance Pretty UniswapContracts where
    pretty = viaShow

handleUniswapContract ::
    ( Member (Error PABError) effs
    , Member (LogMsg ContractEffectMsg) effs
    )
    => ContractEffect Uniswap
    ~> Eff effs
handleUniswapContract = \case
    InitialState c -> case c of
        Init           -> doContractInit init
        UniswapStart   -> doContractInit start
        UniswapUser us -> doContractInit $ userEndpoints us
    UpdateContract c state p -> case c of
        Init           -> doContractUpdate init               state p
        UniswapStart   -> doContractUpdate start              state p
        UniswapUser us -> doContractUpdate (userEndpoints us) state p
    ExportSchema t -> case t of
        Init          -> pure $ endpointsToSchemas @Empty
        UniswapStart  -> pure $ endpointsToSchemas @Empty
        UniswapUser _ -> pure $ endpointsToSchemas @Empty
            -- TODO:
            -- replace with (Marlowe.MarloweSchema .\\ BlockchainActions)
            -- (needs some instances for the Marlowe types (MarloweParams, etc))
    where
        init          = first tshow initContract
        start         = first tshow Uniswap.ownerEndpoint
        userEndpoints = first tshow . Uniswap.userEndpoints

handlers :: SimulatorEffectHandlers Uniswap
handlers = Simulator.mkSimulatorHandlers @Uniswap [] mlw where
    mlw :: SimulatorContractHandler Uniswap
    mlw =
        Simulator.handleContractEffectMsg @Uniswap
        . reinterpret handleUniswapContract

initContract :: Contract (Maybe (Semigroup.Last Currency.Currency)) Currency.CurrencySchema Currency.CurrencyError ()
initContract = do
    ownPK <- pubKeyHash <$> ownPubKey
    cur   <- Currency.forgeContract ownPK [(tn, fromIntegral (length wallets) * amount) | tn <- tokenNames]
    let cs = Currency.currencySymbol cur
        v  = mconcat [Value.singleton cs tn amount | tn <- tokenNames]
    forM_ wallets $ \w -> do
        let pkh = pubKeyHash $ walletPubKey w
        when (pkh /= ownPK) $ do
            tx <- submitTx $ mustPayToPubKey pkh v
            awaitTxConfirmed $ txId tx
    tell $ Just $ Semigroup.Last cur
  where
    amount = 1000000

wallets :: [Wallet]
wallets = [Wallet i | i <- [1 .. 4]]

tokenNames :: [TokenName]
tokenNames = ["A", "B", "C", "D"]