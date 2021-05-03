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
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Main
    ( main
    ) where

import           Control.Monad                            (forM, forM_, void, when)
import           Control.Monad.Freer                      (Eff, Member, reinterpret, type (~>))
import           Control.Monad.Freer.Extras.Log           (LogMsg,logDebug)
import           Control.Monad.IO.Class                   (MonadIO (..))
import           Data.Bifunctor                           (Bifunctor (first))
import qualified Data.Map.Strict                          as Map
import qualified Data.Monoid                              as Monoid
import           Data.Row
import qualified Data.Semigroup                           as Semigroup
import           Data.Text                                (Text,pack)
import           Data.Text.Extras                         (tshow)
import           Data.Text.Prettyprint.Doc                (Pretty (..), viaShow)
import           GHC.Generics                             (Generic)
import           Ledger
import           Ledger.Ada
import           Ledger.Constraints
import           Ledger.Value                             as Value
import           Playground.Schema                        (endpointsToSchemas)
import Plutus.Contract
    ( Contract,
      tell,
      awaitTxConfirmed,
      ownPubKey,
      submitTx )
import qualified Plutus.Contracts.Currency                as Currency
import qualified Plutus.Contracts.LendingPool                 as LP
import           Plutus.PAB.Effects.Contract              (ContractEffect (..), PABContract (..))
-- import           Plutus.PAB.Effects.Contract.ContractTest (doContractInit, doContractUpdate)
import           Plutus.PAB.Events.Contract               (ContractPABRequest)
import           Plutus.PAB.Events.ContractInstanceState  (PartiallyDecodedResponse)
import           Plutus.PAB.Monitoring.PABLogMsg          (ContractEffectMsg (..))
import           Plutus.PAB.Simulator                     (SimulatorContractHandler, SimulatorEffectHandlers)
import qualified Plutus.PAB.Simulator                     as Simulator
import           Plutus.PAB.Types                         (PABError (..))
import qualified Plutus.PAB.Webserver.Server              as PAB.Server
import           Prelude                                  hiding (init)
import           Wallet.Emulator.Types                    (Wallet (..), walletPubKey)
import qualified Plutus.PAB.Events.Contract                  as C
import qualified Data.Aeson                                  as JSON
import qualified Data.Aeson.Types                            as JSON
import           Plutus.Contract.State                       (ContractRequest (..), ContractResponse (..))
import qualified Plutus.Contract.State                       as ContractState
import           Plutus.Contract.Schema                      (Event, Handlers, Input, Output)
import           Plutus.Contract.Resumable                   (Response)
import qualified Plutus.PAB.Events.ContractInstanceState as C
import           Control.Monad.Freer.Error                   (Error, throwError)

main :: IO ()
main = void $ Simulator.runSimulationWith handlers $ do
    Simulator.logString @AavePabServer "Starting AavePabServer PAB webserver on port 8080. Press enter to exit."
    shutdown <- PAB.Server.startServerDebug

    cidInit  <- Simulator.activateContract (Wallet 1) Init
    cs       <- flip Simulator.waitForState cidInit $ \json -> case JSON.fromJSON json of
                    JSON.Success (Just (Semigroup.Last cur)) -> Just $ Currency.currencySymbol cur
                    _                                   -> Nothing
    _        <- Simulator.waitUntilFinished cidInit
    Simulator.logString @AavePabServer $ "Initialization finished. Minted: " ++ show cs
    let coins = Map.fromList [(tn, LP.Coin cs tn) | tn <- tokenNames]
        ada   = LP.Coin adaSymbol adaToken

    cidStart <- Simulator.activateContract (Wallet 1) AaveStart
    us       <- flip Simulator.waitForState cidStart $ \json -> case (JSON.fromJSON json :: JSON.Result (Monoid.Last (Either Text LP.Aave))) of
                    JSON.Success (Monoid.Last (Just (Right us))) -> Just us
                    _                                       -> Nothing
    Simulator.logString @AavePabServer $ "AavePabServer instance created: " ++ show us

    cids <- fmap Map.fromList $ forM wallets $ \w -> do
        cid <- Simulator.activateContract w $ AaveUser us
        Simulator.logString @AavePabServer $ "AavePabServer user contract started for " ++ show w
        return (w, cid)

    let cp = 100000 :: Integer
    Simulator.logString @AavePabServer $ "creating liquidity pool: " ++ show (JSON.encode cp)
    _  <- Simulator.callEndpointOnInstance (cids Map.! Wallet 2) "create" cp
    flip Simulator.waitForState (cids Map.! Wallet 2) $ \json -> case (JSON.fromJSON json :: JSON.Result (Monoid.Last (Either Text LP.UserContractState))) of
        JSON.Success (Monoid.Last (Just (Right LP.Created))) -> Just ()
        _                                                    -> Nothing
    Simulator.logString @AavePabServer "liquidity pool created"

    _ <- liftIO getLine
    shutdown

data AavePabServer

data AaveContracts =
      Init
    | AaveStart
    | AaveUser LP.Aave
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (JSON.FromJSON, JSON.ToJSON)

instance PABContract AavePabServer where
    type ContractDef AavePabServer = AaveContracts
    type State AavePabServer = PartiallyDecodedResponse ContractPABRequest
    serialisableState _ = id

instance Pretty AaveContracts where
    pretty = viaShow

handleAaveContract ::
    ( Member (Error PABError) effs
    , Member (LogMsg ContractEffectMsg) effs
    )
    => ContractEffect AavePabServer
    ~> Eff effs
handleAaveContract = \case
    InitialState c -> case c of
        Init           -> doContractInit init
        AaveStart   -> doContractInit start
        AaveUser us -> doContractInit $ userEndpoints us
    UpdateContract c state p -> case c of
        Init           -> doContractUpdate init               state p
        AaveStart   -> doContractUpdate start              state p
        AaveUser us -> doContractUpdate (userEndpoints us) state p
    ExportSchema t -> case t of
        Init          -> pure $ endpointsToSchemas @Empty
        AaveStart  -> pure $ endpointsToSchemas @Empty
        AaveUser _ -> pure $ endpointsToSchemas @Empty
            -- TODO:
            -- replace with (Marlowe.MarloweSchema .\\ BlockchainActions)
            -- (needs some instances for the Marlowe types (MarloweParams, etc))
    where
        init          = first tshow initContract
        start         = first tshow LP.ownerEndpoint
        userEndpoints = first tshow . LP.userEndpoints

handlers :: SimulatorEffectHandlers AavePabServer
handlers = Simulator.mkSimulatorHandlers @AavePabServer [] mlw where
    mlw :: SimulatorContractHandler AavePabServer
    mlw =
        Simulator.handleContractEffectMsg @AavePabServer
        . reinterpret handleAaveContract

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

doContractInit ::
    forall w schema effs.
    ( Member (Error PABError) effs
    , Forall (Output schema) JSON.ToJSON
    , Forall (Input schema) JSON.ToJSON
    , Monoid w
    , JSON.ToJSON w
    )
    => Contract w schema Text ()
    -> Eff effs (PartiallyDecodedResponse ContractPABRequest)
doContractInit contract = either throwError pure $ do
    let value = ContractState.initialiseContract contract
    fromString $ fmap (fmap C.unContractHandlerRequest) $ JSON.eitherDecode $ JSON.encode value

doContractUpdate ::
    forall w schema effs.
    ( Member (Error PABError) effs
    , AllUniqueLabels (Input schema)
    , Forall (Input schema) JSON.FromJSON
    , Forall (Input schema) JSON.ToJSON
    , Forall (Output schema) JSON.ToJSON
    , Member (LogMsg ContractEffectMsg) effs
    , Monoid w
    , JSON.ToJSON w
    )
    => Contract w schema Text ()
    -> PartiallyDecodedResponse ContractPABRequest
    -> Response C.ContractResponse
    -> Eff effs (PartiallyDecodedResponse ContractPABRequest)
doContractUpdate contract oldState response = do
    let C.PartiallyDecodedResponse{..} = oldState
    oldState' <- traverse fromJSON newState
    typedResp <- traverse (fromJSON . JSON.toJSON . C.ContractHandlersResponse) response
    let conReq = ContractRequest{oldState = oldState', event = typedResp }
    logDebug $ SendContractRequest (fmap JSON.toJSON conReq)
    let response' = mkResponse $ ContractState.insertAndUpdateContract contract conReq
    logDebug $ ReceiveContractResponse response'
    pure response'

fromString :: Either String a -> Either PABError a
fromString = first (ContractCommandError 0 . pack)

mkResponse ::
    forall w schema err.
    ( Forall (Output schema) JSON.ToJSON
    , Forall (Input schema) JSON.ToJSON
    , JSON.ToJSON err
    , JSON.ToJSON w
    )
    => ContractResponse w err (Event schema) (Handlers schema)
    -> PartiallyDecodedResponse ContractPABRequest
mkResponse ContractResponse{newState, hooks, logs, observableState, err} =
    C.PartiallyDecodedResponse
        { C.newState = fmap JSON.toJSON newState
        , C.hooks    = fmap (fmap (encodeRequest @schema)) hooks
        , C.logs     = logs
        , C.observableState = JSON.toJSON observableState
        , C.err = fmap JSON.toJSON err
        }

encodeRequest ::
    forall schema.
    ( Forall (Output schema) JSON.ToJSON
    )
    => Handlers schema
    -> ContractPABRequest
encodeRequest = either error C.unContractHandlerRequest . JSON.eitherDecode . JSON.encode

fromJSON :: (Member (Error PABError) effs, JSON.FromJSON a) => JSON.Value -> Eff effs a
fromJSON =
    either (throwError . OtherError . pack) pure
    . JSON.parseEither JSON.parseJSON
