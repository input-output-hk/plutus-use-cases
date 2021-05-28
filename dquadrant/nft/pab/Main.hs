{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}

{-# LANGUAGE DeriveGeneric #-}
module Main
    ( main
    ) where

import           Control.Monad                       (forM_, void, when)
import           GHC.Generics              (Generic)
import           Control.Monad.Freer                 (Eff, Member, interpret, type (~>))
import           Control.Monad.Freer.Error           (Error)
import           Control.Monad.Freer.Extras.Log      (LogMsg)
import           Control.Monad.IO.Class              (MonadIO (..))
import           Data.Aeson                          (FromJSON, Result (..), fromJSON)
import           Data.Monoid                         (Last (..))
import           Data.Text                           (Text, pack)
import           Ledger
import           Ledger.Constraints
import qualified Ledger.Value                        as Value
import           Plutus.Contract                     hiding (when)
import           Plutus.PAB.Effects.Contract         (ContractEffect (..))
import           Plutus.PAB.Effects.Contract.Builtin (Builtin, SomeBuiltin (..), type (.\\), endpointsToSchemas, handleBuiltin)
import           Plutus.PAB.Monitoring.PABLogMsg     (PABMultiAgentMsg)
import           Plutus.PAB.Simulator                (SimulatorEffectHandlers)
import qualified Plutus.PAB.Simulator                as Simulator
import           Plutus.PAB.Types                    (PABError (..))
import qualified Plutus.PAB.Webserver.Server         as PAB.Server

import           Wallet.Emulator.Types               (Wallet (..), walletPubKey)
import           Wallet.Types                        (ContractInstanceId (..))
import Data.Aeson (FromJSON, ToJSON)
import           Data.Text.Prettyprint.Doc (Pretty (..), viaShow)



import  Plutus.Contract.Blockchain.MarketPlace 



newtype MarketContract = MarketContract Market
    deriving (Eq, Show, Generic, FromJSON, ToJSON)

instance Pretty MarketContract where
    pretty = viaShow


main :: IO ()
main = void $ Simulator.runSimulationWith handlers $ do
    Simulator.logString @(Builtin MarketContract) "MarketPlace PAB Init  .......\n Press enter to exit."
    shutdown <- PAB.Server.startServerDebug


    forM_ wallets $ \w -> do
            cid <- Simulator.activateContract w $ MarketContract defaultMarket
            liftIO $ writeFile ('W' : show (getWallet w) ++ ".cid") $ show $ unContractInstanceId cid
    void $ liftIO getLine
    shutdown

waitForLast :: FromJSON a => ContractInstanceId -> Simulator.Simulation t a
waitForLast cid =
    flip Simulator.waitForState cid $ \json -> case fromJSON json of
        Success (Last (Just x)) -> Just x
        _                       -> Nothing

wallets :: [Wallet]
wallets = [Wallet i | i <- [1 .. 9]]

defaultMarket :: Market
defaultMarket = Market 
    {
    operator   = pubKeyHash (walletPubKey (Wallet 10))
    ,fee        =  1000 
    } 

handleMarketContracts ::
    ( Member (Error PABError) effs
    , Member (LogMsg (PABMultiAgentMsg (Builtin MarketContract))) effs
    )
    => ContractEffect (Builtin MarketContract)
    ~> Eff effs
handleMarketContracts = handleBuiltin getSchema getContract where
    getSchema =  \ _ -> endpointsToSchemas @Empty
    getContract  (MarketContract m) =  SomeBuiltin   (openTheMarket m)

handlers :: SimulatorEffectHandlers (Builtin MarketContract)
handlers =
    Simulator.mkSimulatorHandlers @(Builtin MarketContract) []
    $ interpret handleMarketContracts
