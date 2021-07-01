{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

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
import Data.Aeson
    ( FromJSON, Result(..), fromJSON, FromJSON, ToJSON )
import           Data.Monoid                         (Last (..))
import           Data.Text                           (Text, pack)
import           Ledger hiding(fee)
import           Ledger.Constraints
import qualified Ledger.Value                        as Value
import           Plutus.Contract                     hiding (when)
import           Plutus.PAB.Effects.Contract         (ContractEffect (..))
import           Plutus.PAB.Effects.Contract.Builtin (Builtin, SomeBuiltin (..), type (.\\), endpointsToSchemas, handleBuiltin)
import           Plutus.PAB.Monitoring.PABLogMsg     (PABMultiAgentMsg)
import           Plutus.PAB.Simulator                (SimulatorEffectHandlers, callEndpointOnInstance, logString)
import qualified Plutus.PAB.Simulator                as Simulator
import           Plutus.PAB.Types                    (PABError (..))
import qualified Plutus.PAB.Webserver.Server         as PAB.Server

import           Wallet.Emulator.Types               (Wallet (..), walletPubKey)
import           Wallet.Types                        (ContractInstanceId (..))
import           Data.Text.Prettyprint.Doc (Pretty (..), viaShow)

import  Plutus.Contract.Wallet.MarketEndpoints
import  Plutus.Contract.Wallet.MarketPlace
import Plutus.Contract.Wallet.Nft
import Plutus.Contract.Wallet.Utils (UtilSchema,utilEndpoints)
import qualified Plutus.Contract.Wallet as MWallet
import qualified Data.Aeson.Types as AesonTypes
import Data.ByteString (ByteString)
import qualified Data.Map as Map
import Wallet.Emulator.Wallet (Entity)


defaultMarket :: Market
defaultMarket = Market{
  mOperator   = pubKeyHash (walletPubKey (Wallet 10)),
  mAuctionFee =5_000_000 ,  -- 5%
  mPrimarySaleFee =5_000_000, -- 5%
  mSecondarySaleFee=2_500_000 -- 2.5%
}

instance Pretty Market where
    pretty = viaShow

type MarketPlatformSchema=
   NftSchema
  .\/ UtilSchema
  .\/ MarketSchema

endpoints :: Contract [AesonTypes.Value] MarketPlatformSchema Text ()
endpoints=doSelection  >> endpoints
  where
    doSelection=
      marketEndpoints defaultMarket
      `select` nftEndpoints
      `select` utilEndpoints


main :: IO ()
main = void $ Simulator.runSimulationWith handlers $ do
    Simulator.logString @(Builtin Market) "Press enter to exit."
    shutdown <- PAB.Server.startServerDebug


    forM_ wallets wInit 
            -- cid <- Simulator.activateContract w  defaultMarket
            -- liftIO $ writeFile ('W' : show (getWallet w) ++ ".cid") $ show $ unContractInstanceId cid
    loop
  where
    loop= printOnReturn >>loop

    wInit w = Simulator.activateContract w  defaultMarket


    printOnReturn= do
        void $ liftIO getLine
        slot <- Simulator.currentSlot
        Simulator.logString  @(Builtin Market) $ "--------------------------------"
        Simulator.logString  @(Builtin Market) $ "--------------------------------"
        Simulator.logString @(Builtin Market) "Current Balances"
        b <- Simulator.currentBalances
        Simulator.logBalances @(Builtin Market) b 
wallets :: [Wallet]
wallets = [Wallet i | i <- [1 .. 5]]

handleMarketContracts ::
    ( Member (Error PABError) effs
    , Member (LogMsg (PABMultiAgentMsg (Builtin Market))) effs
    )
    => ContractEffect (Builtin Market)
    ~> Eff effs
handleMarketContracts = handleBuiltin getSchema getContract where
    getSchema =  \ _ -> endpointsToSchemas @MarketPlatformSchema
    getContract   m =  SomeBuiltin   endpoints

handlers :: SimulatorEffectHandlers (Builtin Market)
handlers =
    Simulator.mkSimulatorHandlers @(Builtin Market) []
    $ interpret handleMarketContracts
