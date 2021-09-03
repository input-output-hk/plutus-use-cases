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
{-# LANGUAGE ScopedTypeVariables #-}
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
import           Plutus.PAB.Effects.Contract.Builtin (Builtin, SomeBuiltin (..), type (.\\), endpointsToSchemas, handleBuiltin, HasDefinitions (getDefinitions, getContract, getSchema), BuiltinHandler (contractHandler))
import           Plutus.PAB.Monitoring.PABLogMsg     (PABMultiAgentMsg)
import           Plutus.PAB.Simulator                (SimulatorEffectHandlers, callEndpointOnInstance, logString, SimulatorState, SimulatorContractHandler, mkSimulatorHandlers)
import qualified Plutus.PAB.Simulator                as Simulator
import           Plutus.PAB.Types                    (PABError (..))
import qualified Plutus.PAB.Webserver.Server         as PAB.Server
import           Data.Default                             (Default (def))
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
import Wallet.Emulator.Wallet (Entity (WalletEntity))
import qualified Wallet.Emulator.Wallet as Wallet
import GHC.Conc (atomically)
import Ledger.Ada (adaToken,adaSymbol)
import Prelude
import Schema (FormSchema)
import Playground.Contract (FunctionSchema)
import Plutus.PAB.Core
import qualified Ledger.Fee

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

simpleMarket :: Contract [AesonTypes.Value ] MarketPlatformSchema Text ()
simpleMarket =awaitPromise (marketEndpoints defaultMarket)
  >> awaitPromise utilEndpoints

instance HasDefinitions Market where
    getDefinitions =[defaultMarket]
    getContract market = SomeBuiltin   simpleMarket
    getSchema market = endpointsToSchemas @MarketPlatformSchema


main :: IO ()
main = void $ Simulator.runSimulationWith simulatorHandlers $ do
    shutdown <- PAB.Server.startServerDebug
    forM_ wallets wInit
            -- cid <- Simulator.activateContract w  defaultMarket
            -- liftIO $ writeFile ('W' : show (getWallet w) ++ ".cid") $ show $ unContractInstanceId cid
    Simulator.logString @(Builtin Market) "Press enter to view Balances."
    loop
  where
    loop= printOnReturn >>loop

    wInit w = Simulator.activateContract w  defaultMarket

    printOnReturn= do
        void $ liftIO getLine
        slot <- Simulator.currentSlot
        slot<-  liftIO $ atomically slot
        b<-Simulator.currentBalances
        Simulator.logString  @(Builtin Market) $ "---------------------------------------------------------------------"
        Simulator.logString @(Builtin Market) $  "-------  Balances at SlotNo :" ++ (show $ getSlot slot)
        Simulator.logString  @(Builtin Market) $ "---------------------------------------------------------------------"
        logBalances @(Builtin Market) b

wallets :: [Wallet]
wallets = [Wallet i | i <- [1 .. 5]]


handleMarketContracts = Plutus.PAB.Effects.Contract.Builtin.handleBuiltin  @Market


simulatorHandlers :: EffectHandlers (Builtin Market ) (SimulatorState (Builtin Market ))
simulatorHandlers = mkSimulatorHandlers def def handler where
  handler :: SimulatorContractHandler (Builtin Market)
  handler = interpret (contractHandler handleBuiltin)



logBalances :: forall t effs. Member (LogMsg (PABMultiAgentMsg t)) effs
            => Map.Map Wallet.Entity Value
            -> Eff effs ()
logBalances bs = do
    forM_ (Map.toList bs) $ \(e, v) -> do
        logString @t $ showEntity e <> ": "
        forM_ (Value.flattenValue v) $ \(cs, tn, a) ->
            if cs==adaSymbol && tn == adaToken  then
              logString @t $ "           " <> prettyShow (a `divMod` 1_000_000)
            else
              logString @t $ "    {" <> show cs <> ", " <> show tn <> "}: " <> show a
  where
  prettyShow (l,s)=(
      if s==0 then show l
      else show l ++ "."++show s
    ) ++ " Ada"
  showEntity e= case e of
    WalletEntity w ->  show w++ " (pkh: " ++(show $ pubKeyHash $ walletPubKey w)++" )"
    _ -> show e
