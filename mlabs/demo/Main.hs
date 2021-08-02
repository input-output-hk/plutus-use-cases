{-# OPTIONS_GHC -Wno-missing-import-lists #-}

module Main (main) where

--------------------------------------------------------------------------------

import GHC.Generics
import Prelude

--------------------------------------------------------------------------------

import Control.Monad (forM, forM_, void, when)
import Control.Monad.Freer (Eff, Member, interpret, reinterpret, type (~>))
import Control.Monad.Freer.Error (Error, throwError)
import Control.Monad.Freer.Extras.Log (LogMsg, logDebug)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson (FromJSON, Result (..), ToJSON, encode, fromJSON)
import Data.Bifunctor (Bifunctor (first))
import Data.Default.Class
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Row (type Empty, type (.\\))
import Data.Semigroup qualified as Semigroup
import Data.Text.Prettyprint.Doc (Pretty (..), viaShow)

--------------------------------------------------------------------------------

import Cardano.Prelude qualified as Cardano
import Cardano.Wallet.Types qualified (WalletInfo (..))
import Control.Concurrent.Availability qualified as Availability
import Plutus.Contract qualified as Contract
import Plutus.Contract.Effects.ExposeEndpoint qualified as Cardano
import Plutus.Contract.Resumable (Response)
import Plutus.Contract.Schema (Event, Handlers, Input, Output)
import Plutus.Contract.State (Contract, ContractRequest (..), ContractResponse (..))
import Plutus.Contract.State qualified as Contract
import Plutus.PAB.Core qualified as PAB
import Plutus.PAB.Core.ContractInstance.STM qualified as Cardano
import Plutus.PAB.Effects.Contract (ContractEffect (..), PABContract (..))
import Plutus.PAB.Effects.Contract.Builtin (Builtin, SomeBuiltin (..))
import Plutus.PAB.Effects.Contract.Builtin qualified as Builtin
import Plutus.PAB.Events.Contract (ContractPABRequest)
import Plutus.PAB.Events.Contract qualified as Contract
import Plutus.PAB.Events.ContractInstanceState (PartiallyDecodedResponse)
import Plutus.PAB.Events.ContractInstanceState qualified as Contract
import Plutus.PAB.Monitoring.PABLogMsg (ContractEffectMsg (..), PABMultiAgentMsg (..))
import Plutus.PAB.Simulator (Simulation, SimulatorContractHandler, SimulatorEffectHandlers)
import Plutus.PAB.Simulator qualified as Simulator
import Plutus.PAB.Types (PABError (..), WebserverConfig (..))
import Plutus.PAB.Webserver.Server qualified as PAB
import Plutus.V1.Ledger.Ada qualified as Ada
import Plutus.V1.Ledger.Crypto qualified as Ledger
import Plutus.V1.Ledger.Slot qualified as Ledger (Slot (..))
import Plutus.V1.Ledger.Value qualified as Ledger
import Plutus.V1.Ledger.Value qualified as Value
import PlutusTx.Prelude qualified as PlutusTx
import PlutusTx.Prelude ((%))
import Wallet.Emulator.Types (Wallet (..), walletPubKey)
import Wallet.Emulator.Wallet qualified as Wallet

--------------------------------------------------------------------------------

import qualified Mlabs.Lending.Contract.Lendex as Lendex
import qualified Mlabs.Lending.Logic.Types as Lendex
import Mlabs.Lending.Logic.Types (Coin, UserAct(..), UserId(..), StartParams(..))

--------------------------------------------------------------------------------


main :: IO ()
main = void $
  Simulator.runSimulationWith handlers $ do
    shutdown <- PAB.startServerDebug

    cidInit <- Simulator.activateContract (Wallet 1) Init

    -- The initial spend is enough to identify the entire market, provided the initial params are also clear.
    -- TODO: get pool info here.
    _ <- flip Simulator.waitForState cidInit $ \json -> case fromJSON json of
      Success (Just (Semigroup.Last mkt)) -> Just mkt
      _ -> Nothing


    shutdown

data AavePAB

data AaveContracts
  = Init
  | User Lendex.LendingPool 
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance Pretty AaveContracts where
  pretty = viaShow

instance PABContract AavePAB where
  type ContractDef AavePAB = AaveContracts
  type State AavePAB = PartiallyDecodedResponse ContractPABRequest

  serialisableState _ = id

handleLendexContract ::

  ( Member (Error PABError) effs
  , Member (LogMsg (PABMultiAgentMsg (Builtin AaveContracts))) effs
  ) =>
  ContractEffect (Builtin AaveContracts)
    ~> Eff effs
handleLendexContract = Builtin.handleBuiltin getSchema getContract
  where
    getSchema = \case
      Init -> Builtin.endpointsToSchemas @Empty
      User _ -> Builtin.endpointsToSchemas @Lendex.UserLendexSchema
    getContract = \case
      Init -> SomeBuiltin (Lendex.startLendex startParams)
      User lendex -> SomeBuiltin (Lendex.userAction depositAct)

handlers :: SimulatorEffectHandlers (Builtin AaveContracts)
handlers =
  Simulator.mkSimulatorHandlers @(Builtin AaveContracts) [] $
    interpret handleLendexContract

startParams :: StartParams
startParams = StartParams
  { sp'coins     = [initCoinCfg]
  , sp'initValue =  initValue    -- ^ init value deposited to the lending app
  }

initValue :: Value.Value
initValue = Value.singleton Ada.adaSymbol Ada.adaToken 10000
             -- TODO: figure out how to support multiple currencies
             -- note: looks like we'll need a minimal minting contract to get currencies working, otherwise we can support Ada collateral, Ada borrow by removing `collateralNonBorrow uid asset` from the contract.
             -- <> Value.Singleton  () (Value.tokenName "USDc")

initCoinCfg = Lendex.CoinCfg
  { coinCfg'coin             = Value.AssetClass (Ada.adaSymbol, Ada.adaToken)
  , coinCfg'rate             = 1 % 1
  , coinCfg'aToken           = Value.tokenName "aAda"
  , coinCfg'interestModel    = Lendex.defaultInterestModel
  , coinCfg'liquidationBonus = 2 % 10
  }

depositAct = DepositAct
      { act'amount          = 100
      , act'asset           = Value.AssetClass (Ada.adaSymbol, Ada.adaToken)  
      }
-- --------------------------------------------------------------------------------
