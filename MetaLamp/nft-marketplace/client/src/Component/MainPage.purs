module Component.MainPage where

import Data.Unit
import Prelude
import Business.Marketplace as Marketplace
import Business.MarketplaceInfo as MarketplaceInfo
import Capability.LogMessages (class LogMessages, logInfo)
import Capability.PollContract (class PollContract)
import Component.Utils (runRD)
import Component.WalletSelector as WalletSelector
import Control.Monad.Except (lift, runExceptT, throwError)
import Control.Parallel (parTraverse)
import Data.Array (catMaybes, findMap, groupBy, mapWithIndex, take)
import Data.Array.NonEmpty as NEA
import Data.BigInteger (BigInteger)
import Data.Either (either)
import Data.Json.JsonTuple (JsonTuple(..))
import Data.Lens (Lens')
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties (classes)
import Network.RemoteData (RemoteData(..))
import Network.RemoteData as RD
import Network.RemoteData as RemoteData
import Plutus.PAB.Simulation (MarketplaceContracts)
import Plutus.PAB.Webserver.Types (ContractInstanceClientState)
import Plutus.V1.Ledger.Crypto (PubKeyHash)
import Plutus.V1.Ledger.Value (AssetClass, Value)
import PlutusTx.AssocMap as Map
import Utils.BEM as BEM
import View.RemoteDataState (remoteDataState)

type Slots
  = ( walletSelector :: WalletSelector.ButtonSlot Unit )

data Action
  = ChooseWallet WalletSelector.Output

component ::
  forall m query input output.
  LogMessages m =>
  PollContract m =>
  H.Component HH.HTML query input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
  where
  initialState :: forall i. i -> Unit
  initialState _ = unit

  render :: forall state. state -> H.ComponentHTML Action Slots m
  render _ =
    HH.div_
      [ HH.text "Choose wallet: "
      , HH.slot WalletSelector._walletSelector unit WalletSelector.component unit (Just <<< ChooseWallet)
      ]

  handleAction :: Action -> H.HalogenM Unit Action Slots output m Unit
  handleAction = case _ of
    ChooseWallet (WalletSelector.Submit w) -> do
      logInfo $ show w
      pure unit
