module Component.MainPage where

import Prelude
import Business.Aave as Aave
import Capability.Contract (ContractId(..))
import Capability.LogMessages (class LogMessages)
import Capability.PollContract (class PollContract)
import Component.Contract as Contract
import Component.Contract as ContractComponent
import Component.Utils (runRD)
import Control.Monad.Except (lift, runExceptT, throwError)
import Control.Parallel (parTraverse)
import Data.Array (groupBy, head, mapWithIndex, zip)
import Data.Array.NonEmpty as NEA
import Data.BigInteger (BigInteger)
import Data.Either (either)
import Data.Json.JsonTuple (JsonTuple(..))
import Data.Json.JsonUUID (JsonUUID(..))
import Data.Lens (Lens')
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Data.UUID (toString) as UUID
import Halogen as H
import Halogen.HTML as HH
import Network.RemoteData (RemoteData(..))
import Network.RemoteData as RD
import Network.RemoteData as RemoteData
import Plutus.Contracts.Core (Reserve(..), UserConfig)
import Plutus.PAB.Simulation (AaveContracts)
import Plutus.PAB.Webserver.Types (ContractInstanceClientState(..))
import Plutus.V1.Ledger.Crypto (PubKeyHash)
import Plutus.V1.Ledger.Value (AssetClass)
import PlutusTx.AssocMap as Map
import View.RemoteDataState (remoteDataState)
import View.ReserveInfo (reserveInfo)
import View.UsersTable (poolUsers)
import Wallet.Types (ContractInstanceId(..))

type State
  = { contracts :: RemoteData String (Array (ContractInstanceClientState AaveContracts))
    , pubKeys :: RemoteData String (Array PubKeyHash)
    , users :: RemoteData String (Map.Map (JsonTuple AssetClass PubKeyHash) UserConfig)
    , reserves :: RemoteData String (Map.Map AssetClass Reserve)
    }

_contracts :: Lens' State (RemoteData String (Array (ContractInstanceClientState AaveContracts)))
_contracts = prop (SProxy :: SProxy "contracts")

_pubKeys :: Lens' State (RemoteData String (Array PubKeyHash))
_pubKeys = prop (SProxy :: SProxy "pubKeys")

_users :: Lens' State (RemoteData String (Map.Map (JsonTuple AssetClass PubKeyHash) UserConfig))
_users = prop (SProxy :: SProxy "users")

_reserves :: Lens' State (RemoteData String (Map.Map AssetClass Reserve))
_reserves = prop (SProxy :: SProxy "reserves")

data Action
  = Init
  | GetContracts
  | GetPubKeys
  | GetUserConfigs
  | GetReserves
  | OnSubmitSuccess ContractComponent.Output

type Slots
  = ( contract :: forall query. H.Slot query ContractComponent.Output Int )

_contract = SProxy :: SProxy "contract"

initialState :: forall input. input -> State
initialState _ = { contracts: NotAsked, pubKeys: NotAsked, users: NotAsked, reserves: NotAsked }

component ::
  forall m query input output.
  LogMessages m =>
  PollContract m =>
  H.Component HH.HTML query input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction, initialize = Just Init }
    }
  where
  handleAction :: Action -> H.HalogenM State Action Slots output m Unit
  handleAction = case _ of
    Init -> do
      handleAction GetContracts
      handleAction GetPubKeys
      handleAction GetReserves
      handleAction GetUserConfigs
    OnSubmitSuccess _ -> do
      handleAction GetReserves
      handleAction GetUserConfigs
    GetContracts ->
      runRD _contracts <<< runExceptT
        $ lift Aave.getAaveContracts
        >>= either (throwError <<< show) pure
    GetPubKeys ->
      runRD _pubKeys <<< runExceptT
        $ do
            state <- lift H.get
            contracts <- RemoteData.maybe (throwError "No contracts found") pure state.contracts
            parTraverse
              ( \(ContractInstanceClientState { cicContract }) ->
                  lift (Aave.ownPubKey (toContractIdParam cicContract)) >>= either (throwError <<< show) pure
              )
              contracts
    GetUserConfigs ->
      runRD _users <<< runExceptT
        $ do
            state <- lift H.get
            contracts <-
              RD.maybe (throwError "contracts are missing") pure
                $ state.contracts
            case head contracts of
              Nothing -> throwError "No contract"
              Just (ContractInstanceClientState { cicContract }) -> lift (Aave.users (toContractIdParam cicContract)) >>= either (throwError <<< show) pure
    GetReserves ->
      runRD _reserves <<< runExceptT
        $ do
            state <- lift H.get
            contracts <-
              RD.maybe (throwError "contracts are missing") pure
                $ state.contracts
            case head contracts of
              Nothing -> throwError "No contract"
              Just (ContractInstanceClientState { cicContract }) -> lift (Aave.reserves (toContractIdParam cicContract)) >>= either (throwError <<< show) pure

  render :: State -> H.ComponentHTML Action Slots m
  render state =
    HH.div_
      [ remoteDataState
          ( \reserves ->
              HH.div_
                $ [ HH.h2_ [ HH.text "Pool funds" ] ]
                <> map (\(Tuple a r) -> reserveInfo a r) reserves
          )
          (map Map.toTuples state.reserves)
      , remoteDataState
          ( \userMap ->
              let
                getAsset (Tuple (JsonTuple (Tuple asset _)) _) = asset

                getName (Tuple (JsonTuple (Tuple _ pkh)) _) = show pkh

                getUser (Tuple _ user) = user

                usersByAsset = groupBy (\a b -> getAsset a == getAsset b) <<< Map.toTuples $ userMap

                html =
                  map
                    ( \users ->
                        poolUsers
                          (getAsset <<< NEA.head $ users)
                          (map (\user -> Tuple (getName user) (getUser user)) $ users)
                    )
                    usersByAsset
              in
                HH.div_
                  $ [ HH.h2_ [ HH.text "Users:" ], HH.div_ html ]
          )
          state.users
      , remoteDataState
          ( \{ contracts, pubKeys, reserves } ->
              HH.div_
                $ mapWithIndex
                    ( \index (Tuple (ContractInstanceClientState { cicContract }) walletPubKey) ->
                        HH.slot
                          _contract
                          index
                          Contract.component
                          ({ contractId: toContractIdParam cicContract, walletPubKey, reserves: toReserveInfo reserves })
                          (Just <<< OnSubmitSuccess)
                    )
                    (zip contracts pubKeys)
          )
          ({ contracts: _, pubKeys: _, reserves: _ } <$> state.contracts <*> state.pubKeys <*> state.reserves)
      ]

toContractIdParam :: ContractInstanceId -> ContractId
toContractIdParam (ContractInstanceId { unContractInstanceId: JsonUUID uuid }) = ContractId <<< UUID.toString $ uuid

toReserveInfo :: Map.Map AssetClass Reserve -> Array { amount :: BigInteger, asset :: AssetClass }
toReserveInfo = map toInfo <<< Map.toTuples
  where
  toInfo (Tuple asset (Reserve { rAmount })) = { asset, amount: rAmount }
