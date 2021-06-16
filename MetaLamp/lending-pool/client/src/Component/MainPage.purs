module Component.MainPage where

import Prelude
import Business.Aave as Aave
import Business.AaveInfo as AaveInfo
import Business.AaveUser (UserContractId)
import Business.AaveUser as AaveUser
import Capability.LogMessages (class LogMessages)
import Capability.PollContract (class PollContract)
import Component.Contract as Contract
import Component.Contract as ContractComponent
import Component.Utils (runRD)
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
import Plutus.Contracts.Core (Reserve(..), UserConfig)
import Plutus.PAB.Simulation (AaveContracts)
import Plutus.PAB.Webserver.Types (ContractInstanceClientState)
import Plutus.V1.Ledger.Crypto (PubKeyHash)
import Plutus.V1.Ledger.Value (AssetClass, Value)
import PlutusTx.AssocMap as Map
import View.RemoteDataState (remoteDataState)
import View.ReserveInfo (reserveInfo)
import View.UsersTable (poolUsers)
import Utils.BEM as BEM

type State
  = { contracts :: RemoteData String (Array (ContractInstanceClientState AaveContracts))
    , userContracts :: RemoteData String (Array ({ pubKey :: PubKeyHash, contractId :: UserContractId }))
    , users :: RemoteData String (Map.Map (JsonTuple AssetClass PubKeyHash) UserConfig)
    , userFunds :: RemoteData String (Array (Tuple PubKeyHash Value))
    , reserves :: RemoteData String (Map.Map AssetClass Reserve)
    }

_contracts :: Lens' State (RemoteData String (Array (ContractInstanceClientState AaveContracts)))
_contracts = prop (SProxy :: SProxy "contracts")

_userContracts :: Lens' State (RemoteData String (Array ({ pubKey :: PubKeyHash, contractId :: UserContractId })))
_userContracts = prop (SProxy :: SProxy "userContracts")

_users :: Lens' State (RemoteData String (Map.Map (JsonTuple AssetClass PubKeyHash) UserConfig))
_users = prop (SProxy :: SProxy "users")

_userFunds :: Lens' State (RemoteData String (Array (Tuple PubKeyHash Value)))
_userFunds = prop (SProxy :: SProxy "userFunds")

_reserves :: Lens' State (RemoteData String (Map.Map AssetClass Reserve))
_reserves = prop (SProxy :: SProxy "reserves")

data Action
  = Init
  | GetContracts
  | GetUserContracts
  | GetUserFunds
  | GetUserConfigs
  | GetReserves
  | OnSubmitSuccess ContractComponent.Output

type Slots
  = ( contract :: forall query. H.Slot query ContractComponent.Output Int )

_contract = SProxy :: SProxy "contract"

initialState :: forall input. input -> State
initialState _ = { contracts: NotAsked, userContracts: NotAsked, userFunds: NotAsked, users: NotAsked, reserves: NotAsked }

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
      handleAction GetUserContracts
      handleAction GetUserFunds
      handleAction GetReserves
      handleAction GetUserConfigs
    OnSubmitSuccess _ -> do
      handleAction GetUserFunds
      handleAction GetReserves
      handleAction GetUserConfigs
    GetContracts ->
      runRD _contracts <<< runExceptT
        $ lift Aave.getAaveContracts
        >>= either (throwError <<< show) pure
    GetUserContracts ->
      runRD _userContracts <<< runExceptT
        $ do
            state <- lift H.get
            contracts <- RemoteData.maybe (throwError "No contracts found") pure state.contracts
            parTraverse
              ( \contractId -> do
                  pubKey <- lift (AaveUser.ownPubKey contractId) >>= either (throwError <<< show) pure
                  pure $ { pubKey, contractId }
              )
              (catMaybes <<< map AaveUser.getUserContractId $ contracts)
    GetUserFunds ->
      runRD _userFunds <<< runExceptT
        $ do
            state <- lift H.get
            userContracts <-
              RD.maybe (throwError "userContracts are missing") pure
                $ state.userContracts
            parTraverse
              ( \{ pubKey, contractId } ->
                  lift (AaveUser.ownPubKeyBalance contractId) >>= either (throwError <<< show) (\balance -> pure $ Tuple pubKey balance)
              )
              userContracts
    GetUserConfigs ->
      runRD _users <<< runExceptT
        $ do
            state <- lift H.get
            contracts <-
              RD.maybe (throwError "contracts are missing") pure
                $ state.contracts
            case catMaybes (AaveInfo.getInfoContractId <$> contracts) of
              [ cid ] -> lift (AaveInfo.users cid) >>= either (throwError <<< show) pure
              _ -> throwError "Info contract not found"
    GetReserves ->
      runRD _reserves <<< runExceptT
        $ do
            state <- lift H.get
            contracts <-
              RD.maybe (throwError "contracts are missing") pure
                $ state.contracts
            case catMaybes (AaveInfo.getInfoContractId <$> contracts) of
              [ cid ] -> lift (AaveInfo.reserves cid) >>= either (throwError <<< show) pure
              _ -> throwError "Info contract not found"

  content = BEM.block "content"

  render :: State -> H.ComponentHTML Action Slots m
  render state =
    HH.div [ classes [ content "" ] ]
      [ HH.div [ classes [ content "statistics" ] ]
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
          ]
      , HH.div [ classes [ content "contracts" ] ]
          [ remoteDataState
              ( \{ userContracts, reserves, userFunds } ->
                  HH.div [ classes [ content "contracts-list" ] ]
                    $ mapWithIndex
                        ( \index ({ pubKey: walletPubKey, contractId }) -> case findMap (\(Tuple k v) -> if k == walletPubKey then Just v else Nothing) $ userFunds of
                            Nothing -> HH.div_ [ HH.text "No funds found" ]
                            Just funds ->
                              HH.slot
                                _contract
                                index
                                Contract.component
                                ( { userContractId: contractId
                                  , walletPubKey
                                  , reserves: toReserveInfo reserves
                                  , userFunds: funds
                                  }
                                )
                                (Just <<< OnSubmitSuccess)
                        )
                        (take 2 userContracts)
              )
              ({ userContracts: _, reserves: _, userFunds: _ } <$> state.userContracts <*> state.reserves <*> state.userFunds)
          ]
      ]

toReserveInfo :: Map.Map AssetClass Reserve -> Array { amount :: BigInteger, asset :: AssetClass }
toReserveInfo = map toInfo <<< Map.toTuples
  where
  toInfo (Tuple asset (Reserve { rAmount })) = { asset, amount: rAmount }
