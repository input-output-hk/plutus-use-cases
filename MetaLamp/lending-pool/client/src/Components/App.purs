module Components.App where

import Prelude
import Business.Aave as Aave
import Capability.Contract (ContractId(..))
import Capability.LogMessages (class LogMessages, logError)
import Capability.PollContract (class PollContract)
import Components.AmountForm as AmountForm
import Control.Monad.Except (lift, runExceptT, throwError)
import Data.Array (groupBy, mapWithIndex)
import Data.Array.NonEmpty as NEA
import Data.BigInteger (BigInteger, fromInt)
import Data.Either (Either, either)
import Data.Foldable (find)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Json.JsonTuple (JsonTuple(..))
import Data.Json.JsonUUID (JsonUUID(..))
import Data.Lens (Lens')
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Data.UUID (toString) as UUID
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Network.RemoteData (RemoteData(..))
import Network.RemoteData as RD
import Plutus.Contracts.Core (Reserve(..), UserConfig)
import Plutus.Contracts.Endpoints (BorrowParams(..), DepositParams(..), RepayParams(..), WithdrawParams(..))
import Plutus.PAB.Webserver.Types (ContractInstanceClientState(..))
import Plutus.V1.Ledger.Crypto (PubKeyHash)
import Plutus.V1.Ledger.Value (AssetClass(..), TokenName(..), Value)
import PlutusTx.AssocMap as Map
import Utils.WithRemoteData (runRDWith)
import View.FundsTable (fundsTable)
import View.RemoteDataState (remoteDataState)
import View.ReserveInfo (reserveInfo)
import View.UsersTable (poolUsers)
import Wallet.Emulator.Wallet (Wallet(..))
import Wallet.Types (ContractInstanceId(..))

type State
  = { contractId :: RemoteData String ContractId
    , walletPubKey :: RemoteData String PubKeyHash
    , userFunds :: RemoteData String Value
    , users :: RemoteData String (Map.Map (JsonTuple AssetClass PubKeyHash) UserConfig)
    , reserves :: RemoteData String (Map.Map AssetClass Reserve)
    , deposit :: RemoteData String Unit
    , withdraw :: RemoteData String Unit
    , borrow :: RemoteData String Unit
    , repay :: RemoteData String Unit
    , submit :: RemoteData String Unit
    }

_contractId :: Lens' State (RemoteData String ContractId)
_contractId = prop (SProxy :: SProxy "contractId")

_walletPubKey :: Lens' State (RemoteData String PubKeyHash)
_walletPubKey = prop (SProxy :: SProxy "walletPubKey")

_userFunds :: Lens' State (RemoteData String Value)
_userFunds = prop (SProxy :: SProxy "userFunds")

_users :: Lens' State (RemoteData String (Map.Map (JsonTuple AssetClass PubKeyHash) UserConfig))
_users = prop (SProxy :: SProxy "users")

_reserves :: Lens' State (RemoteData String (Map.Map AssetClass Reserve))
_reserves = prop (SProxy :: SProxy "reserves")

_deposit :: Lens' State (RemoteData String Unit)
_deposit = prop (SProxy :: SProxy "deposit")

_withdraw :: Lens' State (RemoteData String Unit)
_withdraw = prop (SProxy :: SProxy "withdraw")

_borrow :: Lens' State (RemoteData String Unit)
_borrow = prop (SProxy :: SProxy "borrow")

_repay :: Lens' State (RemoteData String Unit)
_repay = prop (SProxy :: SProxy "repay")

_submit :: Lens' State (RemoteData String Unit)
_submit = prop (SProxy :: SProxy "submit")

initialState :: forall input. input -> State
initialState _ =
  { contractId: NotAsked
  , walletPubKey: NotAsked
  , userFunds: NotAsked
  , users: NotAsked
  , reserves: NotAsked
  , withdraw: NotAsked
  , deposit: NotAsked
  , borrow: NotAsked
  , repay: NotAsked
  , submit: NotAsked
  }

data Action
  = Init
  | GetContractAt Wallet
  | GetWalletPubKey
  | GetUserFunds
  | GetUserConfigs
  | GetReserves
  | GetUpdates
  | Deposit { amount :: BigInteger, asset :: AssetClass }
  | Withdraw { amount :: BigInteger, asset :: AssetClass }
  | Borrow { amount :: BigInteger, asset :: AssetClass }
  | Repay { amount :: BigInteger, asset :: AssetClass }
  | SubmitAmount SubmitOperation AmountForm.Output

-- potentially should be separate actions - just a convenience for now, while they are identical
data SubmitOperation
  = SubmitDeposit
  | SubmitWithdraw
  | SubmitBorrow
  | SubmitRepay

derive instance genericSubmitOperation :: Generic SubmitOperation _

instance showSubmitOperation :: Show SubmitOperation where
  show = genericShow

toContractIdParam :: ContractInstanceId -> ContractId
toContractIdParam (ContractInstanceId { unContractInstanceId: JsonUUID uuid }) = ContractId <<< UUID.toString $ uuid

handleException :: forall e a m. LogMessages m => Show e => Either e a -> m Unit
handleException = either (logError <<< show) (const $ pure unit)

type Slots
  = ( amountForm :: forall query. H.Slot query AmountForm.Output Int )

_amountForm = SProxy :: SProxy "amountForm"

component ::
  forall input m query output.
  LogMessages m =>
  PollContract m =>
  H.Component HH.HTML query input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }
  where
  runRD ::
    forall e a.
    Show e =>
    (Lens' State (RemoteData e a)) ->
    H.HalogenM State Action Slots output m (Either e a) ->
    H.HalogenM State Action Slots output m Unit
  runRD selector action =
    (runRDWith selector $ action)
      >>= case _ of
          Failure e -> logError <<< show $ e
          _ -> pure unit

  handleAction :: Action -> H.HalogenM State Action Slots output m Unit
  handleAction = case _ of
    Init -> do
      handleAction (GetContractAt $ Wallet { getWallet: fromInt 2 })
      handleAction GetWalletPubKey
      handleAction GetUpdates
    GetContractAt wallet ->
      runRD _contractId <<< runExceptT
        $ do
            instances <- lift Aave.getAaveContracts >>= either (throwError <<< show) pure
            let
              contract = find (\(ContractInstanceClientState i) -> i.cicWallet == wallet) instances
            maybe
              (throwError "Contract instance not found")
              (pure <<< toContractIdParam <<< _.cicContract <<< unwrap)
              contract
    GetWalletPubKey ->
      runRD _walletPubKey <<< runExceptT
        $ do
            state <- lift H.get
            cid <- RD.maybe (throwError "contractId is missing") pure state.contractId
            lift (Aave.ownPubKey cid) >>= either (throwError <<< show) pure
    GetUserFunds ->
      runRD _userFunds <<< runExceptT
        $ do
            state <- lift H.get
            { cid, pkh } <-
              RD.maybe (throwError "contractId or publicKey are missing") pure
                $ { cid: _, pkh: _ }
                <$> state.contractId
                <*> state.walletPubKey
            lift (Aave.fundsAt cid pkh) >>= either (throwError <<< show) pure
    GetUserConfigs ->
      runRD _users <<< runExceptT
        $ do
            state <- lift H.get
            cid <- RD.maybe (throwError "contractId is missing") pure state.contractId
            lift (Aave.users cid) >>= either (throwError <<< show) pure
    GetReserves ->
      runRD _reserves <<< runExceptT
        $ do
            state <- lift H.get
            cid <- RD.maybe (throwError "contractId or publicKey are missing") pure $ state.contractId
            lift (Aave.reserves cid) >>= either (throwError <<< show) pure
    GetUpdates -> do
      handleAction GetUserFunds
      handleAction GetReserves
      handleAction GetUserConfigs
    Deposit { amount, asset } ->
      runRD _deposit <<< runExceptT
        $ do
            state <- lift H.get
            { cid, pkh } <-
              RD.maybe (throwError "contractId or publicKey are missing") pure
                $ { cid: _, pkh: _ }
                <$> state.contractId
                <*> state.walletPubKey
            lift (Aave.deposit cid $ DepositParams { dpAmount: amount, dpAsset: asset, dpOnBehalfOf: pkh })
              >>= either (throwError <<< show) (const <<< pure $ unit)
    Withdraw { amount, asset } ->
      runRD _withdraw <<< runExceptT
        $ do
            state <- lift H.get
            { cid, pkh } <-
              RD.maybe (throwError "contractId or publicKey are missing") pure
                $ { cid: _, pkh: _ }
                <$> state.contractId
                <*> state.walletPubKey
            lift (Aave.withdraw cid $ WithdrawParams { wpAmount: amount, wpAsset: asset, wpUser: pkh })
              >>= either (throwError <<< show) (const <<< pure $ unit)
    Borrow { amount, asset } ->
      runRD _borrow <<< runExceptT
        $ do
            state <- lift H.get
            { cid, pkh } <-
              RD.maybe (throwError "contractId or publicKey are missing") pure
                $ { cid: _, pkh: _ }
                <$> state.contractId
                <*> state.walletPubKey
            lift (Aave.borrow cid $ BorrowParams { bpAmount: amount, bpAsset: asset, bpOnBehalfOf: pkh })
              >>= either (throwError <<< show) (const <<< pure $ unit)
    Repay { amount, asset } ->
      runRD _repay <<< runExceptT
        $ do
            state <- lift H.get
            { cid, pkh } <-
              RD.maybe (throwError "contractId or publicKey are missing") pure
                $ { cid: _, pkh: _ }
                <$> state.contractId
                <*> state.walletPubKey
            lift (Aave.repay cid $ RepayParams { rpAmount: amount, rpAsset: asset, rpOnBehalfOf: pkh })
              >>= either (throwError <<< show) (const <<< pure $ unit)
    SubmitAmount operation (AmountForm.Submit { name, amount }) ->
      runRD _submit <<< runExceptT
        $ do
            state <- lift H.get
            reserves <- RD.maybe (throwError "reserves are missing") pure $ state.reserves
            case find (\(Tuple k _) -> getAssetName k == name) (Map.toTuples reserves) of
              Just (Tuple asset _) -> case operation of
                SubmitDeposit -> do
                  lift $ handleAction (Deposit { amount, asset })
                  { deposit } <- lift H.get
                  RD.maybe
                    (throwError $ "Submit deposit failed: " <> show deposit)
                    (const <<< lift <<< handleAction $ GetUpdates)
                    deposit
                SubmitWithdraw -> do
                  lift $ handleAction (Withdraw { amount, asset })
                  { withdraw } <- lift H.get
                  RD.maybe
                    (throwError $ "Submit withdraw failed: " <> show withdraw)
                    (const <<< lift <<< handleAction $ GetUpdates)
                    withdraw
                SubmitBorrow -> do
                  lift $ handleAction (Borrow { amount, asset })
                  { borrow } <- lift H.get
                  RD.maybe
                    (throwError $ "Submit borrow failed: " <> show borrow)
                    (const <<< lift <<< handleAction $ GetUpdates)
                    borrow
                SubmitRepay -> do
                  lift $ handleAction (Repay { amount, asset })
                  { repay } <- lift H.get
                  RD.maybe
                    (throwError $ "Submit repay failed: " <> show repay)
                    (const <<< lift <<< handleAction $ GetUpdates)
                    repay
              Nothing -> throwError "Asset name not found"

  render :: State -> H.ComponentHTML Action Slots m
  render state =
    HH.div_
      [ HH.button [ HE.onClick \_ -> Just Init ] [ HH.text "Start" ]
      , remoteDataState
          (\userFunds -> HH.div_ [ HH.h2_ [ HH.text "User funds" ], fundsTable userFunds ])
          state.userFunds
      , remoteDataState
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
      , case state.submit of
          NotAsked -> HH.div_ []
          Loading -> HH.div_ []
          Failure e -> HH.h2_ [ HH.text $ "Error: " <> show e ]
          Success _ -> HH.div_ []
      , remoteDataState
          ( \amounts ->
              HH.div_
                $ mapWithIndex
                    ( \index (Tuple title operation) ->
                        HH.h2_ [ HH.text title, HH.slot _amountForm index AmountForm.amountForm amounts (Just <<< (SubmitAmount operation)) ]
                    )
                    [ Tuple "Deposit" SubmitDeposit, Tuple "Withdraw" SubmitWithdraw, Tuple "Borrow" SubmitBorrow, Tuple "Repay" SubmitRepay ]
          )
          (map reservesToAmounts state.reserves)
      ]

reservesToAmounts :: Map.Map AssetClass Reserve -> Array AmountForm.AmountInfo
reservesToAmounts = map toInfo <<< Map.toTuples
  where
  toInfo (Tuple k (Reserve { rAmount })) = { name: getAssetName k, amount: rAmount }

getAssetName :: AssetClass -> String
getAssetName (AssetClass { unAssetClass: JsonTuple (Tuple _ (TokenName { unTokenName: name })) }) = name
