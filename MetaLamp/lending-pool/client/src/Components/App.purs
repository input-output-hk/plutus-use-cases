module Components.App where

import Prelude

import Business.Aave as Aave
import Capability.Contract (ContractId(..))
import Capability.LogMessages (class LogMessages, logError)
import Capability.PollContract (class PollContract)
import Components.AmountForm as AmountForm
import Control.Monad.Except (lift, runExceptT, throwError)
import Data.Array (mapWithIndex)
import Data.Bifunctor (bimap, lmap)
import Data.BigInteger (BigInteger, fromInt)
import Data.Either (Either(..), either)
import Data.Foldable (find)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Json.JsonTuple (JsonTuple(..))
import Data.Json.JsonUUID (JsonUUID(..))
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Data.UUID (toString) as UUID
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Network.RemoteData (RemoteData(..), fromEither)
import Network.RemoteData as RD
import Plutus.Contracts.Core (Reserve(..))
import Plutus.Contracts.Endpoints (BorrowParams(..), DepositParams(..), RepayParams(..), WithdrawParams(..))
import Plutus.PAB.Webserver.Types (ContractInstanceClientState(..))
import Plutus.V1.Ledger.Crypto (PubKeyHash)
import Plutus.V1.Ledger.Value (AssetClass(..), TokenName(..), Value)
import PlutusTx.AssocMap as Map
import View.FundsTable (fundsTable)
import View.RemoteDataState (remoteDataState)
import View.ReserveInfo (reserveInfo)
import Wallet.Emulator.Wallet (Wallet(..))
import Wallet.Types (ContractInstanceId(..))

type State
  = { contractId :: RemoteData String ContractId
    , walletPubKey :: RemoteData String PubKeyHash
    , userFunds :: RemoteData String Value
    , reserves :: RemoteData String (Map.Map AssetClass Reserve)
    , lastStatus :: RemoteData String String
    }

initialState :: forall input. input -> State
initialState _ =
  { contractId: NotAsked
  , walletPubKey: NotAsked
  , userFunds: NotAsked
  , reserves: NotAsked
  , lastStatus: NotAsked
  }

data Action
  = Init
  | GetContractAt Wallet
  | GetWalletPubKey
  | GetUserFunds
  | GetReserves
  | GetFunds
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
  handleAction :: Action -> H.HalogenM State Action Slots output m Unit
  handleAction = case _ of
    Init -> do
      handleAction (GetContractAt $ Wallet { getWallet: fromInt 2 })
      handleAction GetWalletPubKey
      handleAction GetFunds
    GetContractAt wallet -> do
      H.modify_ _ { contractId = Loading }
      eInstances <- Aave.getAaveContracts
      case eInstances of
        Left e -> H.modify_ _ { contractId = Failure (show e) }
        Right instances -> do
          let
            contract = find (\(ContractInstanceClientState i) -> i.cicWallet == wallet) instances
          case contract of
            Nothing -> H.modify_ _ { contractId = Failure "Contract instance not found" }
            Just (ContractInstanceClientState i) -> H.modify_ _ { contractId = Success (toContractIdParam i.cicContract) }
    GetWalletPubKey ->
      handleException <=< runExceptT
        $ do
            lift $ H.modify_ _ { walletPubKey = Loading }
            state <- lift H.get
            cid <- RD.maybe (throwError "Failed to get wallet public key") pure $ state.contractId
            pkh <- lift $ Aave.ownPubKey cid
            lift $ H.modify_ _ { walletPubKey = fromEither <<< lmap show $ pkh }
    GetUserFunds ->
      handleException <=< runExceptT
        $ do
            lift $ H.modify_ _ { userFunds = Loading }
            state <- lift H.get
            { cid, pkh } <-
              RD.maybe (throwError "Failed to get user funds") pure
                $ { cid: _, pkh: _ }
                <$> state.contractId
                <*> state.walletPubKey
            funds <- lift $ Aave.fundsAt cid pkh
            lift $ H.modify_ _ { userFunds = fromEither <<< lmap show $ funds }
    GetReserves ->
      handleException <=< runExceptT
        $ do
            lift $ H.modify_ _ { reserves = Loading }
            state <- lift H.get
            cid <- RD.maybe (throwError "Failed to get reserves") pure $ state.contractId
            reserves <- lift $ Aave.reserves cid
            lift $ H.modify_ _ { reserves = fromEither <<< lmap show $ reserves }
    GetFunds -> do
      handleAction GetUserFunds
      handleAction GetReserves
    Deposit { amount, asset } ->
      handleException <=< runExceptT
        $ do
            lift $ H.modify_ _ { lastStatus = Loading }
            state <- lift H.get
            { cid, pkh } <-
              RD.maybe (throwError "Failed to deposit") pure
                $ { cid: _, pkh: _ }
                <$> state.contractId
                <*> state.walletPubKey
            res <- lift $ Aave.deposit cid $ DepositParams { dpAmount: amount, dpAsset: asset, dpOnBehalfOf: pkh }
            lift $ H.modify_ _ { lastStatus = fromEither <<< bimap show show $ res }
    Withdraw { amount, asset } ->
      handleException <=< runExceptT
        $ do
            lift $ H.modify_ _ { lastStatus = Loading }
            state <- lift H.get
            { cid, pkh } <-
              RD.maybe (throwError "Failed to withdraw") pure
                $ { cid: _, pkh: _ }
                <$> state.contractId
                <*> state.walletPubKey
            res <- lift $ Aave.withdraw cid $ WithdrawParams { wpAmount: amount, wpAsset: asset, wpUser: pkh }
            lift $ H.modify_ _ { lastStatus = fromEither <<< bimap show show $ res }
    Borrow { amount, asset } ->
      handleException <=< runExceptT
        $ do
            lift $ H.modify_ _ { lastStatus = Loading }
            state <- lift H.get
            { cid, pkh } <-
              RD.maybe (throwError "Failed to borrow") pure
                $ { cid: _, pkh: _ }
                <$> state.contractId
                <*> state.walletPubKey
            res <- lift $ Aave.borrow cid $ BorrowParams { bpAmount: amount, bpAsset: asset, bpOnBehalfOf: pkh }
            lift $ H.modify_ _ { lastStatus = fromEither <<< bimap show show $ res }
    Repay { amount, asset } ->
      handleException <=< runExceptT
        $ do
            lift $ H.modify_ _ { lastStatus = Loading }
            state <- lift H.get
            { cid, pkh } <-
              RD.maybe (throwError "Failed to repay") pure
                $ { cid: _, pkh: _ }
                <$> state.contractId
                <*> state.walletPubKey
            res <- lift $ Aave.repay cid $ RepayParams { rpAmount: amount, rpAsset: asset, rpOnBehalfOf: pkh }
            lift $ H.modify_ _ { lastStatus = fromEither <<< bimap show show $ res }
    SubmitAmount operation (AmountForm.Submit { name, amount }) ->
      handleException <=< runExceptT
        $ do
            state <- lift H.get
            reserves <- RD.maybe (throwError "Failed to submit") pure $ state.reserves
            case find (\(Tuple k _) -> getAssetName k == name) (Map.toTuples reserves) of
              Just (Tuple asset _) -> do
                case operation of
                  SubmitDeposit -> lift $ handleAction (Deposit { amount, asset })
                  SubmitWithdraw -> lift $ handleAction (Withdraw { amount, asset })
                  SubmitBorrow -> lift $ handleAction (Borrow { amount, asset })
                  SubmitRepay -> lift $ handleAction (Repay { amount, asset })
                lastStatus <- lift $ H.gets _.lastStatus
                _ <- RD.maybe (throwError $ show operation <> " has failed: " <> show lastStatus) pure $ lastStatus
                lift $ handleAction GetFunds
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
