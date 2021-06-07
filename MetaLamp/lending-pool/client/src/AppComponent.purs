module AppComponent where

import Prelude

import Aave as Aave
import AmountForm as AmountForm
import Capability (class Contract, class LogMessages, APIError(..), ContractId(..), logError, logInfo)
import Control.Monad.Except (class MonadError, lift, runExceptT, throwError)
import Data.Bifunctor (bimap, lmap)
import Data.BigInteger (BigInteger, fromInt)
import Data.Either (Either(..), either)
import Data.Foldable (find)
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
import Plutus.Contracts.Core (Reserve(..))
import Plutus.Contracts.Endpoints (BorrowParams(..), DepositParams(..), RepayParams(..), WithdrawParams(..))
import Plutus.PAB.Webserver.Types (ContractInstanceClientState(..))
import Plutus.V1.Ledger.Crypto (PubKeyHash)
import Plutus.V1.Ledger.Value (AssetClass(..), TokenName(..), Value(..))
import PlutusTx.AssocMap as Map
import Wallet.Emulator.Wallet (Wallet(..))
import Wallet.Types (ContractInstanceId(..))

type State =
  { contractId :: RemoteData String ContractId,
    walletPubKey :: RemoteData String PubKeyHash,
    userFunds :: RemoteData String Value,
    reserves :: RemoteData String (Map.Map AssetClass Reserve),
    lastStatus :: RemoteData String String }

initialState :: forall input. input -> State
initialState _ =
  { contractId: NotAsked,
    walletPubKey: NotAsked,
    userFunds: NotAsked,
    reserves: NotAsked,
    lastStatus: NotAsked }

data Action =
  Init
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
data SubmitOperation = SubmitDeposit | SubmitWithdraw | SubmitBorrow | SubmitRepay

toContractIdParam :: ContractInstanceId -> ContractId
toContractIdParam (ContractInstanceId { unContractInstanceId: JsonUUID uuid }) = ContractId <<< UUID.toString $ uuid

handleExcept :: forall e a m. LogMessages m => Show e => Either e a -> m Unit
handleExcept = either (show >>> logError) (const $ pure unit)

getRD :: forall e a m. MonadError String m => Show e => Show a => String -> RemoteData e a -> m a
getRD _ (Success s) = pure s
getRD tag rd = throwError $ tag <> " is not available: " <> (show rd)

getContractId :: forall e a m. MonadError String m => Show e => Show a => RemoteData e a -> m a
getContractId = getRD "contractId"

getPubKey :: forall e a m. MonadError String m => Show e => Show a => RemoteData e a -> m a
getPubKey = getRD "pubKey"

getReserves :: forall e a m. MonadError String m => Show e => Show a => RemoteData e a -> m a
getReserves = getRD "reserves"

type Slots = ( amountForm :: forall query. H.Slot query AmountForm.Output Int )
_amountForm = SProxy :: SProxy "amountForm"

component :: forall input m query output.
  LogMessages m =>
  Contract m =>
  H.Component HH.HTML query input output m
component =
  H.mkComponent
    {
      initialState,
      render,
      eval: H.mkEval H.defaultEval { handleAction = handleAction }
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
        instancesRD <- Aave.getAaveContracts
        case instancesRD of
          Left e -> H.modify_ _ { contractId = Failure (show e) }
          Right instances -> do
            let contract = find (\(ContractInstanceClientState i) -> i.cicWallet == wallet) instances
            case contract of
              Nothing -> H.modify_ _ { contractId = Failure "Contract instance not found" }
              Just (ContractInstanceClientState i) ->
                H.modify_ _ { contractId = Success (toContractIdParam i.cicContract) }
      GetWalletPubKey -> handleExcept <=< runExceptT $ do
        lift $ H.modify_ _ { walletPubKey = Loading }
        { contractId } <- lift H.get
        cid <- getContractId contractId
        pkh <- lift $ Aave.ownPubKey cid
        lift $ H.modify_ _ { walletPubKey = fromEither <<< lmap show $ pkh }
      GetUserFunds -> handleExcept <=< runExceptT $ do
        lift $ H.modify_ _ { userFunds = Loading }
        { contractId, walletPubKey } <- lift H.get
        cid <- getContractId contractId
        pkh <- getPubKey walletPubKey
        funds <- lift $ Aave.fundsAt cid pkh
        lift $ H.modify_ _ { userFunds = fromEither <<< lmap show $ funds }
      GetReserves -> handleExcept <=< runExceptT $ do
        lift $ H.modify_ _ { reserves = Loading }
        { contractId } <- lift H.get
        cid <- getContractId contractId
        reserves <- lift $ Aave.reserves cid
        lift $ H.modify_ _ { reserves = fromEither <<< lmap show $ reserves }
      GetFunds -> do
        handleAction GetUserFunds
        handleAction GetReserves

      Deposit { amount, asset } -> handleExcept <=< runExceptT $ do
        { contractId, walletPubKey } <- lift H.get
        cid <- getContractId contractId
        pkh <- getPubKey walletPubKey
        res <- lift $ Aave.deposit cid $ DepositParams { dpAmount: amount, dpAsset: asset, dpOnBehalfOf: pkh }
        lift $ H.modify_ _ { lastStatus = fromEither <<< bimap show show $ res }
      Withdraw { amount, asset } -> handleExcept <=< runExceptT $ do
        { contractId, walletPubKey } <- lift H.get
        cid <- getContractId contractId
        pkh <- getPubKey walletPubKey
        res <- lift $ Aave.withdraw cid $ WithdrawParams { wpAmount: amount, wpAsset: asset, wpTo: pkh, wpFrom: pkh }
        lift $ H.modify_ _ { lastStatus = fromEither <<< bimap show show $ res }
      Borrow { amount, asset } -> handleExcept <=< runExceptT $ do
        { contractId, walletPubKey } <- lift H.get
        cid <- getContractId contractId
        pkh <- getPubKey walletPubKey
        res <- lift $ Aave.borrow cid $ BorrowParams { bpAmount: amount, bpAsset: asset, bpOnBehalfOf: pkh }
        lift $ H.modify_ _ { lastStatus = fromEither <<< bimap show show $ res }
      Repay { amount, asset } -> handleExcept <=< runExceptT $ do
        { contractId, walletPubKey } <- lift H.get
        cid <- getContractId contractId
        pkh <- getPubKey walletPubKey
        res <- lift $ Aave.repay cid $ RepayParams { rpAmount: amount, rpAsset: asset, rpOnBehalfOf: pkh }
        lift $ H.modify_ _ { lastStatus = fromEither <<< bimap show show $ res }

      SubmitAmount operation (AmountForm.Submit { name, amount }) -> handleExcept <=< runExceptT $ do
        { reserves: rs } <- lift H.get
        reserves <- getReserves rs
        case find (\(Tuple k _) -> getAssetName k == name) (Map.toTuples reserves) of
          Just (Tuple asset _) -> do
            case operation of
              SubmitDeposit -> lift $ handleAction (Deposit { amount, asset })
              SubmitWithdraw -> lift $ handleAction (Withdraw { amount, asset })
              SubmitBorrow -> lift $ handleAction (Borrow { amount, asset })
              SubmitRepay -> lift $ handleAction (Repay { amount, asset })
            lift $ handleAction GetFunds
          Nothing -> throwError "Asset name not found"

    render :: State -> H.ComponentHTML Action Slots m
    render state =
      HH.div_
        [ HH.button [ HE.onClick \_ -> Just Init ] [ HH.text "Start" ]
        , remoteDataState (\v -> HH.div_ [HH.h2_ [HH.text "User funds"], fundsTable v]) state.userFunds
        , remoteDataState
            (\v -> HH.div_ $ [HH.h2_ [HH.text "Pool funds"]] <> map (\(Tuple a r) -> reserveTab a r) v)
            (map Map.toTuples state.reserves)
        , remoteDataState
            (\v -> HH.h2_ [HH.text "Deposit", HH.slot _amountForm 0 AmountForm.amountForm v (Just <<< (SubmitAmount SubmitDeposit))])
            (map reservesToAmounts state.reserves)
        , remoteDataState
            (\v -> HH.h2_ [HH.text "Withdraw", HH.slot _amountForm 1 AmountForm.amountForm v (Just <<< (SubmitAmount SubmitWithdraw))])
            (map reservesToAmounts state.reserves)
        , remoteDataState
            (\v -> HH.h2_ [HH.text "Borrow", HH.slot _amountForm 1 AmountForm.amountForm v (Just <<< (SubmitAmount SubmitBorrow))])
            (map reservesToAmounts state.reserves)
        , remoteDataState
            (\v -> HH.h2_ [HH.text "Repay", HH.slot _amountForm 1 AmountForm.amountForm v (Just <<< (SubmitAmount SubmitRepay))])
            (map reservesToAmounts state.reserves)
        ]

remoteDataState :: forall props act e a. Show e => (a -> HH.HTML props act) -> RemoteData e a -> HH.HTML props act
remoteDataState _ NotAsked = HH.div_ [HH.text ""]
remoteDataState _ Loading = HH.div_ [HH.text "Loading..."]
remoteDataState _ (Failure e) = HH.div_ [HH.text $ "Error: " <> show e]
remoteDataState f (Success s) = f s

getAssetName :: AssetClass -> String
getAssetName (AssetClass { unAssetClass: JsonTuple (Tuple _ (TokenName { unTokenName: name })) }) = name

reservesToAmounts :: Map.Map AssetClass Reserve -> Array AmountForm.AmountInfo
reservesToAmounts = map toInfo <<< Map.toTuples
  where
    toInfo (Tuple k (Reserve { rAmount })) = { name: getAssetName k, amount: rAmount }

reserveTab :: forall props act. AssetClass -> Reserve -> HH.HTML props act
reserveTab (AssetClass { unAssetClass: JsonTuple (Tuple _ name)}) (Reserve { rAmount }) =
  poolTab name rAmount

poolTab :: forall props act. TokenName -> BigInteger -> HH.HTML props act
poolTab (TokenName { unTokenName: name }) amount =
  HH.div_ $ [HH.h4_ [HH.text (name <> " pool balance")], HH.text $ show amount]

fundsTable :: forall props act. Value -> HH.HTML props act
fundsTable (Value ({ getValue: m })) = HH.div_ $ do
  (Tuple _ amounts) <- Map.toTuples m
  (Tuple name amount) <- Map.toTuples amounts
  if amount > (fromInt 0)
    then pure $ amountTab name amount
    else []

amountTab :: forall props act. TokenName -> BigInteger -> HH.HTML props act
amountTab (TokenName { unTokenName: name }) amount =
  HH.div_ $ [HH.text (showName name <> " " <> show amount)]
    where
      showName "" = "ADA"
      showName n = n
