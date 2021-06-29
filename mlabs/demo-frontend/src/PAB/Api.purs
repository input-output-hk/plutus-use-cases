module PAB.Api
  ( activateContract
  , getContractDefinitions
  , getStatus
  , getWalletInstances
  , postEndpoint
  , waitForNewState
  ) where

--------------------------------------------------------------------------------

import Affjax as AX
import Affjax.RequestBody as AXRB
import Affjax.ResponseFormat as AXRF
import Data.Argonaut as A
import Data.Either (Either(..))
import Data.Eq ((/=))
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Debug.Trace (trace)
import Effect.Aff (Aff, delay)
import Effect.Class (liftEffect)
import Effect.Class.Console (log, logShow)
import Error (throwRequestError, throwDecodeError)
import PAB.Types (ContractInstanceClientState, ContractInstanceId(..), ContractSignatureResponse, PabConfig, Wallet(..))
import Prelude (bind, pure, show, ($), (<>))

--------------------------------------------------------------------------------
contractInstPath :: String
contractInstPath = "/api/new/contract/instances/"

getContractDefinitions :: 
  PabConfig ->
  Aff (Array ContractSignatureResponse)
getContractDefinitions pab =
  let 
    url = pab.baseUrl <> "/api/new/contract/definitions"
  in
    getJson url

-- | Gets the current status info for the specific contract instance
getStatus ::
  PabConfig ->
  ContractInstanceId ->
  Aff (ContractInstanceClientState A.Json)
getStatus pab { unContractInstanceId: ciid } =
  let
    url = pab.baseUrl <> contractInstPath <> ciid <> "/status"
  in
    getJson url

activateContract ::
  PabConfig ->
  String -> 
  Int ->
  Aff ContractInstanceId
activateContract pab caID wallet =
  let
    url = pab.baseUrl <> "/api/new/contract/activate"
    payload = {
      "caID": "User",
      "caWallet": {
        "getWallet": wallet
      }
    }
  in 
    postJson url payload

-- | Gets the contract instances for the specific wallet
getWalletInstances ::
  PabConfig ->
  Wallet ->
  Aff (Array (ContractInstanceClientState A.Json))
getWalletInstances pab { getWallet: w } =
  let
    url = pab.baseUrl <> contractInstPath <> "wallet/" <> show w
  in
    getJson url

-- | Waits until the state has updated for the specified contract instance
-- and returns the new state.
waitForNewState ::
  PabConfig ->
  ContractInstanceId ->
  A.Json ->
  Aff A.Json
waitForNewState pab ciid prevState = do
  res <- getStatus pab ciid
  let newState = res.cicCurrentState.observableState
  if newState /= prevState then pure newState else keepWaiting
 where
  keepWaiting = do
    _ <- delay (Milliseconds 1000.0)
    waitForNewState pab ciid prevState

-- | Sends a POST request to the specified endpoint.
postEndpoint ::
  forall payload.
  A.EncodeJson payload =>
  PabConfig ->
  ContractInstanceId ->
  String ->
  payload ->
  Aff A.Json
postEndpoint pab { unContractInstanceId: ciid } endpoint payload =
  let
    url =
      pab.baseUrl
        <> "/api/new/contract/instance/"
        <> ciid
        <> "/endpoint/"
        <> endpoint
  in
    postJson url payload

-- | Sends a GET request to the specified URL and returns the response JSON (or error).
getJson ::
  forall res.
  A.DecodeJson res =>
  String ->
  Aff res
getJson url = do
  _ <- liftEffect $ log $ "Request sent to " <> url
  res <- AX.get AXRF.json url
  _ <- liftEffect $ log "Response received:"
  _ <- trace res \x-> pure x
  handleResponse res $ "GET request to " <> url <> " failed: "

-- | Sends a POST request to the specified URL and returns the response JSON (or error).
postJson ::
  forall payload res.
  A.EncodeJson payload =>
  A.DecodeJson res =>
  String ->
  payload ->
  Aff res
postJson url payload = do
  res <- AX.post AXRF.json url (Just $ AXRB.Json $ A.encodeJson payload)
  handleResponse res $ "POST request to " <> url <> " failed: "

handleResponse ::
  forall res.
  A.DecodeJson res =>
  Either AX.Error (AX.Response A.Json) ->
  String ->
  Aff res
handleResponse res errMsg = do
  case res of
    Left e -> throwRequestError errMsg e
    Right res' -> do
      case A.decodeJson res'.body of
        Left e' -> throwDecodeError errMsg e'
        Right decoded -> pure decoded
