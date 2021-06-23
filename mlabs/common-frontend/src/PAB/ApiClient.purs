module PAB.ApiClient where

--------------------------------------------------------------------------------

import Affjax as AX
import Affjax.RequestBody as AXRB
import Affjax.ResponseFormat as AXRF
import Data.Argonaut as A
import Data.Either (Either(..))
import Data.Eq ((/=))
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff (Aff, delay)
import PAB.Types 
  ( ApiError(DecodeError, RequestError)
  , ContractInstanceClientState
  , ContractInstanceId(..)
  , PabConfig
  , Wallet(..)
  )
import Prelude (bind, pure, show, ($), (<>))

--------------------------------------------------------------------------------

contractInstPath :: String
contractInstPath = "/api/new/contract/instances/"

getStatus 
  :: forall t
   . PabConfig
  -> ContractInstanceId
  -> Aff (Either ApiError (ContractInstanceClientState A.Json))
getStatus pab (ContractInstanceId ciid) =
  let url = pab.baseUrl <> contractInstPath <> ciid <> "/status"
  in getJson url

getWallets
  :: PabConfig
  -> Wallet
  -> Aff (Either ApiError (Array (ContractInstanceClientState A.Json)))
getWallets pab (Wallet w) = 
  let url = pab.baseUrl <> contractInstPath <> "wallet/" <> show w
  in getJson url

waitForNewState 
  :: PabConfig
  -> ContractInstanceId
  -> A.Json
  -> Aff A.Json
waitForNewState pab ciid prevState = do
  res <- getStatus pab ciid
  case res of
    Left _       -> keepWaiting
    Right status -> do
      let newState = status.cicCurrentState.observableState
      if newState /= prevState then pure newState
      else keepWaiting
 where 
  keepWaiting = do
    _ <- delay (Milliseconds 1000.0)
    waitForNewState pab ciid prevState

    
postEndpoint
  :: forall payload
   . A.EncodeJson payload
  => PabConfig
  -> ContractInstanceId
  -> String
  -> payload
  -> Aff (Either ApiError A.Json)
postEndpoint pab (ContractInstanceId ciid) endpoint payload =
  let 
    url = 
      pab.baseUrl <> 
      contractInstPath <> 
      ciid <> 
      "/endpoint/" <> 
      endpoint
  in postJson url payload

getJson
  :: forall res
   . A.DecodeJson res
  => String
  -> Aff (Either ApiError res)
getJson url = do
  res <- AX.get AXRF.json url 
  handleResponse res

postJson
  :: forall payload res
   . A.EncodeJson payload
  => A.DecodeJson res
  => String
  -> payload
  -> Aff (Either ApiError res)
postJson url payload = do
  res <- AX.post AXRF.json url (Just $ AXRB.Json $ A.encodeJson payload)
  handleResponse res

handleResponse 
  :: forall res
   . A.DecodeJson res
  => Either AX.Error (AX.Response A.Json)
  -> Aff (Either ApiError res)
handleResponse res = do
  case res of
    Left e     -> pure $ Left (RequestError $ e)
    Right res' -> do
      case A.decodeJson res'.body of
        Left e' -> pure $ Left (DecodeError $ e')
        Right decoded -> pure $ Right decoded