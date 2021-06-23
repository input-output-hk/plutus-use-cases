module PAB.ApiClient where

--------------------------------------------------------------------------------

import Affjax as AX
import Affjax.RequestBody as AXRB
import Affjax.ResponseFormat as AXRF
import Data.Argonaut as A
import Data.Either (Either(..))
import Data.Eq ((==))
import Data.Maybe (Maybe(..))
import Data.String.Common (joinWith)
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff (Aff, delay)
import PAB.Types 
  ( ApiError(DecodeError, RequestError)
  , ContractInstanceClientState
  , ContractInstanceId
  , PABConfig
  , Wallet(..)
  )
import Prelude (bind, pure, show, ($), (<>))

--------------------------------------------------------------------------------

getWallets
  :: PABConfig
  -> Wallet
  -> Aff (Either ApiError (Array ContractInstanceClientState))
getWallets pab (Wallet w) = getJson url
  where
    url = 
      pab.baseUrl <> 
      "/api/new/contract/instances/wallet/" <> 
      show w

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