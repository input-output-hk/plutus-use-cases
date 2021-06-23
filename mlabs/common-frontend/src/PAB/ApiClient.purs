module PAB.ApiClient where

--------------------------------------------------------------------------------

import Affjax as AX
import Affjax.RequestBody as AJRB
import Affjax.ResponseFormat as AJRF
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
  :: forall t
  . PABConfig
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
  res <- AX.get AJRF.json url 
  case res of
    Left e     -> pure $ Left (RequestError $ e)
    Right res' -> do
      case A.decodeJson res'.body of
        Left e' -> pure $ Left (DecodeError $ e')
        Right decoded -> pure $ Right decoded