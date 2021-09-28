module AppAff where

import Prelude
import Affjax (Response, defaultRequest)
import Affjax.RequestBody (RequestBody, string)
import Capability.Contract (class Contract, ContractId(..), Endpoint(..), APIError(..))
import Capability.LogMessages (class LogMessages)
import Capability.PollContract (class PollContract)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Reader.Trans (class MonadAsk, ReaderT, asks, runReaderT)
import Data.Bifunctor (bimap)
import Data.Either (Either)
import Data.HTTP.Method (fromString)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff, Milliseconds(..), delay)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as Console
import Foreign.Generic (class Decode, decode, encodeJSON)
import Servant.PureScript.Ajax (AjaxError(..), ErrorDescription(..), ajax)
import Type.Equality (class TypeEquals, from)

type Env
  = { host :: String, port :: Int }

newtype AppM a
  = AppM (ReaderT Env Aff a)

derive newtype instance functorAppM :: Functor AppM

derive newtype instance applyAppM :: Apply AppM

derive newtype instance applicativeAppM :: Applicative AppM

derive newtype instance bindAppM :: Bind AppM

derive newtype instance monadAppM :: Monad AppM

derive newtype instance monadEffectAppM :: MonadEffect AppM

derive newtype instance monadAffAppM :: MonadAff AppM

instance monadAskAppM :: TypeEquals e Env => MonadAsk e AppM where
  ask = AppM $ asks from

runAppM :: Env -> AppM ~> Aff
runAppM env (AppM m) = runReaderT m env

instance logMessagesAppM :: LogMessages AppM where
  logInfo = Console.log >>> liftEffect
  logError = Console.error >>> liftEffect

errorToString :: AjaxError -> String
errorToString (AjaxError e) = case e.description of
  ResponseError _ r -> "Response error: " <> r
  ResponseFormatError s -> "Parsing error: " <> s
  DecodingError s -> "Decoding error: " <> s
  ConnectionError s -> "Connection error: " <> s
  NotFound -> "Not found"

getBaseURL :: Env -> String
getBaseURL { host, port } = "http://" <> host <> ":" <> (show port)

runAjax :: forall m a. Monad m => ExceptT AjaxError m (Response a) -> m (Either APIError a)
runAjax = (map toCustom) <<< runExceptT
  where
  toCustom = bimap (AjaxCallError <<< errorToString) (\r -> r.body)

get :: forall m a. MonadAsk Env m => MonadAff m => Decode a => String -> m (Either APIError a)
get path = do
  url <- asks ((_ <> path) <<< getBaseURL)
  let
    affReq = defaultRequest { method = fromString "GET", url = url }
  runAjax $ ajax decode affReq

post :: forall m a. MonadAsk Env m => MonadAff m => Decode a => String -> RequestBody -> m (Either APIError a)
post path body = do
  url <- asks ((_ <> path) <<< getBaseURL)
  let
    affReq = defaultRequest { method = fromString "POST", url = url, content = Just body }
  runAjax $ ajax decode affReq

instance contractAppM :: Contract AppM where
  getContracts = get "/api/contract/instances"
  getContractStatus (ContractId cid) = get $ "/api/contract/instance/" <> cid <> "/status"
  callEndpoint (Endpoint endpoint) (ContractId cid) params = post ("/api/contract/instance/" <> cid <> "/endpoint/" <> endpoint) (string <<< encodeJSON $ params)

instance pollContractAppM :: PollContract AppM where
  pollDelay = liftAff <<< delay <<< Milliseconds $ 1000.0
  tooManyRetries retryCount = pure $ retryCount > 20
