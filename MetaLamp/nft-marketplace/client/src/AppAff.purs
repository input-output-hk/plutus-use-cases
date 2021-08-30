module AppAff where

import Capability.Navigate
import Data.Route
import Prelude
import Utils.APIError
import Affjax (Error, Response, defaultRequest, printError, request)
import Affjax.RequestBody (RequestBody, formData, string)
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat as ResponseFormat
import Capability.Contract (class Contract, ContractId(..), Endpoint(..))
import Capability.IPFS as IPFS
import Capability.LogMessages (class LogMessages)
import Capability.PollContract (class PollContract)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Reader.Trans (class MonadAsk, ReaderT, asks, runReaderT)
import Data.Argonaut.Core (stringify)
import Data.Bifunctor (bimap)
import Data.Either (Either(..))
import Data.HTTP.Method (fromString)
import Data.Maybe (Maybe(..))
import Data.MediaType.Common (multipartFormData)
import Data.Newtype (wrap)
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), delay, launchAff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as Console
import Foreign.Generic (class Decode, Foreign, F, decode, encodeJSON)
import Foreign.NullOrUndefined (undefined)
import Routing.Duplex as Routing
import Routing.Hash as Routing
import Servant.PureScript.Ajax (AjaxError(..), ErrorDescription(..), ajax)
import Type.Equality (class TypeEquals, from)
import Web.File.File as FIle
import Web.File.File as File
import Web.XHR.FormData as FormData

type Env
  = { ipfsServer :: ServerInfo, pabServer :: ServerInfo }

type ServerInfo
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
  ResponseError s r -> "Response error: " <> r <> ". with status: " <> show s
  ResponseFormatError s -> "Parsing error: " <> s
  DecodingError s -> "Decoding error: " <> s
  ConnectionError s -> "Connection error: " <> s
  NotFound -> "Not found"

getBaseURL :: ServerInfo -> String
getBaseURL { host, port } = "http://" <> host <> ":" <> (show port)

runAjax :: forall m a. Monad m => ExceptT AjaxError m (Response a) -> m (Either APIError a)
runAjax = (map toCustom) <<< runExceptT
  where
  toCustom = bimap (AjaxCallError <<< errorToString) (\r -> r.body)

getC :: forall m a. MonadAsk Env m => MonadAff m => Decode a => String -> m (Either APIError a)
getC path = do
  url <- asks ((_ <> path) <<< getBaseURL <<< _.pabServer)
  let
    affReq = defaultRequest { method = fromString "GET", url = url }
  runAjax $ ajax decode affReq

postC :: forall m a. MonadAsk Env m => MonadAff m => Decode a => String -> RequestBody -> m (Either APIError a)
postC path body = do
  url <- asks ((_ <> path) <<< getBaseURL <<< _.pabServer)
  let
    affReq = defaultRequest { method = fromString "POST", url = url, content = Just body }
  runAjax $ ajax decode affReq

catIpfs :: forall m. MonadAsk Env m => MonadAff m => String -> m (Either APIError String)
catIpfs path = do
  url <- asks ((_ <> path) <<< getBaseURL <<< _.ipfsServer)
  let
    affReq = defaultRequest { method = fromString "POST", url = url, responseFormat = ResponseFormat.string }
  map run $ liftAff $ request affReq
  where
  run :: Either Error (Response String) -> Either APIError String
  run = bimap (AjaxCallError <<< printError) _.body

fileToFormData :: File.File -> Effect FormData.FormData
fileToFormData file = do
  fd <- FormData.new
  let
    fileName = File.name file
  FormData.setBlob (wrap fileName) (File.toBlob file) (Just $ wrap fileName) fd
  pure fd

pinIpfs :: forall m. MonadAsk Env m => MonadAff m => String -> File.File -> m (Either APIError String)
pinIpfs path file = do
  url <- asks ((_ <> path) <<< getBaseURL <<< _.ipfsServer)
  content <- liftEffect $ fileToFormData file
  let
    affReq = defaultRequest { method = fromString "POST", url = url, content = Just $ formData content, headers = [ ContentType $ wrap "multipart/form-data; boundary=------------------------35e207f1e7c6a4aa" ] }
  map (map _."Hash") $ runAjax $ ajax decodeResp affReq
  where
  decodeResp :: Foreign -> F { "Hash" :: String, "Name" :: String, "Size" :: String }
  decodeResp = decode

instance contractAppM :: Contract AppM where
  getContracts = getC "/api/new/contract/instances"
  getContractStatus (ContractId cid) = getC $ "/api/new/contract/instance/" <> cid <> "/status"
  callEndpoint (Endpoint endpoint) (ContractId cid) params = postC ("/api/new/contract/instance/" <> cid <> "/endpoint/" <> endpoint) (string <<< encodeJSON $ params)

instance pollContractAppM :: PollContract AppM where
  pollDelay = liftAff <<< delay <<< Milliseconds $ 1000.0
  tooManyRetries retryCount = pure $ retryCount > 20

instance navigateAppM :: Navigate AppM where
  navigate = liftEffect <<< Routing.setHash <<< Routing.print routeCodec

instance ipfsAppM :: IPFS.IPFS AppM where
  pinFile = pinIpfs "/api/v0/add"
  catFile cid = catIpfs ("/api/v0/cat?arg=" <> cid)
