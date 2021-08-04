module PAB.Api
  ( walletInstances
  , callEndpoint
  , getStatus
  -- , contractDefinitions
  -- , activateContract
  , PABConnectionInfo
  )
where

--------------------------------------------------------------------------------

import Affjax (Error, Response, URL, defaultRequest)
import Affjax as AX
import Affjax.RequestBody as AJRB
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.RequestHeader as RequestHeader
import Affjax.ResponseFormat (ResponseFormat)
import Affjax.ResponseFormat as AJRF
import Control.Applicative ((<$>))
import Control.Monad.Except (runExcept)
import Data.Argonaut as A
import Data.Array (any)
import Data.Array as Arr
import Data.Either (Either(..))
import Data.Eq (eq, (/=))
import Data.Function (on)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.MediaType.Common (applicationJSON)
import Data.Newtype (unwrap)
import Data.String (codePointFromChar, drop, takeWhile)
import Data.String.Common (joinWith)
import Effect.Aff (Aff)
import Error as Error
import Foreign (Foreign)
import Foreign.Class (class Decode, class Encode, encode)
import Foreign.Generic (decodeJSON, encodeJSON)
import PAB.Types (ContractActivationArgs, ContractInstanceClientState, ContractInstanceId)
import Prelude
import Wallet.Emulator.Wallet (Wallet)
import Debug.Trace

--------------------------------------------------------------------------------

type PABConnectionInfo =
    { baseURL :: String
    }

-- Sadly, UUID library is a little too opaque for our needs
formatCID :: ContractInstanceId -> String
formatCID cId =
  takeWhile (\x -> x /= codePointFromChar ')')
    $ drop 6
    $ show
    $ cId.unContractInstanceId


walletInstances 
  :: PABConnectionInfo 
  -> Wallet 
  -> Aff (Array ContractInstanceClientState)
walletInstances pab { getWallet } =
  let
    url = 
      joinWith ""
        [ pab.baseURL
        , "/api/new/contract/instances/wallet/"
        , show getWallet
        ]
  in
  getJSON url
callEndpoint 
  :: forall payload
   . A.EncodeJson payload 
  => PABConnectionInfo
  -> ContractInstanceId 
  -> String
  -> payload
  -> Aff A.Json
callEndpoint pab { unContractInstanceId } endpoint payload = do
  let 
    url = 
      joinWith ""
        [ pab.baseURL
        , "/api/new/contract/instance/" 
        , unContractInstanceId 
        , "/endpoint/" 
        , endpoint
        ]
  traceM "PAB API line 91"
  postJSON url payload
getStatus
  :: PABConnectionInfo
  -> ContractInstanceId
  -> Aff ContractInstanceClientState
getStatus pab { unContractInstanceId } =
  let
    url =
      joinWith ""
        [ pab.baseURL
        , "/api/new/contract/instance/"
        , unContractInstanceId
        , "/status"
        ]
  in
  getJSON url

-- waitForStateChange
--   :: PABConnectionInfo
--   -> ContractInstanceId
--   -> Foreign
--   -> Aff Foreign
-- waitForStateChange pab id lastKnownState = do
--   status <- getStatus pab id
--   if encode (unwrap (unwrap status).cicCurrentState).observableState == lastKnownState then do
--     _ <- delay (Milliseconds 1000.0)
--     waitForStateChange pab id lastKnownState
--   else
--     pure (encode $ (unwrap (unwrap status).cicCurrentState).observableState)

getJSON 
  :: forall resp
   . A.DecodeJson resp 
  => String 
  -> Aff resp
getJSON url = do
  result <- AX.get AJRF.json url
  case result of
    Left e -> Error.throwAffJaxError ("While calling GET on '" <> url <> "'") e
    Right response -> do
      case A.decodeJson response.body of
        Left e -> Error.throwDecodeError "While decoding a GET response body" e
        Right contractInstanceRef -> pure contractInstanceRef
postJSON 
  :: forall payload resp
   . A.EncodeJson payload 
  => A.DecodeJson resp 
  => String 
  -> payload 
  -> Aff resp
postJSON url payload = do
  let jsonPayload = A.encodeJson payload
  _ <- pure $ spy "payload json" jsonPayload
  result <- AX.post AJRF.json url $ Just $ AJRB.Json $ A.encodeJson payload
  _ <- pure $ spy "result" result
  case result of
    Left err -> Error.throwAffJaxError ("While calling POST on '" <> url <> "'") err
    Right response -> do
      case A.decodeJson response.body of
        Left e -> Error.throwDecodeError "While decoding a POST response body" e
        Right contractInstanceRef -> pure contractInstanceRef


addHeader :: Maybe RequestHeader -> Array RequestHeader -> Array RequestHeader
addHeader mh hs = case mh of
  Just h | not $ any (on eq RequestHeader.name h) hs -> hs `Arr.snoc` h
  _ -> hs