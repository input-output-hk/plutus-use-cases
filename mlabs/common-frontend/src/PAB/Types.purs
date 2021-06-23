module PAB.Types
  ( ActiveEndpoint
  , ApiError(..)
  , CombinedWSStreamToClient
  , CombinedWSStreamToServer
  , ContractActivationArgs
  , ContractInstanceClientState
  , ContractInstanceId(..)
  , ContractRequest
  , ContractSignatureResponse
  , CurrencySymbol(..)
  , EndpointDescription(..)
  , FormSchema(..)
  , FunctionSchema
  , InstanceStatusToClient
  , IterationID(..)
  , LogLevel(..)
  , LogMessage
  , lovelaceValueOf
  , PabConfig
  , PartiallyDecodedResponse
  , RequestID(..)
  , Slot
  , TokenName(..)
  , Value(..)
  , Wallet(..)
  ) where

--------------------------------------------------------------------------------
import Affjax as AX
import Data.Argonaut as A
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple(..))

--------------------------------------------------------------------------------
-- TODO: Use generated types from plutus-pab PSGenerator if possible
-- Custom types
type PabConfig
  = { baseUrl :: String
    }

data ApiError
  = RequestError AX.Error
  | DecodeError A.JsonDecodeError

-- Types from Control.Monad.Freer.Extras.Log
type LogMessage
  = { _logLevel :: LogLevel, _logMessageContent :: A.Json }

data LogLevel
  = Debug
  | Info
  | Notice
  | Warning
  | Error
  | Critical
  | Alert
  | Emergency

derive instance genericLogLevel :: Generic LogLevel _

instance encodeJsonLogLevel :: EncodeJson LogLevel where
  encodeJson a = genericEncodeJson a

instance decodeJsonLogLevel :: DecodeJson LogLevel where
  decodeJson a = genericDecodeJson a

-- Types from Plutus.Contract.Effects.ExposeEndpoint
type ActiveEndpoint
  = { aeDescription :: EndpointDescription -- ^ The name of the endpoint
    , aeMetadata :: Maybe A.Json -- ^ Data that should be shown to the user
    }

-- Types from Plutus.PAB.Events.Contract
newtype ContractInstanceId
  = ContractInstanceId String

derive instance genericContractInstanceId :: Generic (ContractInstanceId) _

instance encodeJsonContractInstanceId :: EncodeJson (ContractInstanceId) where
  encodeJson a = genericEncodeJson a

instance decodeJsonContractInstanceId :: DecodeJson (ContractInstanceId) where
  decodeJson a = genericDecodeJson a

-- types from Plutus.PAB.Events.ContractInstanceState
type PartiallyDecodedResponse v
  = { hooks :: Array (ContractRequest v)
    , logs :: Array LogMessage
    , lastLogs :: Array LogMessage -- The log messages returned by the last step ('lastLogs' is a suffix of 'logs')
    , err :: Maybe A.Json
    , observableState :: A.Json
    }

-- Types from Plutus.PAB.Webserver.Types
type ContractSignatureResponse t
  = { csrDefinition :: t
    , csrSchemas :: Array (FunctionSchema FormSchema)
    }

-- | Data needed to start a new instance of a contract.
type ContractActivationArgs t
  = { caID :: t -- ^ ID of the contract
    , caWallet :: Wallet -- ^ Wallet that should be used for this instance
    }

-- | Current state of a contract instance
type ContractInstanceClientState t
  = { cicContract :: ContractInstanceId
    , cicCurrentState :: PartiallyDecodedResponse ActiveEndpoint
    , cicWallet :: Wallet
    , cicDefintion :: t
    }

-- | Status updates for contract instances streamed to client
data InstanceStatusToClient
  = NewObservableState A.Json -- ^ The observable state of the contract has changed.
  | NewActiveEndpoints (Array ActiveEndpoint) -- ^ The set of active endpoints has changed.
  | ContractFinished (Maybe A.Json) -- ^ Contract instance is done with an optional error message.

-- | Data sent to the client through the combined websocket API
data CombinedWSStreamToClient
  = InstanceUpdate ContractInstanceId InstanceStatusToClient
  | SlotChange Slot -- ^ New slot number
  | WalletFundsChange Wallet Value -- ^ The funds of the wallet have changed

-- | Instructions sent to the server through the combined websocket API
data CombinedWSStreamToServer
  = Subscribe (Either ContractInstanceId Wallet)
  | Unsubscribe (Either ContractInstanceId Wallet)

-- Types from Playground.Types
type FunctionSchema a
  = { endpointDescription :: EndpointDescription
    , argument :: a
    -- ^ All contract endpoints take a single argument. (Multiple arguments must be wrapped up into a container.)
    }

-- types from Plutus.Contract.Resumable
newtype RequestID
  = RequestID Int

derive instance genericRequestID :: Generic RequestID _

instance encodeJsonRequestID :: EncodeJson RequestID where
  encodeJson a = genericEncodeJson a

instance decodeJsonRequestID :: DecodeJson RequestID where
  decodeJson a = genericDecodeJson a

newtype IterationID
  = IterationID Int

derive instance genericIterationID :: Generic IterationID _

instance encodeJsonIterationID :: EncodeJson IterationID where
  encodeJson a = genericEncodeJson a

instance decodeJsonIterationID :: DecodeJson IterationID where
  decodeJson a = genericDecodeJson a

type ContractRequest v
  = { rqID :: RequestID
    , itID :: IterationID
    , rqRequest :: v
    }

-- Types from Plutus.V1.Ledger.Slot
newtype Slot
  = Slot Int

-- Types from Plutus.V1.Ledger.Value
newtype CurrencySymbol
  = CurrencySymbol String

derive instance genericCurrencySymbol :: Generic CurrencySymbol _

instance encodeJsonCurrencySymbol :: EncodeJson CurrencySymbol where
  encodeJson a = genericEncodeJson a

instance decodeJsonCurrencySymbol :: DecodeJson CurrencySymbol where
  decodeJson a = genericDecodeJson a

newtype TokenName
  = TokenName String

derive instance genericTokenName :: Generic TokenName _

instance encodeJsonTokenName :: EncodeJson TokenName where
  encodeJson a = genericEncodeJson a

instance decodeJsonTokenName :: DecodeJson TokenName where
  decodeJson a = genericDecodeJson a

newtype Value
  = Value (Array (Tuple CurrencySymbol (Array (Tuple TokenName Int))))

derive instance genericValue :: Generic Value _

instance encodeJsonValue :: EncodeJson Value where
  encodeJson a = genericEncodeJson a

instance decodeJsonValue :: DecodeJson Value where
  decodeJson a = genericDecodeJson a

lovelaceValueOf :: Int -> Value
lovelaceValueOf lovelace = Value [ Tuple (CurrencySymbol "") [ Tuple (TokenName "") lovelace ] ]

-- Types from Schema 
data FormSchema
  = FormSchemaUnit
  | FormSchemaBool
  | FormSchemaInt
  | FormSchemaInteger
  | FormSchemaString
  | FormSchemaHex
  -- ^ A string that may only contain @0-9a-fA-F@
  | FormSchemaArray FormSchema
  | FormSchemaMaybe FormSchema
  | FormSchemaRadio (Array String)
  -- ^ A radio button with a list of labels.
  | FormSchemaTuple FormSchema FormSchema
  | FormSchemaObject (Array (Tuple String FormSchema))
  -- Blessed types that get their own special UI widget.
  | FormSchemaValue
  | FormSchemaSlotRange
  -- Exceptions.
  | FormSchemaUnsupported String

derive instance genericFormSchema :: Generic FormSchema _

instance encodeJsonFormSchema :: EncodeJson FormSchema where
  encodeJson a = genericEncodeJson a

instance decodeJsonFormSchema :: DecodeJson FormSchema where
  decodeJson a = genericDecodeJson a

-- Types from Wallet.Emulator.Wallet
newtype Wallet
  = Wallet Int

derive instance genericWallet :: Generic Wallet _

instance encodeJsonWallet :: EncodeJson Wallet where
  encodeJson a = genericEncodeJson a

instance decodeJsonWallet :: DecodeJson Wallet where
  decodeJson a = genericDecodeJson a

-- Types from Wallet.Types
newtype EndpointDescription
  = EndpointDescription String

derive instance genericEndpointDescription :: Generic EndpointDescription _

instance encodeJsonEndpointDescription :: EncodeJson EndpointDescription where
  encodeJson a = genericEncodeJson a

instance decodeJsonEndpointDescription :: DecodeJson EndpointDescription where
  decodeJson a = genericDecodeJson a
