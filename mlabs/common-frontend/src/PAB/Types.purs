module PAB.Types
  ( ActiveEndpoint
  , ApiError(..)
  , CombinedWSStreamToClient
  , CombinedWSStreamToServer
  , ContractActivationArgs
  , ContractCall
  , ContractInstanceClientState
  , ContractInstanceId(..)
  , ContractRequest
  , ContractSignatureResponse
  , CurrencySymbol(..)
  , EndpointDescription(..)
  , FormArgument
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
type ContractInstanceId
  = { unContractInstanceId :: String }

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

-- Types from Playground.Types (modified)

data ContractCall a
  = CallEndpoint
    { caller         :: Wallet
    , argumentValues :: FunctionSchema a
    }
    -- ^ Call one of the defined endpoints of your contract.

  | PayToWallet
    { sender    :: Wallet
    , recipient :: Wallet
    , amount    :: Value
    }
    -- ^ Make a wallet-to-wallet transfer of the specified value.

data FunctionSchema a =
    FunctionSchema
        { endpointDescription :: EndpointDescription
        , argument            :: a
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

-- Types from Pluts.V1.Ledger.Interval (modified)
type Interval = { ivFrom :: LowerBound, ivTo :: UpperBound }

data LowerBound = LowerBound Extended Closure

derive instance genericLowerBound :: Generic LowerBound _

instance encodeJsonLowerBound :: EncodeJson LowerBound where
  encodeJson a = genericEncodeJson a

instance decodeJsonLowerBound :: DecodeJson LowerBound where
  decodeJson a = genericDecodeJson a

data UpperBound = UpperBound Extended Closure

derive instance genericUpperBound :: Generic UpperBound _

instance encodeJsonUpperBound :: EncodeJson UpperBound where
  encodeJson a = genericEncodeJson a

instance decodeJsonUpperBound :: DecodeJson UpperBound where
  decodeJson a = genericDecodeJson a

type Closure = Boolean

data Extended = NegInf | Finite A.Json | PosInf

derive instance genericExtended :: Generic Extended _

instance encodeJsonExtended :: EncodeJson Extended where
  encodeJson a = genericEncodeJson a

instance decodeJsonExtended :: DecodeJson Extended where
  decodeJson a = genericDecodeJson a

-- Types from Plutus.V1.Ledger.Slot
newtype Slot
  = Slot Int

-- Types from Plutus.V1.Ledger.Value
type CurrencySymbol
  = { unCurrencySymbol :: String }

type TokenName
  = { unTokenName :: String }

type Value
  = { getValue :: Array (Tuple CurrencySymbol (Array (Tuple TokenName Int))) }

lovelaceValueOf :: Int -> Value
lovelaceValueOf lovelace = { getValue: [ Tuple { unCurrencySymbol: "" } [ Tuple { unTokenName: "" } lovelace ] ] }

-- Types from playground-common Schema 
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

-- Modified version of original type
data FormArgument
    = FormUnit
    | FormBool Boolean
    | FormInt (Maybe Int)
    | FormInteger (Maybe Int)
    | FormString (Maybe String)
    | FormHex (Maybe String)
    | FormRadio (Array String) (Maybe String)
    | FormArray FormSchema (Array A.Json)
    | FormMaybe FormSchema (Maybe A.Json)
    | FormTuple A.Json A.Json
    | FormObject (Array (Tuple String A.Json))
    | FormValue Value
    | FormPOSIXTimeRange Interval
    | FormUnsupported String

derive instance genericFormArgument :: Generic FormArgument _

instance encodeJsonFormArgument :: EncodeJson FormArgument where
  encodeJson a = genericEncodeJson a

instance decodeJsonFormArgument :: DecodeJson FormArgument where
  decodeJson a = genericDecodeJson a

-- Types from Wallet.Emulator.Wallet
type Wallet
  = { getWallet :: Int }

-- Types from Wallet.Types
type EndpointDescription
  = { getEndpointDescription :: String }
