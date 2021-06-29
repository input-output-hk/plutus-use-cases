module PAB.Types
  ( ActiveEndpoint
  , CombinedWSStreamToClient
  , CombinedWSStreamToServer
  , ContractActivationArgs
  , ContractCall(..)
  , ContractDefinition(..)
  , ContractInstanceClientState
  , ContractInstanceId(..)
  , ContractRequest
  , ContractSignatureResponse
  , Closure
  , CurrencySymbol(..)
  , defaultValue
  , EndpointDescription(..)
  , Extended(..)
  , Fix(..)
  , FormArgument(..)
  , FormArgumentF(..)
  , FormSchema(..)
  , FunctionSchema
  , InstanceStatusToClient
  , Interval
  , IterationID(..)
  , LogLevel(..)
  , LogMessage
  , LowerBound(..)
  , lovelaceValueOf
  , PabConfig
  , PartiallyDecodedResponse
  , RequestID(..)
  , Slot
  , taggedJsonEncoding
  , toArgument
  , TokenName(..)
  , UpperBound(..)
  , Value(..)
  , Wallet(..)
  ) where

--------------------------------------------------------------------------------

import Prelude

import Data.Argonaut (JsonDecodeError(..))
import Data.Argonaut as A
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.Argonaut.Decode.Error (JsonDecodeError(TypeMismatch, UnexpectedValue))
import Data.Argonaut.Decode.Generic (genericDecodeJson, genericDecodeJsonWith)
import Data.Argonaut.Encode.Class (class EncodeJson, encodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson, genericEncodeJsonWith)
import Data.Argonaut.Types.Generic (Encoding)
import Data.BigInt (BigInt)
import Data.Either (Either(..))
import Data.Eq (class Eq, class Eq1)
import Data.Functor (class Functor)
import Data.Generic.Rep (class Generic)
import Data.Json.JsonTuple (JsonTuple(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Newtype (unwrap)
import Data.Show (class Show)
import Data.Show.Generic (genericShow)
import Data.String.Extra as String
import Data.Traversable (sequence, traverse)
import Data.Tuple (Tuple(..))
import Foreign (Foreign)
import Foreign.Object (lookup)
import Foreign.Object as FO
import Matryoshka (class Corecursive, class Recursive, Algebra, ana, cata)
import Web.HTML.Event.EventTypes (offline)

--------------------------------------------------------------------------------
-- Custom types
type PabConfig
  = { baseUrl :: String
    }

-- PAB and Playground types

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

type ActiveEndpoint
  = { aeDescription :: EndpointDescription -- ^ The name of the endpoint
    , aeMetadata :: Maybe A.Json -- ^ Data that should be shown to the user
    }

type ContractInstanceId
  = { unContractInstanceId :: String }

type PartiallyDecodedResponse v
  = { hooks :: Array (ContractRequest v)
    , logs :: Array LogMessage
    , lastLogs :: Array LogMessage -- The log messages returned by the last step ('lastLogs' is a suffix of 'logs')
    , err :: Maybe A.Json
    , observableState :: A.Json
    }

type ContractSignatureResponse
  = { csrDefinition :: String
    , csrSchemas :: Array (FunctionSchema FormSchema)
    }

newtype ContractDefinition = ContractDefinition String

derive instance genericContractDefinition :: Generic ContractDefinition _

instance encodeJsonContractDefinition :: EncodeJson ContractDefinition where
  encodeJson a = genericEncodeJson a

instance decodeJsonContractDefinition :: DecodeJson ContractDefinition where
  decodeJson a = genericDecodeJson a

type FunctionSchema a =
  { endpointDescription :: EndpointDescription
  , argument            :: a
  -- ^ All contract endpoints take a single argument. (Multiple arguments must be wrapped up into a container.)
  }

type EndpointDescription
  = { getEndpointDescription :: String }

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
  | FormSchemaObject (Array (JsonTuple String FormSchema))
  -- Blessed types that get their own special UI widget.
  | FormSchemaValue
  -- FormSchemaSlotRange
  | FormSchemaPOSIXTimeRange
  -- Exceptions.
  | FormSchemaUnsupported String

derive instance genericFormSchema :: Generic FormSchema _

derive instance eqFormSchema :: Eq FormSchema

derive instance ordFormSchema :: Ord FormSchema

instance showFormSchema :: Show FormSchema where
  show a = genericShow a

instance encodeJsonFormSchema :: EncodeJson FormSchema where
  encodeJson a = genericEncodeJsonWith taggedJsonEncoding a

instance decodeJsonFormSchema :: DecodeJson FormSchema where
  decodeJson a = genericDecodeJsonWith taggedJsonEncoding a

type FormArgument
  = Fix FormArgumentF

-- TODO: Use BigInt for FormIntegerF
data FormArgumentF a
    = FormUnitF
    | FormBoolF Boolean
    | FormIntF (Maybe Int)
    | FormIntegerF (Maybe Int) 
    | FormStringF (Maybe String)
    | FormHexF (Maybe String)
    | FormRadioF (Array String) (Maybe String)
    | FormArrayF FormSchema (Array a)
    | FormMaybeF FormSchema (Maybe a)
    | FormTupleF  a a
    | FormObjectF (Array (JsonTuple String a))
    | FormValueF Value
    | FormPOSIXTimeRangeF Interval
    | FormUnsupportedF String

derive instance functorFormArgumentF :: Functor FormArgumentF

derive instance genericFormArgumentF :: Generic (FormArgumentF a) _

instance encodeJsonFormArgumentF :: EncodeJson a => EncodeJson (FormArgumentF a) where
  encodeJson a = genericEncodeJson a

instance decodeJsonFormArgumentF :: DecodeJson a => DecodeJson (FormArgumentF a) where
  decodeJson a = genericDecodeJson a

-- | This recursive type is isomorphic to `Data.Functor.Mu.Mu`, and
-- only exists because we want `Encode`/`Decode` instances.
newtype Fix f
  = Fix (f (Fix f))

derive instance newtypeFix :: Newtype (Fix f) _

derive instance genericFix :: Generic (Fix f) _

derive instance eqFix :: Eq1 f => Eq (Fix f)

instance showFix :: Show (Fix FormArgumentF) where
  show (Fix a) = genericShow a

instance recursiveFix ∷ Functor f ⇒ Recursive (Fix f) f where
  project (Fix v) = v

instance corecursiveFix ∷ Functor f ⇒ Corecursive (Fix f) f where
  embed v = Fix v

instance encodeJsonFix :: EncodeJson (Fix FormArgumentF) where
  encodeJson (Fix value) = genericEncodeJsonWith taggedJsonEncoding value

instance decodeJsonFix :: DecodeJson (Fix FormArgumentF) where
  decodeJson value = genericDecodeJsonWith taggedJsonEncoding value

--
-- instance showFix :: Show (Fix FormArgumentF) where
--   show value = genericShow value


toArgument :: Value -> FormSchema -> FormArgument
toArgument initialValue = ana algebra
 where
  algebra :: FormSchema -> FormArgumentF FormSchema
  algebra FormSchemaUnit = FormUnitF

  algebra FormSchemaBool = FormBoolF false

  algebra FormSchemaInt = FormIntF Nothing

  algebra FormSchemaInteger = FormIntegerF Nothing

  -- text inputs cannot distinguish between `Nothing` and `Just ""` -
  -- use the latter as the default value, or validation behaves weirdly
  algebra FormSchemaString = FormStringF (Just "")

  algebra FormSchemaHex = FormHexF Nothing

  algebra (FormSchemaRadio xs) = FormRadioF xs Nothing

  algebra (FormSchemaArray xs) = FormArrayF xs []

  algebra (FormSchemaMaybe x) = FormMaybeF x Nothing

  algebra FormSchemaValue = FormValueF $ initialValue

  algebra FormSchemaPOSIXTimeRange = FormPOSIXTimeRangeF defaultTimeRange

  algebra (FormSchemaTuple a b) = FormTupleF a b

  algebra (FormSchemaObject xs) = FormObjectF xs

  algebra (FormSchemaUnsupported x) = FormUnsupportedF x

formArgumentToJson :: FormArgument -> Maybe A.Json
formArgumentToJson = cata algebra
  where
  algebra :: Algebra FormArgumentF (Maybe A.Json)
  algebra FormUnitF = Just $ encodeJson (mempty :: Array Unit)

  algebra (FormBoolF b) = Just $ encodeJson b

  algebra (FormIntF n) = encodeJson <$> n

  algebra (FormIntegerF n) = encodeJson <$> n

  algebra (FormStringF str) = encodeJson <$> str

  algebra (FormRadioF _ option) = encodeJson <$> option

  algebra (FormHexF str) = encodeJson <<< String.toHex <$> str

  algebra (FormTupleF (Just fieldA) (Just fieldB)) = Just $ encodeJson [ fieldA, fieldB ]

  algebra (FormTupleF _ _) = Nothing

  algebra (FormMaybeF _ field) = encodeJson <$> field

  algebra (FormArrayF _ fields) = Just $ encodeJson fields

  algebra (FormObjectF fields) = encodeFields fields
    where
    encodeFields :: Array (JsonTuple String (Maybe A.Json)) -> Maybe A.Json
    encodeFields xs = map (encodeJson <<< FO.fromFoldable) $ prepareObject xs

    prepareObject :: Array (JsonTuple String (Maybe A.Json)) -> Maybe (Array (Tuple String A.Json))
    prepareObject = traverse processTuples

    processTuples :: JsonTuple String (Maybe A.Json) -> Maybe (Tuple String A.Json)
    processTuples = unwrap >>> sequence

  algebra (FormValueF x) = Just $ encodeJson x

  algebra (FormPOSIXTimeRangeF x) = Just $ encodeJson x

  algebra (FormUnsupportedF _) = Nothing

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

type Interval = { ivFrom :: LowerBound, ivTo :: UpperBound }

defaultTimeRange :: Interval
defaultTimeRange =
  { ivFrom: LowerBound NegInf true
  , ivTo: UpperBound PosInf true
  }

data LowerBound = LowerBound Extended Closure

derive instance genericLowerBound :: Generic LowerBound _

instance showLowerBound :: Show LowerBound where
  show a = genericShow a

instance encodeJsonLowerBound :: EncodeJson LowerBound where
  encodeJson a = genericEncodeJson a

instance decodeJsonLowerBound :: DecodeJson LowerBound where
  decodeJson a = genericDecodeJson a

data UpperBound = UpperBound Extended Closure

derive instance genericUpperBound :: Generic UpperBound _

instance showUpperBound :: Show UpperBound where
  show a = genericShow a

instance encodeJsonUpperBound :: EncodeJson UpperBound where
  encodeJson a = genericEncodeJson a

instance decodeJsonUpperBound :: DecodeJson UpperBound where
  decodeJson a = genericDecodeJson a

type Closure = Boolean

data Extended = NegInf | Finite Int | PosInf

derive instance genericExtended :: Generic Extended _

instance showExtended :: Show Extended where
  show a = genericShow a

instance encodeJsonExtended :: EncodeJson Extended where
  encodeJson a = genericEncodeJson a

instance decodeJsonExtended :: DecodeJson Extended where
  decodeJson a = genericDecodeJson a

newtype Slot
  = Slot Int

type CurrencySymbol
  = { unCurrencySymbol :: String }

type TokenName
  = { unTokenName :: String }

type Value
  = { getValue :: Array (JsonTuple CurrencySymbol (Array (JsonTuple TokenName Int))) }

-- derive instance genericValue :: Generic Value _

-- instance showValue :: Show Value where
--   show a = genericShow a

-- instance encodeJsonValue :: EncodeJson Value where
--   encodeJson a = genericEncodeJson a

-- instance decodeJsonValue :: DecodeJson Value where
--   decodeJson a = genericDecodeJson a

lovelaceValueOf :: Int -> Value
lovelaceValueOf lovelace = 
  { getValue: 
      [ JsonTuple $ Tuple 
          { unCurrencySymbol: "" } 
          [ (JsonTuple $ Tuple { unTokenName: "" } lovelace) ] 
      ] 
  }

defaultValue :: Value
defaultValue = lovelaceValueOf 0

type Wallet
  = { getWallet :: Int }


taggedJsonEncoding :: Encoding
taggedJsonEncoding 
  = { tagKey: "tag"
    , valuesKey: "contents"
    , unwrapSingleArguments: true
    }