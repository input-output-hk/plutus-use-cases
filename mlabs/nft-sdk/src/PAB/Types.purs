module PAB.Types
  ( ActiveContract
  , ContractInstanceRef
  , ContractState
  , ContractInstanceId
  , ContractActivationArgs
  , BaseURL
  , Wallet
  , ContractInstanceClientState
  , ActiveEndpoint
  , EndpointDescription
  , PartiallyDecodedResponse
  , ContractRequest
  , TokenName
  , Value
  , lovelaceValueOf
  , CurrencySymbol

  , HaskellUnit
  , haskellUnit
  , swapUnitTypes
  )
where

--------------------------------------------------------------------------------

import Prelude

import Data.Argonaut as A
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))

--------------------------------------------------------------------------------

type CurrencySymbol = { unCurrencySymbol :: String }

-- We need to use BigInt here.
type Value = { getValue :: Array (Tuple CurrencySymbol (Array (Tuple TokenName Int))) }

lovelaceValueOf :: Int -> Value
lovelaceValueOf lovelace =
    { getValue: [ Tuple { unCurrencySymbol: "" } [ Tuple { unTokenName: "" } lovelace ] ] }

type TokenName = { unTokenName :: String }

type BaseURL = String

type ContractActivationArgs = { contractPath :: String }

type Wallet = { getWallet :: Int }

type ContractInstanceId = { unContractInstanceId :: String }

type ContractState =  A.Json -- really, just a way to deal with the foreign value without figuring this out right now.

type ContractInstanceRef =
  { csContract           :: ContractInstanceId
  , csContractDefinition :: ContractActivationArgs
  , csCurrentIteration   :: Int
  , csCurrentState       :: PartiallyDecodedResponse ActiveEndpoint
  }

type EndpointDescription = { getEndpointDescription :: String }

type ActiveEndpoint =
  { aeDescription :: EndpointDescription
  }

type ContractRequest o =
    { rqRequest :: o
    }

type PartiallyDecodedResponse v =
  { hooks           :: Array (ContractRequest v)
  , observableState :: A.Json
  -- NOTE: incomplete
  }

type ContractInstanceClientState =
    { cicContract     :: ContractInstanceId
    , cicWallet       :: Wallet
    , cicCurrentState :: PartiallyDecodedResponse ActiveEndpoint
    , cicDefintion    :: ContractState
    }

type ActiveContract =
    { contractInstanceRef :: ContractInstanceRef
    , baseURL :: BaseURL
    }


--------------------------------------------------------------------------------
data HaskellUnit = HaskellUnit

instance decodeJsonHaskellUnit :: A.DecodeJson HaskellUnit where
  decodeJson = A.caseJsonArray (Left $ "expected []")
    (\array -> case array of
      [] -> Right HaskellUnit
      _  -> Left $ "expected empty []"
      )

instance encodeJsonHaskellUnit :: A.EncodeJson HaskellUnit where
  encodeJson _ = A.fromArray []

haskellUnit :: HaskellUnit
haskellUnit = HaskellUnit

swapUnitTypes :: HaskellUnit -> Unit
swapUnitTypes = const unit