module Capability.Contract where

import Prelude
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Foreign (unsafeToForeign)
import Foreign.Generic (class Decode, class Encode)
import Halogen (HalogenM, lift)
import Plutus.PAB.Webserver.Types (ContractInstanceClientState)
import Utils.APIError

newtype ContractId
  = ContractId String

derive newtype instance showContractId :: Show ContractId

newtype Endpoint
  = Endpoint String

derive newtype instance showEndpoint :: Show Endpoint

data ContractUnit
  = ContractUnit

instance encodeContractUnit :: Encode ContractUnit where
  encode value = unsafeToForeign []

class
  Monad m <= Contract m where
  getContracts :: forall a. Decode a => m (Either APIError (Array (ContractInstanceClientState a)))
  callEndpoint :: forall a. Encode a => Endpoint -> ContractId -> a -> m (Either APIError Unit)
  getContractStatus :: forall a. Decode a => ContractId -> m (Either APIError (ContractInstanceClientState a))

instance contractHalogenM :: Contract m => Contract (HalogenM st act slots msg m) where
  getContracts = lift getContracts
  callEndpoint endpoint cid params = lift $ callEndpoint endpoint cid params
  getContractStatus = getContractStatus >>> lift
