module Business.Aave where

import Prelude
import Capability.Contract (class Contract, APIError, ContractId(..), Endpoint, getContracts)
import Capability.PollContract (class PollContract, LeftPoll(..), PollError, PollResponse, pollEndpoint)
import Control.Monad.Except (runExcept, throwError, withExcept)
import Data.Either (Either)
import Data.Json.JsonUUID (JsonUUID(..))
import Data.Lens (Prism', preview)
import Data.Maybe (Maybe, maybe)
import Data.RawJson (RawJson(..))
import Foreign.Generic (class Decode, class Encode, decodeJSON)
import Plutus.Abstract.ContractResponse (ContractResponse(..))
import Plutus.PAB.Events.ContractInstanceState (PartiallyDecodedResponse(..))
import Plutus.PAB.Simulation (AaveContracts)
import Plutus.PAB.Webserver.Types (ContractInstanceClientState(..))
import Wallet.Types (ContractInstanceId(..))
import Data.UUID (toString) as UUID

getAaveContracts :: forall m. Contract m => m (Either APIError (Array (ContractInstanceClientState AaveContracts)))
getAaveContracts = getContracts

getAaveResponseWith ::
  forall m a p s.
  PollContract m =>
  Encode p =>
  Decode s =>
  Show s =>
  Endpoint ->
  Prism' s a ->
  ContractId ->
  p ->
  m (Either PollError a)
getAaveResponseWith endpoint pick cid param = pollEndpoint getNext endpoint param cid
  where
  getNext :: ContractInstanceClientState AaveContracts -> PollResponse a
  getNext (ContractInstanceClientState { cicCurrentState: PartiallyDecodedResponse { observableState: RawJson s } }) =
    runExcept
      $ do
          (contractResponse :: ContractResponse String s) <- withExcept (ResponseError <<< show) (decodeJSON s)
          case contractResponse of
            ContractPending -> throwError Continue
            ContractError e -> throwError <<< ResponseError $ e
            ContractSuccess state ->
              maybe
                (throwError <<< ResponseError $ "Invalid state: " <> (show state))
                pure
                (preview pick state)

getAaveContractId :: forall a. Prism' AaveContracts a -> ContractInstanceClientState AaveContracts -> Maybe ContractId
getAaveContractId pick (ContractInstanceClientState { cicContract, cicDefintion }) = (const $ toContractIdParam cicContract) <$> (preview pick cicDefintion)

toContractIdParam :: ContractInstanceId -> ContractId
toContractIdParam (ContractInstanceId { unContractInstanceId: JsonUUID uuid }) = ContractId <<< UUID.toString $ uuid
