module Business.Marketplace where

import Prelude
import Utils.APIError
import Capability.Contract (class Contract, ContractId(..), Endpoint(..), getContracts)
import Capability.PollContract (class PollContract, LeftPoll(..), PollError, PollResponse, pollEndpoint)
import Control.Monad.Except (runExcept, throwError, withExcept)
import Data.Either (Either)
import Data.Json.JsonUUID (JsonUUID(..))
import Data.Lens (Prism', preview)
import Data.Map (lookup)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.RawJson (RawJson(..))
import Data.UUID (toString) as UUID
import Foreign.Generic (class Decode, class Encode, decodeJSON)
import Plutus.Abstract.RemoteData as PRD
import Plutus.PAB.Events.ContractInstanceState (PartiallyDecodedResponse(..))
import Plutus.PAB.MarketplaceContracts (MarketplaceContracts)
import Plutus.PAB.Webserver.Types (ContractInstanceClientState(..))
import Wallet.Types (ContractInstanceId(..))
import Plutus.Abstract.ContractResponse (ContractState(..))

getMarketplaceContracts :: forall m. Contract m => m (Either APIError (Array (ContractInstanceClientState MarketplaceContracts)))
getMarketplaceContracts = getContracts

getMarketplaceResponseWith ::
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
getMarketplaceResponseWith endpoint pick cid param = pollEndpoint getNext endpoint param cid
  where
  getNext :: ContractInstanceClientState MarketplaceContracts -> PollResponse a
  getNext (ContractInstanceClientState { cicCurrentState: PartiallyDecodedResponse { observableState: RawJson s } }) =
    runExcept
      $ do
          (fullResponse :: Maybe (ContractState String String s)) <- withExcept (ResponseError <<< show) (decodeJSON s)
          ContractState { endpointName, response } <- maybe (throwError Continue) pure fullResponse
          when (Endpoint endpointName /= endpoint) (throwError <<< ResponseError $ "Endpoint name mismatch")
          case response of
            PRD.Failure e -> throwError <<< ResponseError $ e
            PRD.Success state ->
              maybe
                (throwError <<< ResponseError $ "Invalid state: " <> (show state))
                pure
                (preview pick state)
            _ -> throwError Continue

getMarketplaceContractId :: forall a. Prism' MarketplaceContracts a -> ContractInstanceClientState MarketplaceContracts -> Maybe ContractId
getMarketplaceContractId pick (ContractInstanceClientState st) = (const $ toContractIdParam st.cicContract) <$> (preview pick st.cicDefinition)

toContractIdParam :: ContractInstanceId -> ContractId
toContractIdParam (ContractInstanceId { unContractInstanceId: JsonUUID uuid }) = ContractId <<< UUID.toString $ uuid
