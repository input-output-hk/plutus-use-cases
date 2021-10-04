module Business.Marketplace where

import Prelude
import Utils.APIError
import Capability.Contract (class Contract, ContractId(..), Endpoint, getContracts)
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
import Plutus.Abstract.ContractResponse (ContractResponse(..))
import Plutus.Abstract.RemoteData as PRD
import Plutus.PAB.Events.ContractInstanceState (PartiallyDecodedResponse(..))
import Plutus.PAB.Simulation (MarketplaceContracts)
import Plutus.PAB.Webserver.Types (ContractInstanceClientState(..))
import Wallet.Types (ContractInstanceId(..))

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
          (contractResponse :: ContractResponse String s) <- withExcept (ResponseError <<< show) (decodeJSON s)
          case lookup (unwrap endpoint) (unwrap contractResponse).getEndpointResponses of
            Just (PRD.Failure e) -> throwError <<< ResponseError $ e
            Just (PRD.Success state) ->
              maybe
                (throwError <<< ResponseError $ "Invalid state: " <> (show state))
                pure
                (preview pick state)
            _ -> throwError Continue

getMarketplaceContractId :: forall a. Prism' MarketplaceContracts a -> ContractInstanceClientState MarketplaceContracts -> Maybe ContractId
getMarketplaceContractId pick (ContractInstanceClientState st) = (const $ toContractIdParam st.cicContract) <$> (preview pick st.cicDefinition)

toContractIdParam :: ContractInstanceId -> ContractId
toContractIdParam (ContractInstanceId { unContractInstanceId: JsonUUID uuid }) = ContractId <<< UUID.toString $ uuid
