module Capability.PollContract where

import Prelude
import Capability.Contract (class Contract, APIError(..), ContractId, Endpoint, callEndpoint, getContractStatus)
import Control.Monad.Except (runExceptT, throwError)
import Data.Either (Either(..), either)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Foreign.Generic (class Decode, class Encode)
import Halogen (HalogenM, lift)
import Plutus.PAB.Webserver.Types (ContractInstanceClientState)

class
  Contract m <= PollContract m where
  pollDelay :: m Unit
  tooManyRetries :: Int -> m Boolean

instance pollContractHalogenM :: PollContract m => PollContract (HalogenM st act slots msg m) where
  pollDelay = lift pollDelay
  tooManyRetries = lift <<< tooManyRetries

data PollError
  = TooManyRetries
  | PollAPIError String
  | PollResponseError String

derive instance genericPollError :: Generic PollError _

instance showPollError :: Show PollError where
  show = genericShow

data LeftPoll
  = Continue
  | ResponseError String

type PollResponse a
  = Either LeftPoll a

pollStatus ::
  forall m a c.
  PollContract m =>
  Decode c =>
  (ContractInstanceClientState c -> PollResponse a) ->
  Endpoint ->
  ContractId ->
  m (Either PollError a)
pollStatus = worker 0
  where
  worker retryCount getNext endpoint cid =
    runExceptT
      $ do
          limitExceeded <- lift $ tooManyRetries retryCount
          when limitExceeded $ throwError TooManyRetries
          _ <- lift pollDelay
          status <- lift (getContractStatus cid) >>= either (throwError <<< toPollError) pure
          case getNext status of
            Left Continue -> lift (worker (retryCount + 1) getNext endpoint cid) >>= either throwError pure
            Left (ResponseError e) -> throwError <<< PollResponseError $ e
            Right s -> pure s

pollEndpoint ::
  forall m a c p.
  PollContract m =>
  Encode p =>
  Decode c =>
  (ContractInstanceClientState c -> PollResponse a) ->
  Endpoint ->
  p ->
  ContractId ->
  m (Either PollError a)
pollEndpoint getNext endpoint param cid =
  callEndpoint endpoint cid param
    >>= either (pure <<< Left <<< toPollError) (const $ pollStatus getNext endpoint cid)

toPollError :: APIError -> PollError
toPollError (AjaxCallError e) = PollAPIError e
