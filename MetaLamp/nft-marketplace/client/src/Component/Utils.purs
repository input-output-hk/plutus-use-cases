module Component.Utils where

import Prelude
import Business.MarketplaceInfo (InfoContractId)
import Capability.LogMessages (class LogMessages, logError)
import Data.Either (Either)
import Data.Lens (Lens')
import Data.UserInstance (UserInstance)
import Halogen as H
import Network.RemoteData (RemoteData(..))
import Utils.WithRemoteData (runRDWith)

type PageInput
  = { userInstance :: UserInstance
    , infoInstance :: InfoContractId
    }

runRD ::
  forall e a s action slots output m.
  LogMessages m =>
  Show e =>
  (Lens' s (RemoteData e a)) ->
  H.HalogenM s action slots output m (Either e a) ->
  H.HalogenM s action slots output m Unit
runRD selector action =
  (runRDWith selector $ action)
    >>= case _ of
        Failure e -> logError <<< show $ e
        _ -> pure unit
