module Middleware.PabClient.API where

import qualified Data.Aeson                 as JSON
import           Data.Text                  (Text)
import           Middleware.PabClient.Types
import           Servant.API                (Capture, Description, Get, JSON,
                                             Post, Put, QueryParam, ReqBody,
                                             (:<|>), (:>))

-- | PAB client API for contracts
type API
    = "api" :> ("healthcheck" :> Description "Is the server alive?" :> Get '[JSON] ()
    :<|> "contract" :>
               "instance" :>
                    (Capture "contract-instance-id" ContractInstanceId :>
                        (    "status"   :> Description "Current status of contract instance." :> Get '[JSON] ContractState
                        :<|> "endpoint" :> Capture "endpoint-name" String :> ReqBody '[JSON] JSON.Value :> Description "Call an endpoint." :> Post '[JSON] ()
                        :<|> "stop"     :> Description "Terminate the instance." :> Put '[JSON] ()
                        )
                    )
        )
