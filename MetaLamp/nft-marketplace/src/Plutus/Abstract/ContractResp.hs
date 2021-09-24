module Plutus.Abstract.ContractResp where

import qualified Data.Map.Strict            as Map
import           Data.Text                  (Text)
import           Plutus.Abstract.RemoteData (RemoteData)

newtype ContractResp a = ContractResp
  { getContractResponses :: Map.Map String (RemoteData Text a)
  }
