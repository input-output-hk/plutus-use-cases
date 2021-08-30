module Capability.IPFS where

import Prelude
import Utils.APIError

import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Foreign (unsafeToForeign)
import Foreign.Generic (class Decode, class Encode)
import Halogen (HalogenM, lift)
import Plutus.PAB.Webserver.Types (ContractInstanceClientState)
import Web.File.File (File)

type IpfsCid
  = String

class
  Monad m <= IPFS m where
  pinFile :: File -> m (Either APIError IpfsCid)
  catFile :: IpfsCid -> m (Either APIError String)

instance contractHalogenM :: IPFS m => IPFS (HalogenM st act slots msg m) where
  pinFile = pinFile >>> lift
  catFile = catFile >>> lift
