module Mlabs.Lending where

-- import qualified PlutusTx.Prelude     as Plutus

import           Data.Aeson
import           Data.Text (Text)
import           GHC.Generics (Generic)

import           Ledger                           hiding (singleton)
-- import           Ledger.Constraints               as Constraints
-- import           Ledger.Constraints.OnChain       as Constraints
-- import           Ledger.Constraints.TxConstraints as Constraints
import           Plutus.Contract
import qualified PlutusTx
import qualified Ledger.Typed.Scripts             as Scripts


import           Playground.Contract (ToSchema)

newtype Pool = Pool
  { poolCurrency :: CurrencySymbol
  }
  deriving (Show)
PlutusTx.unstableMakeIsData ''Pool

data Action
  = Create Pool
  | Close
  deriving (Show)

PlutusTx.unstableMakeIsData ''Action

data LendingDatum = LendingDatum
  { ldCurrency :: CurrencySymbol
  }

PlutusTx.unstableMakeIsData ''LendingDatum

data CreateParams = CreateParams
  { cpCurrency :: CurrencySymbol
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

create :: HasBlockchainActions s => CreateParams -> Contract w s Text ()
create _ = do
  return ()

data Lending
instance Scripts.ScriptType Lending where
  type RedeemerType Lending = Action
  type DatumType    Lending = LendingDatum

type LendingSchema =
     BlockchainActions
         .\/ Endpoint "create" ()

-- endpoints :: Contract w LendingSchema Void ()
-- endpoints = forever endpoints


