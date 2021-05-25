-- | Validation of forge for NFTs
module Mlabs.Nft.Contract.Forge(
    currencyPolicy
  , currencySymbol
) where

import Control.Monad.State.Strict (evalStateT)

import PlutusTx.Prelude
import Ledger (CurrencySymbol)

import Ledger.Typed.Scripts (MonetaryPolicy)
import qualified Plutus.V1.Ledger.Value as Value
import qualified Plutus.V1.Ledger.Scripts as Scripts
import qualified Ledger.Typed.Scripts     as Scripts
import qualified PlutusTx                 as PlutusTx
import Plutus.V1.Ledger.Contexts
import Ledger.Constraints

import Mlabs.Nft.Logic.Types
import Mlabs.Nft.Logic.State

validate :: NftId -> ScriptContext -> Bool
validate _ _ = True

-------------------------------------------------------------------------------

currencyPolicy :: NftId -> MonetaryPolicy
currencyPolicy nid = Scripts.mkMonetaryPolicyScript $
  $$(PlutusTx.compile [|| \x -> Scripts.wrapMonetaryPolicy (validate x) ||])
  `PlutusTx.applyCode` PlutusTx.liftCode nid

currencySymbol :: NftId -> CurrencySymbol
currencySymbol nid = scriptCurrencySymbol (currencyPolicy nid)



