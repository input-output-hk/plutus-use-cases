module Mlabs.NFT.Governance.Validation(
  epochMintPolicy,
  govMintPolicy,
  epochScript,
  )
where

import Prelude qualified as Hask
import Mlabs.Data.LinkedList 
import PlutusTx.Prelude 
import Mlabs.NFT.Types
import Ledger
import PlutusTx qualified
import Ledger.Typed.Scripts
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)


--------------------------------------------------------------------------------
-- Epoch Logic
{-

The Epoch parametrizes the application and points to all the relevant
information regarding the application. It is the Single Source of Truth for the
entire application. The logic from one epoch to another can be changed through
the expressing of votes of xGov Token holders. 

-}

-- | The single token that provides proof of uniqueness to the entire
-- application. The Token sits in the EUTXo containing the HEAD of the Epoch
-- list.
newtype UniqueAppToken = UniqueAppToken {uniqueapp'Token :: AssetClass}
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (ToJSON, FromJSON)
PlutusTx.unstableMakeIsData ''UniqueAppToken
PlutusTx.makeLift ''UniqueAppToken

-- | The Epoch Symbol is determined by the UniqueAppToken 
newtype EpochSymbol = EpochSymbol {et'assetClass :: AssetClass}
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (ToJSON, FromJSON)

PlutusTx.unstableMakeIsData ''EpochSymbol
PlutusTx.makeLift ''EpochSymbol

-- | Epoch Minting Policy Redeemer
data EpochAct
  = -- | Initialise a new epoch with a new EpochSymbol.
    EpochInit
  | -- | Forward the Epoch. Must make sure all of the polls are closed. 
    EpochNext

PlutusTx.makeLift ''EpochAct
PlutusTx.unstableMakeIsData ''EpochAct
newtype EpochDatum = EpochDatum {epoch'list :: LList Integer () ()}
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (ToJSON, FromJSON)

PlutusTx.unstableMakeIsData ''EpochDatum
PlutusTx.makeLift ''EpochDatum

-- | Minting policy for Epoch tokens.
mkEpochMintPolicy :: UniqueAppToken -> EpochAct -> ScriptContext -> Bool
mkEpochMintPolicy !_ !_ !_ = True 

epochMintPolicy :: NftAppInstance -> MintingPolicy
epochMintPolicy appInstance =
  mkMintingPolicyScript $
    $$(PlutusTx.compile [||wrapMintingPolicy . mkEpochMintPolicy||])
      `PlutusTx.applyCode` PlutusTx.liftCode appInstance

mkEpochScript :: NftAppInstance -> EpochDatum -> EpochAct -> ScriptContext -> Bool
mkEpochScript !_ !_ !_ !_ = True 

data EpochManage
instance ValidatorTypes EpochManage where
  type DatumType EpochManage    = EpochDatum
  type RedeemerType EpochManage = EpochAct

{-# INLINEABLE epochScript #-}
epochScript :: NftAppInstance -> TypedValidator EpochManage
epochScript appInstance =
  mkTypedValidator @EpochManage
    ($$(PlutusTx.compile [|| mkEpochScript ||])
     `PlutusTx.applyCode` PlutusTx.liftCode appInstance)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = wrapValidator @EpochDatum @EpochAct

--------------------------------------------------------------------------------
-- GOV List

newtype GovDatum = GovDatum {gov'list :: LList UserId () ()}
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (ToJSON, FromJSON)

PlutusTx.unstableMakeIsData ''GovDatum
PlutusTx.makeLift ''GovDatum

data GovManage
instance ValidatorTypes GovManage where
  type DatumType GovManage    = GovDatum
  type RedeemerType GovManage = GovAct

-- | Minting policy for GOV and xGOV tokens.
mkGovMintPolicy :: NftAppInstance -> EpochSymbol -> GovAct -> ScriptContext -> Bool
mkGovMintPolicy !_ !_ !_ !_ = True 

govMintPolicy :: NftAppInstance -> EpochSymbol -> MintingPolicy
govMintPolicy app epoch =
  mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \app' epoch' -> wrapMintingPolicy $ mkGovMintPolicy app' epoch'||])
    `PlutusTx.applyCode`
       PlutusTx.liftCode app
    `PlutusTx.applyCode`
      PlutusTx.liftCode epoch

{-# INLINEABLE govScript #-}
govScript :: NftAppInstance -> TypedValidator EpochManage
govScript appInstance =
  mkTypedValidator @EpochManage
    ($$(PlutusTx.compile [|| mkEpochScript ||])
     `PlutusTx.applyCode` PlutusTx.liftCode appInstance)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = wrapValidator @EpochDatum @EpochAct

--------------------------------------------------------------------------------
-- Reward Treasury
