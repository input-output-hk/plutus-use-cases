module Mlabs.NFT.Governance.Validation (
  epochMintPolicy,
  --  govMintPolicy,
  --  epochScript,
) where

--
--import Prelude qualified as Hask
--import Mlabs.Data.LinkedList
import PlutusTx.Prelude

--import Mlabs.NFT.Types
--import Ledger
--import PlutusTx qualified
--import Ledger.Typed.Scripts
--import GHC.Generics (Generic)
--import Data.Aeson (FromJSON, ToJSON)
--
epochMintPolicy :: forall a. a
epochMintPolicy = error ()

----------------------------------------------------------------------------------
---- GOV List
--
--newtype GovDatum = GovDatum {gov'list :: LList UserId () ()}
--  deriving stock (Hask.Show, Generic, Hask.Eq)
--  deriving anyclass (ToJSON, FromJSON)
--
--PlutusTx.unstableMakeIsData ''GovDatum
--PlutusTx.makeLift ''GovDatum
--
--data GovManage
--instance ValidatorTypes GovManage where
--  type DatumType GovManage    = GovDatum
--  type RedeemerType GovManage = GovAct
--
---- | Minting policy for GOV and xGOV tokens.
--mkGovMintPolicy :: EpochSymbol -> GovAct -> ScriptContext -> Bool
--mkGovMintPolicy !_ !_ !_ !_ = True
--
--govMintPolicy :: NftAppInstance -> EpochSymbol -> MintingPolicy
--govMintPolicy app epoch =
--  mkMintingPolicyScript $
--    $$(PlutusTx.compile [|| \app' epoch' -> wrapMintingPolicy $ mkGovMintPolicy app' epoch'||])
--    `PlutusTx.applyCode`
--       PlutusTx.liftCode app
--    `PlutusTx.applyCode`
--      PlutusTx.liftCode epoch
--
--{-# INLINEABLE govScript #-}
--govScript :: NftAppInstance -> TypedValidator EpochManage
--govScript appInstance =
--  mkTypedValidator @EpochManage
--    ($$(PlutusTx.compile [|| mkEpochScript ||])
--     `PlutusTx.applyCode` PlutusTx.liftCode appInstance)
--    $$(PlutusTx.compile [|| wrap ||])
--  where
--    wrap = wrapValidator @EpochDatum @EpochAct
--
----------------------------------------------------------------------------------
---- Reward Treasury
--
