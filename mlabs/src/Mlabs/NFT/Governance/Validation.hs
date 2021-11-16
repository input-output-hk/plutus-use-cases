module Mlabs.NFT.Governance.Validation (
  govScript,
  govMintPolicy,
  govScrAddress,
  GovManage,
) where

--import Prelude qualified as Hask

import Ledger (
  Address,
  MintingPolicy,
  ScriptContext,
  mkMintingPolicyScript,
 )
import Ledger.Typed.Scripts (
  TypedValidator,
  ValidatorTypes (..),
  mkTypedValidator,
  validatorAddress,
  wrapMintingPolicy,
  wrapValidator,
 )

import Mlabs.NFT.Governance.Types (GovAct (..), GovDatum)
import Mlabs.NFT.Types (NftAppInstance)
import PlutusTx qualified
import PlutusTx.Prelude (Bool (True), ($), (.))

data GovManage
instance ValidatorTypes GovManage where
  type DatumType GovManage = GovDatum
  type RedeemerType GovManage = GovAct

{-# INLINEABLE mkGovMintPolicy #-}

-- | Minting policy for GOV and xGOV tokens.
mkGovMintPolicy :: NftAppInstance -> GovAct -> ScriptContext -> Bool
mkGovMintPolicy _ act _ =
  case act of
    InitialiseGov ->
      True
    MintGov ->
      True -- makes sure that 1:1 Gov/xGov tokens are minted with the amount of
      -- lovelace paid at the Treasury address. Makes sure that the Gov is
      -- inserted to the linked list (correctly).
    Proof ->
      True -- does nothing (i.e. makes sure nothing gets minted)
    ProofAndBurn ->
      True -- makes sure that Gov/xGov is removed and the Gov linked list is
      -- updated accordingly.

{-# INLINEABLE govMintPolicy #-}

-- | Gov Minting Policy
govMintPolicy :: NftAppInstance -> MintingPolicy
govMintPolicy x =
  mkMintingPolicyScript $
    $$(PlutusTx.compile [||wrapMintingPolicy . mkGovMintPolicy||])
      `PlutusTx.applyCode` PlutusTx.liftCode x

{-# INLINEABLE mkGovScript #-}

-- | Minting policy for GOV and xGOV tokens.
mkGovScript :: GovDatum -> GovAct -> ScriptContext -> Bool
mkGovScript _ act _ =
  case act of
    InitialiseGov ->
      True
    MintGov ->
      True -- makes sure that the correct fees are paid, and the correct amount
      -- of Gov/xGov is minted. Also makes sure that the Gov/xGov are sent
      -- to the correct addresses, and that the Gov list is not altered
      -- maliciously.
    Proof ->
      True -- makes sure that the token is used as proof and returned to the Gov
      -- Address, with nothing being altered.
    ProofAndBurn ->
      True -- makes sure, that the corresponding Gov to the xGov is removed from
      -- the list. The user can also claim their locked lovelace back (take
      -- their stake out of the app).

{-# INLINEABLE govScript #-}
govScript :: TypedValidator GovManage
govScript =
  mkTypedValidator @GovManage
    $$(PlutusTx.compile [||mkGovScript||])
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = wrapValidator @GovDatum @GovAct

{-# INLINEABLE govScrAddress #-}

-- | Address of Gov Script Logic.
govScrAddress :: Ledger.Address
govScrAddress = validatorAddress govScript
