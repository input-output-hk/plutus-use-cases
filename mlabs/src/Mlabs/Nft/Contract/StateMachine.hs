{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Mlabs.Nft.Contract.StateMachine (
  NftMachine,
  NftMachineClient,
  NftError,
  toNftError,
  nftAddress,
  nftPolicy,
  nftValue,
  runStepWith,
  runInitialiseWith,
  scriptInstance,
) where

import PlutusTx.Prelude hiding (Applicative (..), Monoid (..), Semigroup (..), check)
import Prelude qualified as Hask (String)

import Control.Monad.State.Strict (runStateT)
import Data.Functor (void)
import Data.String (fromString)
import Ledger (Address, MintingPolicy, ValidatorHash, scriptHashAddress)
import Ledger.Constraints (ScriptLookups, TxConstraints, mustBeSignedBy)
import Ledger.Typed.Scripts qualified as Scripts
import Plutus.Contract (Contract)
import Plutus.Contract.StateMachine qualified as SM
import Plutus.V1.Ledger.Value (AssetClass (..), CurrencySymbol, Value, assetClassValue)
import PlutusTx qualified
import PlutusTx.Prelude qualified as Plutus

import Mlabs.Emulator.Blockchain (toConstraints, updateRespValue)
import Mlabs.Emulator.Types (UserId (..))
import Mlabs.Nft.Contract.Forge qualified as Forge
import Mlabs.Nft.Logic.React (react)
import Mlabs.Nft.Logic.Types (Act (UserAct), Nft (nft'id), NftId (nftId'token))

type NftMachine = SM.StateMachine Nft Act
type NftMachineClient = SM.StateMachineClient Nft Act

-- | NFT errors
type NftError = SM.SMContractError

toNftError :: Hask.String -> NftError
toNftError = SM.SMCContractError . fromString

{-# INLINEABLE machine #-}

-- | State machine definition
machine :: NftId -> NftMachine
machine nftId = SM.mkStateMachine Nothing (transition nftId) isFinal
  where
    isFinal = const False

{-# INLINEABLE mkValidator #-}

-- | State machine validator
mkValidator :: NftId -> Scripts.ValidatorType NftMachine
mkValidator nftId = SM.mkValidator (machine nftId)

-- | State machine client
client :: NftId -> NftMachineClient
client nftId = SM.mkStateMachineClient $ SM.StateMachineInstance (machine nftId) (scriptInstance nftId)

-- | NFT validator hash
nftValidatorHash :: NftId -> ValidatorHash
nftValidatorHash nftId = Scripts.validatorHash (scriptInstance nftId)

-- | NFT script address
nftAddress :: NftId -> Address
nftAddress nftId = scriptHashAddress (nftValidatorHash nftId)

-- | NFT script instance
scriptInstance :: NftId -> Scripts.TypedValidator NftMachine
scriptInstance nftId =
  Scripts.mkTypedValidator @NftMachine
    ( $$(PlutusTx.compile [||mkValidator||])
        `PlutusTx.applyCode` PlutusTx.liftCode nftId
    )
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.wrapValidator

{-# INLINEABLE transition #-}

-- | State transitions for NFT
transition ::
  NftId ->
  SM.State Nft ->
  Act ->
  Maybe (SM.TxConstraints SM.Void SM.Void, SM.State Nft)
transition nftId SM.State {stateData = oldData, stateValue = oldValue} input
  | idIsValid =
    case runStateT (react input) oldData of
      Left _err -> Nothing
      Right (resps, newData) ->
        Just
          ( foldMap toConstraints resps Plutus.<> ctxConstraints
          , SM.State
              { stateData = newData
              , stateValue = updateRespValue resps oldValue
              }
          )
  | otherwise = Nothing
  where
    idIsValid = nftId == nft'id oldData

    -- we check that user indeed signed the transaction with his own key
    ctxConstraints = maybe Plutus.mempty mustBeSignedBy userId

    userId = case input of
      UserAct (UserId uid) _ -> Just uid
      _ -> Nothing

-----------------------------------------------------------------------
-- NFT forge policy

-- | NFT monetary policy
nftPolicy :: NftId -> MintingPolicy
nftPolicy nid = Forge.currencyPolicy (nftAddress nid) nid

-- | NFT currency symbol
nftSymbol :: NftId -> CurrencySymbol
nftSymbol nid = Forge.currencySymbol (nftAddress nid) nid

-- | NFT coin (AssetClass)
nftCoin :: NftId -> AssetClass
nftCoin nid = AssetClass (nftSymbol nid, nftId'token nid)

-- | Single value of NFT coin. We check that there is only one NFT-coin can be minted.
nftValue :: NftId -> Value
nftValue nid = assetClassValue (nftCoin nid) 1

------------------------------------------------------------------------

runStepWith ::
  forall w e schema.
  SM.AsSMContractError e =>
  NftId ->
  Act ->
  ScriptLookups NftMachine ->
  TxConstraints (Scripts.RedeemerType NftMachine) (Scripts.DatumType NftMachine) ->
  Contract w schema e ()
runStepWith nid act lookups constraints = void $ SM.runStepWith lookups constraints (client nid) act

runInitialiseWith ::
  SM.AsSMContractError e =>
  NftId ->
  Nft ->
  Value ->
  ScriptLookups NftMachine ->
  TxConstraints (Scripts.RedeemerType NftMachine) (Scripts.DatumType NftMachine) ->
  Contract w schema e ()
runInitialiseWith nftId nft val lookups tx = void $ SM.runInitialiseWith lookups tx (client nftId) nft val
