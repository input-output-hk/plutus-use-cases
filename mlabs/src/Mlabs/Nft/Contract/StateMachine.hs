{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}

module Mlabs.Nft.Contract.StateMachine(
    NftMachine
  , NftMachineClient
  , NftError
  , toNftError
  , nftAddress
  , nftPolicy
  , nftValue
  , runStepWith
  , runInitialiseWith
) where

import Control.Monad.State.Strict (runStateT)
import Data.Functor (void)
import Data.String

import           Plutus.Contract
import qualified Plutus.Contract.StateMachine as SM
import           Ledger                       hiding (singleton)
import qualified Ledger.Typed.Scripts         as Scripts
import           Ledger.Constraints
import qualified PlutusTx                     as PlutusTx
import           PlutusTx.Prelude             hiding (Applicative (..), check, Semigroup(..), Monoid(..))
import qualified PlutusTx.Prelude             as Plutus
import Plutus.V1.Ledger.Value

import Mlabs.Emulator.Blockchain
import Mlabs.Emulator.Types
import Mlabs.Nft.Logic.React
import Mlabs.Nft.Logic.Types
import qualified Mlabs.Nft.Contract.Forge as Forge
import qualified Mlabs.Plutus.Contract.StateMachine as SM


type NftMachine = SM.StateMachine Nft Act
type NftMachineClient = SM.StateMachineClient Nft Act

-- | NFT errors
type NftError = SM.SMContractError

toNftError :: String -> NftError
toNftError = SM.SMCContractError . fromString

{-# INLINABLE machine #-}
-- | State machine definition
machine :: NftId -> NftMachine
machine nftId = (SM.mkStateMachine Nothing (transition nftId) isFinal)
  where
    isFinal = const False

{-# INLINABLE mkValidator #-}
-- | State machine validator
mkValidator :: NftId -> Scripts.ValidatorType NftMachine
mkValidator nftId = SM.mkValidator (machine nftId)

-- | State machine client
client :: NftId -> NftMachineClient
client nftId = SM.mkStateMachineClient $ SM.StateMachineInstance (machine nftId) (scriptInstance nftId)

-- | NFT validator hash
nftValidatorHash :: NftId -> ValidatorHash
nftValidatorHash nftId = Scripts.scriptHash (scriptInstance nftId)

-- | NFT script address
nftAddress :: NftId -> Address
nftAddress nftId = scriptHashAddress (nftValidatorHash nftId)

-- | NFT script instance
scriptInstance :: NftId -> Scripts.ScriptInstance NftMachine
scriptInstance nftId = Scripts.validator @NftMachine
  ($$(PlutusTx.compile [|| mkValidator ||])
    `PlutusTx.applyCode` (PlutusTx.liftCode nftId)
  )
  $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator

{-# INLINABLE transition #-}
-- | State transitions for NFT
transition ::
     NftId
  -> SM.State Nft
  -> Act
  -> Maybe (SM.TxConstraints SM.Void SM.Void, SM.State Nft)
transition nftId SM.State{stateData=oldData, stateValue=oldValue} input
  | idIsValid =
      case runStateT (react input) oldData of
        Left _err              -> Nothing
        Right (resps, newData) -> Just ( foldMap toConstraints resps Plutus.<> ctxConstraints
                                      , SM.State { stateData  = newData
                                                  , stateValue = updateRespValue resps oldValue })
  | otherwise = Nothing
  where
    idIsValid = nftId == nft'id oldData

    -- we check that user indeed signed the transaction with his own key
    ctxConstraints = maybe Plutus.mempty mustBeSignedBy userId

    userId = case input of
      UserAct (UserId uid) _ -> Just uid
      _                      -> Nothing

-----------------------------------------------------------------------
-- NFT forge policy

-- | NFT monetary policy
nftPolicy :: NftId -> MonetaryPolicy
nftPolicy nid = Forge.currencyPolicy (nftAddress nid)  nid

-- | NFT currency symbol
nftSymbol :: NftId -> CurrencySymbol
nftSymbol nid = Forge.currencySymbol (nftAddress nid) nid

-- | NFT coin (AssetClass)
nftCoin :: NftId -> AssetClass
nftCoin nid = AssetClass (nftSymbol nid, nid.nftId'token)

-- | Single value of NFT coin. We check that there is only one NFT-coin can be minted.
nftValue :: NftId -> Value
nftValue nid = assetClassValue (nftCoin nid) 1

------------------------------------------------------------------------

runStepWith :: forall w e schema .
  ( SM.AsSMContractError e
  , HasUtxoAt schema
  , HasWriteTx schema
  , HasOwnPubKey schema
  , HasTxConfirmation schema
  )
  => NftId
  -> Act
  -> ScriptLookups NftMachine
  -> TxConstraints (Scripts.RedeemerType NftMachine) (Scripts.DatumType NftMachine)
  -> Contract w schema e ()
runStepWith nid act lookups constraints = void $ SM.runStepWith (client nid) act lookups constraints

runInitialiseWith ::
  ( SM.AsSMContractError e
  , HasUtxoAt schema
  , HasWriteTx schema
  , HasOwnPubKey schema
  , HasTxConfirmation schema
  )
  => NftId
  -> Nft
  -> Value
  -> ScriptLookups NftMachine
  -> TxConstraints (Scripts.RedeemerType NftMachine) (Scripts.DatumType NftMachine)
  -> Contract w schema e ()
runInitialiseWith nftId nft val lookups tx = void $ SM.runInitialiseWith (client nftId) nft val lookups tx
