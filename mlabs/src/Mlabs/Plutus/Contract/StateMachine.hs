{-# LANGUAGE NamedFieldPuns #-}
-- | Missing functions for StateMachine
module Mlabs.Plutus.Contract.StateMachine(
  runInitialiseWith
) where

import Prelude

import           Control.Lens
import           Control.Monad.Error.Lens
import           Ledger.Constraints                   (ScriptLookups, mustPayToTheScript)
import qualified Ledger.Constraints.OffChain          as Constraints
import qualified Ledger.Typed.Scripts                 as Scripts
import           Plutus.Contract
import qualified Plutus.Contract.StateMachine.OnChain as SM
import qualified PlutusTx                             as PlutusTx
import Ledger.Value

import Plutus.Contract.StateMachine

-- | Initialise a state machine
runInitialiseWith ::
    forall w e state schema input.
    ( PlutusTx.IsData state
    , PlutusTx.IsData input
    , HasTxConfirmation schema
    , HasWriteTx schema
    , AsSMContractError e
    )
    => StateMachineClient state input
    -- ^ The state machine
    -> state
    -- ^ The initial state
    -> Value
    -- ^ The value locked by the contract at the beginning
    -> ScriptLookups (StateMachine state input)
    -> TxConstraints (Scripts.RedeemerType (StateMachine state input)) (Scripts.DatumType (StateMachine state input))
    -> Contract w schema e state
runInitialiseWith StateMachineClient{scInstance} initialState initialValue customLookups customConstraints = mapError (review _SMContractError) $ do
    let StateMachineInstance{validatorInstance, stateMachine} = scInstance
        tx = mustPayToTheScript initialState (initialValue <> SM.threadTokenValue stateMachine) <> customConstraints
    let lookups = Constraints.scriptInstanceLookups validatorInstance <> customLookups
    utx <- either (throwing _ConstraintResolutionError) pure (Constraints.mkTx lookups tx)
    submitTxConfirmed utx
    pure initialState

