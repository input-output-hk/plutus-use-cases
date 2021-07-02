{-# LANGUAGE NamedFieldPuns #-}
-- | Missing functions for StateMachine
module Mlabs.Plutus.Contract.StateMachine(
    runInitialiseWith
  , runStepWith
) where

import Prelude

import Data.Void (absurd)
import           Control.Lens
import           Control.Monad.Error.Lens
import           Ledger.Constraints                   (ScriptLookups, mustPayToTheScript, UnbalancedTx)
import qualified Ledger.Constraints.OffChain          as Constraints
import qualified Ledger.Typed.Scripts                 as Scripts
import           Plutus.Contract
import qualified Plutus.Contract.StateMachine.OnChain as SM
import qualified PlutusTx                             as PlutusTx
import Ledger.Value
import Plutus.V1.Ledger.Contexts (pubKeyHash)

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

-- | Run one step of a state machine, returning the new state.
runStepWith ::
    forall w e state schema input.
    ( AsSMContractError e
    , PlutusTx.IsData state
    , PlutusTx.IsData input
    , HasUtxoAt schema
    , HasWriteTx schema
    , HasOwnPubKey schema
    , HasTxConfirmation schema
    )
    => StateMachineClient state input
    -- ^ The state machine
    -> input
    -- ^ The input to apply to the state machine
    -> ScriptLookups (StateMachine state input)
    -> TxConstraints (Scripts.RedeemerType (StateMachine state input)) (Scripts.DatumType (StateMachine state input))
    -> Contract w schema e (TransitionResult state input)
runStepWith smc input lookups constraints =
    runGuardedStepWith smc input lookups constraints (\_ _ _ -> Nothing) >>= pure . \case
        Left a  -> absurd a
        Right a -> a

-- | Tries to run one step of a state machine: If the /guard/ (the last argument) returns @'Nothing'@ when given the
-- unbalanced transaction to be submitted, the old state and the new step, the step is run and @'Right'@ the new state is returned.
-- If the guard returns @'Just' a@, @'Left' a@ is returned instead.
runGuardedStepWith ::
    forall w a e state schema input.
    ( AsSMContractError e
    , PlutusTx.IsData state
    , PlutusTx.IsData input
    , HasUtxoAt schema
    , HasWriteTx schema
    , HasOwnPubKey schema
    , HasTxConfirmation schema
    )
    => StateMachineClient state input              -- ^ The state machine
    -> input                                       -- ^ The input to apply to the state machine
    -> ScriptLookups (StateMachine state input)
    -> TxConstraints (Scripts.RedeemerType (StateMachine state input)) (Scripts.DatumType (StateMachine state input))
    -> (UnbalancedTx -> state -> state -> Maybe a) -- ^ The guard to check before running the step
    -> Contract w schema e (Either a (TransitionResult state input))
runGuardedStepWith smc input userLookups userConstraints guard = mapError (review _SMContractError) $ mkStep smc input >>= \case
    Right (StateMachineTransition{smtConstraints,smtOldState=State{stateData=os}, smtNewState=State{stateData=ns}, smtLookups}) -> do
        pk <- ownPubKey
        let lookups = smtLookups { Constraints.slOwnPubkey = Just $ pubKeyHash pk }
        utx <- either (throwing _ConstraintResolutionError) pure (Constraints.mkTx (lookups <> userLookups) (smtConstraints <> userConstraints))
        case guard utx os ns of
            Nothing -> do
                submitTxConfirmed utx
                pure $ Right $ TransitionSuccess ns
            Just a  -> pure $ Left a
    Left e -> pure $ Right $ TransitionFailure e

