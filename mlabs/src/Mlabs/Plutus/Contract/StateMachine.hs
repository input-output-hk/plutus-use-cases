{-# LANGUAGE NamedFieldPuns #-}
-- | Missing functions for StateMachine
module Mlabs.Plutus.Contract.StateMachine(
    runInitialiseWith
  , runStepWith
) where

import Prelude

import           Data.Void                            (absurd)
import           Control.Lens                         (review)
import           Control.Monad.Error.Lens             (throwing)
import           Ledger.Constraints                   (ScriptLookups, mustPayToTheScript, UnbalancedTx, TxConstraints)
import qualified Ledger.Constraints.OffChain          as Constraints
import qualified Ledger.Typed.Scripts                 as Scripts
import qualified Plutus.Contract                      as Contract
import qualified PlutusTx
import           Ledger.Value                         (Value)
import           Plutus.V1.Ledger.Contexts            (pubKeyHash)

import qualified Plutus.Contract.StateMachine         as SM
import qualified Plutus.Contract.StateMachine.OnChain as SM

-- | Initialise a state machine
runInitialiseWith ::
    forall w e state schema input.
    ( PlutusTx.IsData state
    , PlutusTx.IsData input
    , Contract.HasTxConfirmation schema
    , Contract.HasWriteTx schema
    , SM.AsSMContractError e
    )
    => SM.StateMachineClient state input
    -- ^ The state machine
    -> state
    -- ^ The initial state
    -> Value
    -- ^ The value locked by the contract at the beginning
    -> ScriptLookups (SM.StateMachine state input)
    -> SM.TxConstraints (Scripts.RedeemerType (SM.StateMachine state input)) (Scripts.DatumType (SM.StateMachine state input))
    -> Contract.Contract w schema e state
runInitialiseWith SM.StateMachineClient{scInstance} initialState initialValue customLookups customConstraints = Contract.mapError (review SM._SMContractError) $ do
    let SM.StateMachineInstance{validatorInstance, stateMachine} = scInstance
        tx = mustPayToTheScript initialState (initialValue <> SM.threadTokenValue stateMachine) <> customConstraints
    let lookups = Constraints.scriptInstanceLookups validatorInstance <> customLookups
    utx <- either (throwing Contract._ConstraintResolutionError) pure (Constraints.mkTx lookups tx)
    Contract.submitTxConfirmed utx
    pure initialState

-- | Run one step of a state machine, returning the new state.
runStepWith ::
    forall w e state schema input.
    ( SM.AsSMContractError e
    , PlutusTx.IsData state
    , PlutusTx.IsData input
    , Contract.HasUtxoAt schema
    , Contract.HasWriteTx schema
    , Contract.HasOwnPubKey schema
    , Contract.HasTxConfirmation schema
    )
    => SM.StateMachineClient state input
    -- ^ The state machine
    -> input
    -- ^ The input to apply to the state machine
    -> ScriptLookups (SM.StateMachine state input)
    -> SM.TxConstraints (Scripts.RedeemerType (SM.StateMachine state input)) (Scripts.DatumType (SM.StateMachine state input))
    -> Contract.Contract w schema e (SM.TransitionResult state input)
runStepWith smc input lookups constraints =
    runGuardedStepWith smc input lookups constraints (\_ _ _ -> Nothing) >>= pure . \case
        Left a  -> absurd a
        Right a -> a

-- | Tries to run one step of a state machine: If the /guard/ (the last argument) returns @'Nothing'@ when given the
-- unbalanced transaction to be submitted, the old state and the new step, the step is run and @'Right'@ the new state is returned.
-- If the guard returns @'Just' a@, @'Left' a@ is returned instead.
runGuardedStepWith ::
    forall w a e state schema input.
    ( SM.AsSMContractError e
    , PlutusTx.IsData state
    , PlutusTx.IsData input
    , Contract.HasUtxoAt schema
    , Contract.HasWriteTx schema
    , Contract.HasOwnPubKey schema
    , Contract.HasTxConfirmation schema
    )
    => SM.StateMachineClient state input              -- ^ The state machine
    -> input                                       -- ^ The input to apply to the state machine
    -> ScriptLookups (SM.StateMachine state input)
    -> TxConstraints (Scripts.RedeemerType (SM.StateMachine state input)) (Scripts.DatumType (SM.StateMachine state input))
    -> (UnbalancedTx -> state -> state -> Maybe a) -- ^ The guard to check before running the step
    -> Contract.Contract w schema e (Either a (SM.TransitionResult state input))
runGuardedStepWith smc input userLookups userConstraints guard = Contract.mapError (review SM._SMContractError) $ SM.mkStep smc input >>= \case
    Right (SM.StateMachineTransition{smtConstraints,smtOldState=SM.State{stateData=os}, smtNewState=SM.State{stateData=ns}, smtLookups}) -> do
        pk <- Contract.ownPubKey
        let lookups = smtLookups { Constraints.slOwnPubkey = Just $ pubKeyHash pk }
        utx <- either (throwing Contract._ConstraintResolutionError) pure (Constraints.mkTx (lookups <> userLookups) (smtConstraints <> userConstraints))
        case guard utx os ns of
            Nothing -> do
                Contract.submitTxConfirmed utx
                pure $ Right $ SM.TransitionSuccess ns
            Just a  -> pure $ Left a
    Left e -> pure $ Right $ SM.TransitionFailure e

