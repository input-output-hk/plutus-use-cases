module Mlabs.Lending.Lendex(

) where

import qualified Prelude
import Control.Monad.State.Strict (runStateT)

import qualified Plutus.Contract.StateMachine as SM
import qualified Ledger.Typed.Scripts         as Scripts
import qualified PlutusTx                     as PlutusTx
import           PlutusTx.Prelude             hiding (Applicative (..), check)

import Mlabs.Lending.Logic.React
import Mlabs.Lending.Logic.Types

type Lendex = SM.StateMachine LendingPool Act

{-# INLINABLE machine #-}
machine :: Lendex
machine = SM.mkStateMachine Nothing transition isFinal
  where
    isFinal = const False

{-# INLINABLE mkValidator #-}
mkValidator :: Scripts.ValidatorType Lendex
mkValidator = SM.mkValidator machine

scriptInstance :: Scripts.ScriptInstance Lendex
scriptInstance = Scripts.validator @Lendex
  $$(PlutusTx.compile [|| mkValidator ||])
  $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator

transition ::
     SM.State LendingPool
  -> Act
  -> Maybe (SM.TxConstraints SM.Void SM.Void, SM.State LendingPool)
transition SM.State{stateData=oldData, stateValue=oldValue} input = case runStateT (react input) oldData of
  Left err           -> Nothing
  Right (_, newData) -> Just (mempty, SM.State { stateData=newData, stateValue=oldValue })



