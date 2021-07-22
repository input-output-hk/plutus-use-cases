-- | State machine and binding of transitions to Plutus for lending app
module Mlabs.Lending.Contract.StateMachine(
    Lendex
  , LendexError
  , toLendexError
  , lendexAddress
  , runStep
  , runStepWith
  , runInitialise
) where

import qualified Prelude                         as Hask ( String )
import           PlutusTx.Prelude                hiding (Applicative (..), check, Semigroup(..), Monoid(..))
import qualified PlutusTx.Prelude                as Plutus
import qualified Ledger.TimeSlot                 as TimeSlot (posixTimeRangeToSlotRange)

import           Control.Monad.State.Strict      (runStateT)
import           Data.Default                    (Default (def))
import           Data.Functor                    (void)
import           Data.String                     (IsString(fromString))
import           Ledger.Constraints              (ScriptLookups, TxConstraints, mustBeSignedBy)
import qualified Ledger
import qualified Ledger.Typed.Scripts.Validators as Validators
import qualified Plutus.Contract                 as Contract
import qualified Plutus.Contract.StateMachine    as SM
import qualified PlutusTx

import           Mlabs.Emulator.Blockchain       (toConstraints, updateRespValue)
import           Mlabs.Lending.Logic.React       (react)
import qualified Mlabs.Lending.Logic.Types       as Types

type Lendex = SM.StateMachine (Types.LendexId, Types.LendingPool) Types.Act

-- | Error type
type LendexError = SM.SMContractError

toLendexError :: Hask.String -> LendexError
toLendexError = SM.SMCContractError . fromString

{-# INLINABLE machine #-}
machine :: Types.LendexId -> Lendex
machine lid = (SM.mkStateMachine Nothing (transition lid) isFinal)
  { SM.smCheck = checkTimestamp }
  where
    isFinal = const False

    checkTimestamp _ input ctx = maybe True check $ getInputTime input
      where
        check t = Ledger.Slot t `Ledger.member` TimeSlot.posixTimeRangeToSlotRange def range
        range = Ledger.txInfoValidRange $ Ledger.scriptContextTxInfo ctx

    getInputTime = \case
      Types.UserAct time _ _  -> Just time
      Types.PriceAct time _ _ -> Just time
      _                 -> Nothing

{-# INLINABLE mkValidator #-}
mkValidator :: Types.LendexId -> Validators.ValidatorType Lendex
mkValidator lid = SM.mkValidator (machine lid)

client :: Types.LendexId -> SM.StateMachineClient (Types.LendexId, Types.LendingPool) Types.Act
client lid = SM.mkStateMachineClient $ SM.StateMachineInstance (machine lid) (scriptInstance lid)

lendexValidatorHash :: Types.LendexId -> Ledger.ValidatorHash
lendexValidatorHash lid = Validators.validatorHash (scriptInstance lid)

lendexAddress :: Types.LendexId -> Ledger.Address
lendexAddress lid = Ledger.scriptHashAddress (lendexValidatorHash lid)

scriptInstance :: Types.LendexId -> Validators.TypedValidator Lendex
scriptInstance lid = Validators.mkTypedValidator @Lendex
  ($$(PlutusTx.compile [|| mkValidator ||])
      `PlutusTx.applyCode` (PlutusTx.liftCode lid)
  )
  $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Validators.wrapValidator

{-# INLINABLE transition #-}
transition ::
     Types.LendexId
  -> SM.State (Types.LendexId, Types.LendingPool)
  -> Types.Act
  -> Maybe (SM.TxConstraints SM.Void SM.Void, SM.State (Types.LendexId, Types.LendingPool))
transition lid SM.State{stateData=oldData, stateValue=oldValue} input
  | lid == inputLid = case runStateT (react input) (snd oldData) of
      Left _err              -> Nothing
      Right (resps, newData) -> Just ( foldMap toConstraints resps Plutus.<> ctxConstraints
                                    , SM.State { stateData  = (lid, newData)
                                                , stateValue = updateRespValue resps oldValue })
  | otherwise = Nothing
  where
    inputLid = fst oldData

    -- we check that user indeed signed the transaction with his own key
    ctxConstraints = maybe Plutus.mempty mustBeSignedBy userId

    userId = case input of
      Types.UserAct _ (Types.UserId uid) _ -> Just uid
      _                        -> Nothing

----------------------------------------------------------------------
-- specific versions of SM-functions

runStep :: forall w e schema .
  SM.AsSMContractError e
  => Types.LendexId -> Types.Act -> Contract.Contract w schema e ()
runStep lid act = void $ SM.runStep (client lid) act

runStepWith :: forall w e schema .
  SM.AsSMContractError e
  => Types.LendexId
  -> Types.Act
  -> ScriptLookups Lendex
  -> TxConstraints (Validators.RedeemerType Lendex) (Validators.DatumType Lendex)
  -> Contract.Contract w schema e ()
runStepWith lid act lookups constraints = void $ SM.runStepWith lookups constraints (client lid) act 

runInitialise :: forall w e schema .
  SM.AsSMContractError e
  => Types.LendexId -> Types.LendingPool -> Ledger.Value -> Contract.Contract w schema e ()
runInitialise lid lendingPool val = void $ SM.runInitialise (client lid) (lid, lendingPool) val


