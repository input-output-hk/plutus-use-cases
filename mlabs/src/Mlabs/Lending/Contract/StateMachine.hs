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

import Data.Monoid (Last(..))

import Mlabs.Emulator.Blockchain
import Mlabs.Emulator.Types
import Mlabs.Lending.Logic.React
import Mlabs.Lending.Logic.Types
import qualified Mlabs.Plutus.Contract.StateMachine as SM

type Lendex = SM.StateMachine (LendexId, LendingPool) Act

-- | Error type
type LendexError = SM.SMContractError

toLendexError :: String -> LendexError
toLendexError = SM.SMCContractError . fromString

{-# INLINABLE machine #-}
machine :: LendexId -> Lendex
machine lid = (SM.mkStateMachine Nothing (transition lid) isFinal)
  { SM.smCheck = checkTimestamp }
  where
    isFinal = const False

    checkTimestamp _ input ctx = maybe True check $ getInputTime input
      where
        check t = member (Slot t) range
        range = txInfoValidRange $ scriptContextTxInfo ctx

    getInputTime = \case
      UserAct time _ _  -> Just time
      PriceAct time _ _ -> Just time
      _                 -> Nothing

{-# INLINABLE mkValidator #-}
mkValidator :: LendexId -> Scripts.ValidatorType Lendex
mkValidator lid = SM.mkValidator (machine lid)

client :: LendexId -> SM.StateMachineClient (LendexId, LendingPool) Act
client lid = SM.mkStateMachineClient $ SM.StateMachineInstance (machine lid) (scriptInstance lid)

lendexValidatorHash :: LendexId -> ValidatorHash
lendexValidatorHash lid = Scripts.scriptHash (scriptInstance lid)

lendexAddress :: LendexId -> Address
lendexAddress lid = scriptHashAddress (lendexValidatorHash lid)

scriptInstance :: LendexId -> Scripts.ScriptInstance Lendex
scriptInstance lid = Scripts.validator @Lendex
  ($$(PlutusTx.compile [|| mkValidator ||])
      `PlutusTx.applyCode` (PlutusTx.liftCode lid)
  )
  $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator

{-# INLINABLE transition #-}
transition ::
     LendexId
  -> SM.State (LendexId, LendingPool)
  -> Act
  -> Maybe (SM.TxConstraints SM.Void SM.Void, SM.State (LendexId, LendingPool))
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
      UserAct _ (UserId uid) _ -> Just uid
      _                        -> Nothing

----------------------------------------------------------------------
-- specific versions of SM-functions

runStep :: forall w e schema .
  ( SM.AsSMContractError e
  , HasUtxoAt schema
  , HasWriteTx schema
  , HasOwnPubKey schema
  , HasTxConfirmation schema
  ) => LendexId -> Act -> Contract w schema e ()
runStep lid act = void $ SM.runStep (client lid) act

runStepWith :: forall w e schema .
  ( SM.AsSMContractError e
  , HasUtxoAt schema
  , HasWriteTx schema
  , HasOwnPubKey schema
  , HasTxConfirmation schema
  )
  => LendexId
  -> Act
  -> ScriptLookups Lendex
  -> TxConstraints (Scripts.RedeemerType Lendex) (Scripts.DatumType Lendex)
  -> Contract w schema e ()
runStepWith lid act lookups constraints = void $ SM.runStepWith (client lid) act lookups constraints

runInitialise :: forall w e schema .
  ( HasTxConfirmation schema
  , HasWriteTx schema
  , SM.AsSMContractError e
  ) => LendexId -> LendingPool -> Value -> Contract (Last (LendexId, LendingPool)) schema e ()
runInitialise lid lendingPool val = do
  state <- SM.runInitialise (client lid) (lid, lendingPool) val
  tell $ Last $ Just $ state
  return ()


