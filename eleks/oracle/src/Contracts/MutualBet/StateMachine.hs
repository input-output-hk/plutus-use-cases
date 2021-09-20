{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DerivingVia            #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE RecordWildCards        #-}

module Contracts.MutualBet.StateMachine(
    typedValidator
    , mutualBetStateMachine
    , MutualBetMachine
    ) where

import           Contracts.Oracle
import           Contracts.MutualBet.Types
import           Control.Lens                     (makeClassyPrisms)
import           Data.Bool                        (bool)
import           Data.Aeson                       (FromJSON, ToJSON)
import           Data.Default                     (Default (def))
import           Data.Either                      (fromRight)
import           Data.Maybe                       (fromJust)
import           Data.Monoid                      (Last (..))
import           Data.Text                        (Text, pack)
import           Data.Semigroup.Generic           (GenericSemigroupMonoid (..))
import           GHC.Generics                     (Generic)
import           Ledger                           hiding (singleton, MintingPolicyHash)
import qualified Ledger
import qualified Ledger.Ada                       as Ada
import qualified Ledger.Constraints               as Constraints
import           Ledger.Constraints.TxConstraints (TxConstraints)
import qualified Ledger.Interval                  as Interval
import qualified Ledger.Oracle                    as Oracle
import qualified Ledger.Value                     as Value
import           Ledger.TimeSlot                  (SlotConfig)
import qualified Ledger.TimeSlot                  as TimeSlot
import qualified Ledger.Typed.Scripts             as Scripts
import           Ledger.Typed.Tx                  (TypedScriptTxOut (..))
import           Schema                           (ToSchema)
import           Plutus.Contract
import           Plutus.Contract.StateMachine     (State (..), StateMachine (..), StateMachineClient, ThreadToken, Void,
                                                   WaitingResult (..))
import qualified Plutus.Contract.StateMachine     as SM
import           Plutus.Contract.Types            (Promise (..))
import           Plutus.Contract.Util             (loopM)
import qualified PlutusTx
import           PlutusTx.Prelude
import qualified Prelude                          as Haskell

type MutualBetMachine = StateMachine MutualBetState MutualBetInput

{-# INLINABLE betsValueAmount #-}
-- | The combined value of bets.
betsValueAmount :: [Bet] -> Ada
betsValueAmount = fold . map (\bet -> amount bet)

{-# INLINABLE winBets #-}
-- | Get winning bets.
winBets :: Integer -> [Bet] -> [Bet]
winBets gameOutcome bets = filter (\bet -> outcome bet == gameOutcome) bets

{-# INLINABLE calculatePrize #-}
calculatePrize:: Bet -> Ada -> Ada -> Ada
calculatePrize bet totalBets totalWin =
    let 
        totalPrize = totalBets - totalWin
        betAmount = amount bet
    in
        bool ((Ada.divide betAmount totalWin) * totalPrize) 0 (totalWin == 0)
        

{-# INLINABLE calculateWinnerShare #-}
calculateWinnerShare :: Bet -> Ada -> Ada -> Ada
calculateWinnerShare bet totalBets totalWin = 
    (amount bet) + calculatePrize bet totalBets totalWin

{-# INLINABLE getWinners #-}
getWinners :: Integer -> [Bet] -> [(PubKeyHash, Ada)]
getWinners gameOutcome bets = 
    let 
        winnerBets = winBets gameOutcome bets
        total = betsValueAmount bets
        totalWin = betsValueAmount $ winnerBets
    in 
        map (\winBet -> (bettor winBet, calculateWinnerShare winBet total totalWin)) winnerBets

{-# INLINABLE mkTxPayWinners #-}
mkTxPayWinners :: (PubKeyHash, Ada)-> TxConstraints Void Void
mkTxPayWinners (winnerAddressHash, winnerPrize) = Constraints.mustPayToPubKey winnerAddressHash $ Ada.toValue winnerPrize

{-# INLINABLE mutualBetTransition #-}
-- | The transitions of the auction state machine.
mutualBetTransition :: MutualBetParams -> State MutualBetState -> MutualBetInput -> Maybe (TxConstraints Void Void, State MutualBetState)
mutualBetTransition MutualBetParams{mbpOracle} State{stateData=oldState} input =
    case (oldState, input) of

        (Ongoing bets, NewBet{newBet, newBettor, newOutcome}) ->
            let constraints = mempty
                newBets = Bet{amount = newBet, bettor = newBettor, outcome = newOutcome}:bets
                newState =
                    State
                        { stateData = Ongoing newBets
                        , stateValue = Ada.toValue $ betsValueAmount newBets
                        }
            in Just (constraints, newState)
        (Ongoing bets, Payout{oracleValue, oracleRef})
          | isValueSigned oracleValue ->
            let 
                winners = getWinners (ovWinner oracleValue) bets
                redeemer = Redeemer $ PlutusTx.toBuiltinData $ Use
                constraints = foldMap mkTxPayWinners winners 
                              <> Constraints.mustSpendScriptOutput oracleRef redeemer
                newState = State { stateData = Finished bets, stateValue = mempty }
            in Just (constraints, newState)
        _ -> Nothing
    where isValueSigned oracleValue = isJust $ verifyOracleValueSigned (ovWinnerSigned oracleValue) (oOperatorKey mbpOracle)


{-# INLINABLE mutualBetStateMachine #-}
mutualBetStateMachine :: (ThreadToken, MutualBetParams) -> MutualBetMachine
mutualBetStateMachine (threadToken, params) = SM.mkStateMachine (Just threadToken) (mutualBetTransition params) isFinal where
    isFinal Finished{} = True
    isFinal _          = False

{-# INLINABLE mkValidator #-}
mkValidator :: (ThreadToken, MutualBetParams) -> Scripts.ValidatorType MutualBetMachine
mkValidator = SM.mkValidator . mutualBetStateMachine

-- | The script instance of the auction state machine. It contains the state
--   machine compiled to a Plutus core validator script.
typedValidator :: (ThreadToken, MutualBetParams) -> Scripts.TypedValidator MutualBetMachine
typedValidator = Scripts.mkTypedValidatorParam @MutualBetMachine
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.wrapValidator