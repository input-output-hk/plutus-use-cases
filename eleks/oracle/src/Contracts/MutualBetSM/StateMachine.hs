{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE ImportQualifiedPost   #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
module Contracts.MutualBetSM.StateMachine(
    typedValidator
    , mutualBetStateMachine
    , MutualBetMachine
    , includeWinshareInBets
    ) where

import Contracts.MutualBetSM.Types
import Contracts.Oracle
import Data.Bool (bool)
import Ledger hiding (MintingPolicyHash, singleton)
import Ledger.Ada qualified as Ada
import Ledger.Constraints qualified as Constraints
import Ledger.Constraints.TxConstraints (TxConstraints)
import Ledger.Typed.Scripts qualified as Scripts
import Plutus.Contract.StateMachine (State (..), StateMachine (..), ThreadToken, Void)
import Plutus.Contract.StateMachine qualified as SM
import PlutusTx qualified
import PlutusTx.Prelude

type MutualBetMachine = StateMachine MutualBetState MutualBetInput

{-# INLINABLE betsValueAmount #-}
-- | The combined value of bets.
betsValueAmount :: [Bet] -> Ada
betsValueAmount = fold . map (\bet -> betAmount bet)

{-# INLINABLE winBets #-}
-- | Get winning bets.
winBets :: Integer -> [Bet] -> [Bet]
winBets winnerTeamId bets = filter (\bet -> betTeamId bet == winnerTeamId) bets

{-# INLINABLE calculatePrize #-}
calculatePrize:: Bet -> Ada -> Ada -> Ada
calculatePrize bet totalBets totalWin =
    let
        totalPrize = totalBets - totalWin
        amount = betAmount bet
    in
        bool ((Ada.divide (amount * totalPrize ) totalWin)) 0 (totalWin == 0)

{-# INLINABLE calculateWinnerShare #-}
calculateWinnerShare :: Bet -> Ada -> Ada -> Ada
calculateWinnerShare bet totalBets totalWin = calculatePrize bet totalBets totalWin

{-# INLINABLE getWinners #-}
getWinners :: Integer -> [Bet] -> [(PubKeyHash, Ada, Ada)]
getWinners winnerTeamId bets =
    let
        winnerBets = winBets winnerTeamId bets
        total = betsValueAmount bets
        totalWin = betsValueAmount $ winnerBets
    in
        map (\winBet -> (betBettor winBet, betAmount winBet, calculateWinnerShare winBet total totalWin)) winnerBets

{-# INLINABLE includeWinshareInBets #-}
includeWinshareInBets  :: Integer -> [Bet] -> [Bet]
includeWinshareInBets winnerTeamId bets =
    let
        winnerBets = winBets winnerTeamId bets
        total = betsValueAmount bets
        totalWin = betsValueAmount $ winnerBets
    in
    map (\bet -> bet{
        betWinShare = if betTeamId bet == winnerTeamId
            then calculateWinnerShare bet total totalWin
            else Ada.lovelaceOf 0
        }) bets

{-# INLINABLE mkTxPayWinners #-}
mkTxPayWinners :: [(PubKeyHash, Ada, Ada)]-> TxConstraints Void Void
mkTxPayWinners = foldMap (\(winnerAddressHash, winnerBetAmount, winnerPrize) -> Constraints.mustPayToPubKey winnerAddressHash $ Ada.toValue $ winnerBetAmount + winnerPrize)

{-# INLINABLE mkTxReturnBets #-}
mkTxReturnBets :: [Bet] -> TxConstraints Void Void
mkTxReturnBets = foldMap (\bet -> Constraints.mustPayToPubKey (betBettor bet) $ Ada.toValue (betAmount bet))

{-# INLINABLE isValidBet #-}
isValidBet ::  MutualBetParams -> Bet -> Bool
isValidBet MutualBetParams{mbpTeam1, mbpTeam2, mbpMinBet} Bet{betAmount, betTeamId, betWinShare}
    | betAmount < mbpMinBet = False
    | mbpTeam1 /= betTeamId && mbpTeam2 /= betTeamId = False
    | betWinShare /= 0 = False
    | otherwise = True

{-# INLINABLE deleteFirstOccurence #-}
deleteFirstOccurence :: Bet -> [Bet] -> [Bet]
deleteFirstOccurence bet (x:xs)
    | (bet==x) = xs
    | otherwise = x : deleteFirstOccurence bet xs

{-# INLINABLE mutualBetTransition #-}
-- | The transitions of the mutual bet state machine.
mutualBetTransition :: MutualBetParams -> State MutualBetState -> MutualBetInput -> Maybe (TxConstraints Void Void, State MutualBetState)
mutualBetTransition params@MutualBetParams{mbpOracle, mbpOwner, mbpBetFee} State{stateData=oldStateData, stateValue=oldStateValue} input =
    case (oldStateData, input) of
        (Ongoing bets, NewBet{newBet})
            | isValidBet params newBet ->
                let constraints = Constraints.mustPayToPubKey mbpOwner $ Ada.toValue mbpBetFee
                    newBets = newBet:bets
                    newState =
                        State
                            { stateData = Ongoing newBets
                            , stateValue = oldStateValue <> (Ada.toValue $ betAmount newBet)
                            }
                in Just (constraints, newState)
        (Ongoing bets, CancelBet{cancelBet})
            | elem cancelBet bets ->
                let constraints = Constraints.mustPayToPubKey (betBettor cancelBet) $ Ada.toValue (betAmount cancelBet)
                    newBets = deleteFirstOccurence cancelBet bets
                    newState =
                        State
                            { stateData = Ongoing newBets
                            , stateValue = oldStateValue <> inv (Ada.toValue $ betAmount cancelBet)
                            }
                in Just (constraints, newState)
        (Ongoing bets, FinishBetting{oracleSigned})
            | Just (OracleSignedMessage{osmWinnerId}, oracleSignConstraints) <- verifyOracleValueSigned (oOperatorKey mbpOracle) oracleSigned ->
                let constraints = mempty
                    newState =
                        State
                            { stateData = BettingClosed bets
                            , stateValue = oldStateValue
                            }
                in Just (constraints, newState)
        (Ongoing bets, CancelGame) ->
                let constraints = mkTxReturnBets bets
                    newState = State{ stateData = Finished bets, stateValue = mempty }
                in Just (constraints, newState)
        (BettingClosed bets, Payout{oracleValue, oracleRef, oracleSigned})
            | Just (OracleSignedMessage{osmWinnerId}, oracleSignConstraints) <- verifyOracleValueSigned (oOperatorKey mbpOracle) oracleSigned ->
                let
                    winners = getWinners osmWinnerId bets
                    payConstraints = if null winners
                        then mkTxReturnBets bets
                        else mkTxPayWinners winners
                    constraints = payConstraints
                                <> oracleSignConstraints


                    newState = State { stateData = Finished bets, stateValue = mempty }
                in Just (constraints, newState)
        _ -> Nothing

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
