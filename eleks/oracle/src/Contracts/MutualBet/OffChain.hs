{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DerivingVia            #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE NumericUnderscores     #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE NumericUnderscores     #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE RecordWildCards        #-}

module Contracts.MutualBet.OffChain(
    MutualBetStartSchema,
    BettorSchema,
    MutualBetParams(..),
    BetParams(..),
    mutualBetStart,
    mutualBetBettor,
    MutualBetOutput(..),
    MutualBetError(..),
    ThreadToken,
    SM.getThreadToken
    ) where

import           Contracts.Oracle
import           Contracts.MutualBet.Types
import           Contracts.MutualBet.StateMachine
import           Control.Lens                     (makeClassyPrisms)
import           Data.Bool                        (bool)
import           Data.Aeson                       (FromJSON, ToJSON)
import           Data.Default                     (Default (def))
import           Data.Either                      (fromRight)
import qualified Data.Map                          as Map
import           Data.Maybe                       (catMaybes)
import           Data.Monoid                      (Last (..))
import           Data.Text                        (Text, pack)
import           Data.Semigroup.Generic           (GenericSemigroupMonoid (..))
import           GHC.Generics                     (Generic)
import           Ledger                           hiding (singleton, MintingPolicyHash)
import qualified Ledger
import qualified Ledger.Ada                       as Ada
import qualified Ledger.Constraints               as Constraints
import           Ledger.Constraints               (ScriptLookups (..))
import           Ledger.Constraints.TxConstraints (TxConstraints)
import qualified Ledger.Interval                  as Interval
import qualified Ledger.Oracle                    as Oracle
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
import           Types.Game

data BetParams = 
    BetParams
        { nbpAmount  :: Integer -- Bet lovelace amount 
        , nbpWinnerId :: Integer -- Bet on this team to win
        }
    deriving stock (Haskell.Eq, Haskell.Show, Generic)
    deriving anyclass (ToJSON, FromJSON, ToSchema)

type BettorSchema = Endpoint "bet" BetParams
                    .\/ Endpoint "cancelBet" BetParams
type MutualBetStartSchema = EmptySchema -- Don't need any endpoints: the contract runs automatically until the mutual bet is finished.

data MutualBetError =
    StateMachineContractError SM.SMContractError -- State machine operation failed
    | MutualBetContractError ContractError -- Endpoint, coin selection, etc. failed
    | OracleError Text -- Oracle request Error
    deriving stock (Haskell.Eq, Haskell.Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

makeClassyPrisms ''MutualBetError

instance AsContractError MutualBetError where
    _ContractError = _MutualBetContractError . _ContractError

instance SM.AsSMContractError MutualBetError where
    _SMContractError = _StateMachineContractError . SM._SMContractError

-- | The machine client of the auction state machine. It contains the script instance
--   with the on-chain code, and the Haskell definition of the state machine for
--   off-chain use.
machineClient
    :: Scripts.TypedValidator MutualBetMachine
    -> ThreadToken -- ^ Thread token of the instance
    -> MutualBetParams
    -> StateMachineClient MutualBetState MutualBetInput
machineClient inst threadToken mutualBetParams =
    let machine = mutualBetStateMachine (threadToken, mutualBetParams)
    in SM.mkStateMachineClient (SM.StateMachineInstance machine inst)
    
-- | Client code for the mutual bet contract start
mutualBetStart :: MutualBetParams -> Contract MutualBetOutput MutualBetStartSchema MutualBetError ()
mutualBetStart params = do
    threadToken <- SM.getThreadToken
    --logInfo "bet start thread token" ++ Haskell.show threadToken
    tell $ threadTokenOut threadToken
    self <- ownPubKeyHash
    let inst         = typedValidator (threadToken, params)
        client       = machineClient inst threadToken params

    _ <- handleError
            (\e -> do { logError (MutualBetFailed e); throwError (StateMachineContractError e) })
            (SM.runInitialise client (initialState self) $ Ada.toValue 0)

    logInfo $ MutualBetStarted params
    logInfo ("Request oracle for game " ++ (Haskell.show $ mbpGame params))
    _ <- mapError OracleError $ requestOracleForAddress (mbpOracle params) (mbpGame params)
    waitGameStateChange client

    where 
        waitGameStateChange client = do 
            gameState <- waitForGameStateChange params
            case osmGameStatus $ gmsSignedMessageData gameState of
                NS -> waitGameStateChange client
                FT -> payout params client gameState 
                LIVE -> do
                    logInfo @Haskell.String "Make bet over"
                    markBettingClosed params client gameState 
                    waitGameStateChange client
                CANC -> cancelGame params client gameState

payout :: 
    MutualBetParams -> 
    StateMachineClient MutualBetState MutualBetInput -> 
    GameStateChange -> 
    Contract MutualBetOutput MutualBetStartSchema MutualBetError ()
payout params client GameStateChange{gmsOutRef, gmsOutTx, gmsOracleData, gmsSignedMessage} = do
    logInfo ("Payout " ++ Haskell.show gmsOracleData)
    let oracle  = mbpOracle params
        redeemer = Redeemer $ PlutusTx.toBuiltinData $ OracleRedeem 
        oracleRequest = oracleToRequestToken oracle
        requestToken = requestTokenValue oracle
        mintRedeemer = Redeemer $ PlutusTx.toBuiltinData $ RedeemToken
    let lookups = ScriptLookups
                { slMPS = Map.singleton (oracleRequestMintPolicyHash oracleRequest) (requestTokenPolicy oracleRequest)
                , slTxOutputs = Map.singleton gmsOutRef gmsOutTx
                , slOtherScripts = Map.singleton (oracleValidatorHash oracle) (oracleValidator oracle)
                , slOtherData = Map.empty
                , slTypedValidator = Nothing
                , slOwnPubkeyHash = Nothing
                }
        constraints = Constraints.mustSpendScriptOutput gmsOutRef redeemer
                    <> Constraints.mustMintValueWithRedeemer mintRedeemer (inv requestToken)
    r <- SM.runStepWith lookups constraints client Payout{oracleValue = gmsOracleData, oracleRef = gmsOutRef, oracleSigned = gmsSignedMessage}
    case r of
        SM.TransitionFailure i            -> logError (TransitionFailed i)
        SM.TransitionSuccess (Finished h) -> logInfo $ MutualBetGameEnded h
        SM.TransitionSuccess s            -> logWarn ("Unexpected state after Payout transition: " <> Haskell.show s)

markBettingClosed :: 
    MutualBetParams -> 
    StateMachineClient MutualBetState MutualBetInput -> 
    GameStateChange -> 
    Contract MutualBetOutput MutualBetStartSchema MutualBetError ()
markBettingClosed params client GameStateChange{gmsSignedMessage, gmsOracleData} = do
    logInfo ("Close betting for in progress game " ++ Haskell.show gmsOracleData)
    r <- SM.runStep client FinishBetting{oracleSigned = gmsSignedMessage}
    case r of
        SM.TransitionFailure i                  -> logError (TransitionFailed i)
        SM.TransitionSuccess (BettingClosed h)  -> logInfo $ MutualBetBettingClosed h
        SM.TransitionSuccess s                  -> logWarn ("Unexpected state after BettingClosed transition: " <> Haskell.show s)

cancelGame :: 
    MutualBetParams -> 
    StateMachineClient MutualBetState MutualBetInput -> 
    GameStateChange -> 
    Contract MutualBetOutput MutualBetStartSchema MutualBetError ()
cancelGame params client  GameStateChange{gmsOutRef, gmsOutTx}  = do
    logInfo @Haskell.String "Cancel game"
    let oracle  = mbpOracle params
        redeemer = Redeemer $ PlutusTx.toBuiltinData $ OracleRedeem
        oracleRequest = oracleToRequestToken oracle
        requestToken = requestTokenValue oracle
        mintRedeemer = Redeemer $ PlutusTx.toBuiltinData $ RedeemToken
    let lookups = ScriptLookups
                { slMPS = Map.singleton (oracleRequestMintPolicyHash oracleRequest) (requestTokenPolicy oracleRequest)
                , slTxOutputs = Map.singleton gmsOutRef gmsOutTx
                , slOtherScripts = Map.singleton (oracleValidatorHash oracle) (oracleValidator oracle)
                , slOtherData = Map.empty
                , slTypedValidator = Nothing
                , slOwnPubkeyHash = Nothing
                }
        constraints = Constraints.mustSpendScriptOutput gmsOutRef redeemer
                    <> Constraints.mustMintValueWithRedeemer mintRedeemer (inv requestToken)
    r <- SM.runStepWith lookups constraints client CancelGame
    case r of
        SM.TransitionFailure i            -> logError (TransitionFailed i)
        SM.TransitionSuccess (Finished h) -> logInfo $ MutualBetCancelled h
        SM.TransitionSuccess s            -> logWarn ("Unexpected state after Cancel transition: " <> Haskell.show s)

-- | Get the current state of the contract and log it.
currentState :: StateMachineClient MutualBetState MutualBetInput -> Contract MutualBetOutput BettorSchema MutualBetError (Maybe MutualBetState)
currentState client = mapError StateMachineContractError (SM.getOnChainState client) >>= \case
    Just (SM.OnChainState{SM.ocsTxOut=TypedScriptTxOut{tyTxOutData=Ongoing bets}}, _) -> do
        tell $ mutualBetStateOut $ Ongoing bets
        pure (Just $ Ongoing bets)
    Just (SM.OnChainState{SM.ocsTxOut=TypedScriptTxOut{tyTxOutData=BettingClosed bets}}, _) -> do
        tell $ mutualBetStateOut $ BettingClosed bets
        pure (Just $ BettingClosed bets)
    _ -> do
        logWarn CurrentStateNotFound
        pure Nothing

{- Note [Bettor client]

In the bettor client we want to keep track of the on-chain state of the mutual bet
to give our user a chance to react on other players betting.

At the same time we want to have the "bet" endpoint active for any bets of our
own, and we want to stop the client when the auction is over.

To achieve this, we have a loop where we wait for one of several events to
happen and then deal with the event. The waiting is implemented in
@waitForChange@ and the event handling is in @handleEvent@.

Updates to the user are provided via 'tell'.

-}

isCurrentGame :: Ledger.PubKeyHash -> MutualBetParams -> OracleData -> Either Haskell.String OracleData
isCurrentGame pkh params oracleData
    | pkh /= (ovRequestAddress oracleData) = Left "Not signed by owner wallet"
    | (mbpGame params) /= (ovGame oracleData) = Left "Not current game"
    | otherwise = Right oracleData

mapSignedMessage :: MutualBetParams -> (TxOutRef, ChainIndexTxOut, OracleData) -> Maybe GameStateChange
mapSignedMessage params (oref, o, od) = case ovSignedMessage od of
    Just signed -> case Oracle.verifySignedMessageOffChain (oOperatorKey $ mbpOracle params) signed of
        Left err       -> Nothing
        Right message  -> Just $ GameStateChange{
            gmsOutRef = oref
            , gmsOutTx = o
            , gmsOracleData = od
            , gmsSignedMessage = signed
            , gmsSignedMessageData = message
            }
    Nothing -> Nothing

waitForGameStateChange ::
    MutualBetParams
    -> Contract w s MutualBetError GameStateChange
waitForGameStateChange params = do
        waitEnd
    where
        isCurrentGameState pkh params GameStateChange{gmsOracleData} = isRight $ isCurrentGame pkh params gmsOracleData
        waitEnd = do  
            txs <- mapError OracleError $ awaitNextOracleRequest (mbpOracle params)
            pkh <- ownPubKeyHash
            logInfo @Haskell.String "Await next"
            let currentGameSignedTx = find (isCurrentGameState pkh params) . catMaybes . map (mapSignedMessage params) $ txs 
            case currentGameSignedTx of
                Nothing -> do { logInfo @Haskell.String "Not current game state change"; waitEnd; }
                Just d  -> do { logInfo ("State changes " ++ Haskell.show d); return d; }

data BettorEvent =
        MutualBetIsOver [Bet] TeamId -- ^ The mutual bet is over
        | MakeBet BetParams -- ^ make a bet bet
        | UndoBet BetParams -- ^ cancel a bet
        | BettingHasСlosed [Bet] -- ^ no one can place more bets
        | OtherBet [Bet] -- ^ Another bettor make a bet
        | NoChange [Bet] -- ^ Nothing has changed
        deriving (Haskell.Show)

waitForChange :: SlotConfig -> MutualBetParams -> StateMachineClient MutualBetState MutualBetInput -> [Bet] -> Contract MutualBetOutput BettorSchema MutualBetError BettorEvent
waitForChange slotCfg params client bets = do
    now <- currentTime
    let waitFor = now + 10_000 
    smUpdatePromise <- SM.waitForUpdateTimeout client (isTime waitFor)
    let
        makeBet = endpoint @"bet" $ \params -> do 
                                        logInfo ("last bets" ++ Haskell.show params)
                                        pure $ MakeBet params
        cancelBet = endpoint @"cancelBet" $ \params -> do 
                                logInfo ("last bets" ++ Haskell.show params)
                                pure $ UndoBet params
        otherBid = do
            promiseBind
                smUpdatePromise
                 $ \case
                    ContractEnded _ input -> 
                        do 
                            case input of 
                                Payout{oracleSigned=signedMessage} -> do
                                    let winnerId = getWinnerId signedMessage
                                    logInfo ("Winner ID: " ++ Haskell.show winnerId)
                                    pure (MutualBetIsOver bets winnerId)
                                CancelGame -> pure (MutualBetIsOver bets 0)
                                _          -> pure (MutualBetIsOver bets 0)
                    -- The state machine transitionned to a new state
                    Transition _ _ SM.OnChainState{SM.ocsTxOut=TypedScriptTxOut{tyTxOutData=state}}   -> do
                        case state of 
                            (Ongoing bets) -> pure (OtherBet bets)
                            (BettingClosed bets) -> pure (BettingHasСlosed bets)
                            _ -> do
                                logInfo @Haskell.String ("Unexpected state")
                                pure (NoChange bets)
                    _ -> pure (NoChange bets)
    selectList [makeBet, cancelBet, otherBid]

getWinnerId :: (Oracle.SignedMessage OracleSignedMessage) -> TeamId
getWinnerId signedMessage = 
    case PlutusTx.fromBuiltinData . getDatum $ Oracle.osmDatum signedMessage of
        Just oracleMessage -> osmWinnerId oracleMessage
        Nothing -> 0    

handleEvent :: StateMachineClient MutualBetState MutualBetInput -> [Bet] -> BettorEvent -> Contract MutualBetOutput BettorSchema MutualBetError (Either [Bet] ())
handleEvent client bets change =
    let continue = pure . Left
        stop     = pure (Right ())
    -- see note [Bettor client]
    in case change of
        MutualBetIsOver s w -> do
            let bets' = includeWinshareInBets w bets
            logInfo ("Mutual bet over"  ++ Haskell.show bets')
            tell (mutualBetStateOut $ Finished bets')
            stop
        BettingHasСlosed s -> do
            logInfo @Haskell.String "Betting has closed"  
            logInfo ("BettingHasСlosedState: "  ++ Haskell.show s)
            tell (mutualBetStateOut $ BettingClosed s)
            continue s
        MakeBet betParams -> do
            logInfo @Haskell.String "Submitting bet"
            self <- ownPubKeyHash
            let betAda = Ada.lovelaceOf $ nbpAmount betParams
                newBetInput = NewBet{ newBet = Bet{ betAmount = betAda, betBettor = self, betTeamId = nbpWinnerId betParams, betWinShare = Ada.lovelaceOf 0}}
            r <- SM.runStep client newBetInput
            logInfo @Haskell.String "SM: runStep done"
            case r of
                SM.TransitionFailure i -> logError (TransitionFailed i) >> continue bets
                SM.TransitionSuccess (Ongoing bets)  -> do
                    tell (mutualBetStateOut $ Ongoing bets)
                    logInfo (BetSubmitted bets) >> continue bets
                SM.TransitionSuccess (BettingClosed bets)  -> logInfo (MutualBetBettingClosed bets) >> continue bets
                SM.TransitionSuccess (Finished bets) -> logError (MutualBetGameEnded bets) >> stop
        UndoBet betParams -> do
            logInfo @Haskell.String "Cancelling bet"
            self <- ownPubKeyHash
            let betAda = Ada.lovelaceOf $ nbpAmount betParams
                cancelBet = CancelBet{ cancelBet = Bet{ betAmount = betAda, betBettor = self, betTeamId = nbpWinnerId betParams, betWinShare = Ada.lovelaceOf 0}}
            r <- SM.runStep client cancelBet
            logInfo @Haskell.String "SM: runStep done"
            case r of
                SM.TransitionFailure i -> logError (TransitionFailed i) >> continue bets
                SM.TransitionSuccess (Ongoing bets)  -> do
                    tell (mutualBetStateOut $ Ongoing bets)
                    logInfo (BetCancelled bets) >> continue bets
                SM.TransitionSuccess (BettingClosed bets)  -> logInfo (MutualBetBettingClosed bets) >> continue bets
                SM.TransitionSuccess (Finished bets) -> logError (MutualBetGameEnded bets) >> stop
        OtherBet s -> do
            logInfo @Haskell.String "SM: OtherBet"
            tell (mutualBetStateOut $ Ongoing s)
            continue s
        NoChange s -> do
            continue s

mutualBetBettor :: SlotConfig -> ThreadToken -> MutualBetParams -> Contract MutualBetOutput BettorSchema MutualBetError ()
mutualBetBettor slotCfg currency params = do
    let inst   = typedValidator (currency, params)
        client = machineClient inst currency params

        -- the actual loop, see note [Bettor client]
        loop         = loopM (\h -> waitForChange slotCfg params client h >>= handleEvent client h)
    tell $ threadTokenOut currency
    initial <- currentState client
    case initial of
        Just (Ongoing bets) -> loop bets
        Just (BettingClosed bets) -> loop bets
        -- If the state can't be found we wait for it to appear.
        Nothing -> SM.waitForUpdate client >>= \case
            Just SM.OnChainState{SM.ocsTxOut=TypedScriptTxOut{tyTxOutData=Ongoing bets}}       -> loop bets
            Just SM.OnChainState{SM.ocsTxOut=TypedScriptTxOut{tyTxOutData=BettingClosed bets}} -> loop bets
            _                -> logWarn CurrentStateNotFound
