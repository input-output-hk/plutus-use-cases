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

module Contracts.MutualBet.OffChain(
    MutualBetState(..),
    MutualBetInput(..),
    MutualBetStartSchema,
    BettorSchema,
    MutualBetParams(..),
    NewBetParams(..),
    Bet(..),
    mutualBetStart,
    mutualBetBettor,
    MutualBetOutput(..),
    MutualBetError(..),
    ThreadToken,
    SM.getThreadToken
    ) where

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
import           Contracts.Oracle

-- | Definition of an mutual bet
data MutualBetParams
    = MutualBetParams
        { mbpGame   :: Integer -- ^ Game id
        , mbpOracle :: Oracle
        , mbpTeam1 :: Integer
        , mbpTeam2 :: Integer
        }
        deriving stock (Haskell.Eq, Haskell.Show, Generic)
        deriving anyclass (ToJSON, FromJSON, ToSchema)

PlutusTx.makeLift ''MutualBetParams

data NewBetParams = 
    NewBetParams
        { nbpAmount  :: Ada
        , nbpOutcome :: Integer
        }
    deriving stock (Haskell.Eq, Haskell.Show, Generic)
    deriving anyclass (ToJSON, FromJSON, ToSchema)

data Bet =
    Bet
        { amount  :: Ada
        , bettor  :: PubKeyHash
        , outcome :: Integer
        }
    deriving stock (Haskell.Eq, Haskell.Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

PlutusTx.unstableMakeIsData ''Bet

-- | The states of the auction
data MutualBetState
    = Ongoing [Bet] -- Bids can be submitted.
    | Finished [Bet] -- The auction is finished
    deriving stock (Generic, Haskell.Show, Haskell.Eq)
    deriving anyclass (ToJSON, FromJSON)

-- | Observable state of the mutual bet app
data MutualBetOutput =
    MutualBetOutput
        { mutualBetState       :: Last MutualBetState
        , mutualBetThreadToken :: Last ThreadToken
        }
        deriving stock (Generic, Haskell.Show, Haskell.Eq)
        deriving anyclass (ToJSON, FromJSON)

deriving via (GenericSemigroupMonoid MutualBetOutput) instance (Haskell.Semigroup MutualBetOutput)
deriving via (GenericSemigroupMonoid MutualBetOutput) instance (Haskell.Monoid MutualBetOutput)

mutualBetStateOut :: MutualBetState -> MutualBetOutput
mutualBetStateOut s = Haskell.mempty { mutualBetState = Last (Just s) }

threadTokenOut :: ThreadToken -> MutualBetOutput
threadTokenOut t = Haskell.mempty { mutualBetThreadToken = Last (Just t) }

-- | Initial 'MutualBetState'. In the beginning there are no bets
initialState :: PubKeyHash -> MutualBetState
initialState self = Ongoing []

PlutusTx.unstableMakeIsData ''MutualBetState

-- | Transition between auction states
data MutualBetInput
    = NewBet { newBet :: Ada, newBettor :: PubKeyHash, newOutcome :: Integer } -- Increase the price
    | Payout { oracleValue :: OracleData }
    deriving stock (Generic, Haskell.Show)
    deriving anyclass (ToJSON, FromJSON)

PlutusTx.unstableMakeIsData ''MutualBetInput

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
        (Ongoing bets, Payout{oracleValue}) ->
            let 
                winners = getWinners (ovWinner oracleValue) bets
                constraints = foldMap mkTxPayWinners winners
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

type BettorSchema = Endpoint "bet" NewBetParams
type MutualBetStartSchema = EmptySchema -- Don't need any endpoints: the contract runs automatically until the auction is finished.

data MutualBetLog =
    MutualBetStarted MutualBetParams
    | MutualBetFailed SM.SMContractError
    | BetSubmitted [Bet]
    | MutualBetEnded [Bet]
    | CurrentStateNotFound
    | TransitionFailed (SM.InvalidTransition MutualBetState MutualBetInput)
    deriving stock (Haskell.Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data MutualBetError =
    StateMachineContractError SM.SMContractError -- ^ State machine operation failed
    | MutualBetContractError ContractError -- ^ Endpoint, coin selection, etc. failed
    | OracleError Text
    deriving stock (Haskell.Eq, Haskell.Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

makeClassyPrisms ''MutualBetError

instance AsContractError MutualBetError where
    _ContractError = _MutualBetContractError . _ContractError

instance SM.AsSMContractError MutualBetError where
    _SMContractError = _StateMachineContractError . SM._SMContractError

-- | Client code for the mutual bet contract start
mutualBetStart :: MutualBetParams -> Contract MutualBetOutput MutualBetStartSchema MutualBetError ()
mutualBetStart params = do
    threadToken <- SM.getThreadToken
    --logInfo "bet start thread token" ++ Haskell.show threadToken
    tell $ threadTokenOut threadToken
    self <- Ledger.pubKeyHash <$> ownPubKey
    let inst         = typedValidator (threadToken, params)
        client       = machineClient inst threadToken params

    _ <- handleError
            (\e -> do { logError (MutualBetFailed e); throwError (StateMachineContractError e) })
            (SM.runInitialise client (initialState self) $ Ada.toValue 0)

    logInfo $ MutualBetStarted params
    _ <- mapError OracleError $ requestOracleForAddress (mbpOracle params) (mbpGame params)
    ov <- waitForGameEnd params
    logInfo @Haskell.String "Payout"
    r <- SM.runStep client Payout{oracleValue = ov}
    case r of
        SM.TransitionFailure i            -> logError (TransitionFailed i) -- TODO: Add an endpoint "retry" to the seller?
        SM.TransitionSuccess (Finished h) -> logInfo $ MutualBetEnded h
        SM.TransitionSuccess s            -> logWarn ("Unexpected state after Payout transition: " <> Haskell.show s)


-- | Get the current state of the contract and log it.
currentState :: StateMachineClient MutualBetState MutualBetInput -> Contract MutualBetOutput BettorSchema MutualBetError (Maybe [Bet])
currentState client = mapError StateMachineContractError (SM.getOnChainState client) >>= \case
    Just (SM.OnChainState{SM.ocsTxOut=TypedScriptTxOut{tyTxOutData=Ongoing bets}}, _) -> do
        tell $ mutualBetStateOut $ Ongoing bets
        pure (Just bets)
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

isGameCompleted :: Ledger.PubKey -> MutualBetParams -> OracleData -> Either Haskell.String OracleData
isGameCompleted pk params oracleData
    | (pubKeyHash pk) /= (ovRequestAddress oracleData) = Left "Not signed by current script"
    | (mbpGame params) /= (ovGame oracleData) = Left "Not current game"
    | isNothing (ovWinnerSigned oracleData) = Left "Not signed"
    | otherwise = case Oracle.verifySignedMessageOffChain 
            (oOperatorKey $ mbpOracle params) (fromJust $ ovWinnerSigned oracleData) of
                Left err       -> Left "Sign error"
                Right winnerId -> 
                    if winnerId > 0 
                    then Right oracleData
                    else Left "Winner is empty"

waitForGameEnd ::
    MutualBetParams
    -> Contract w s MutualBetError OracleData
waitForGameEnd params = do
        waitEnd
    where 
        waitEnd = do  
            txs <- mapError OracleError $ awaitNextOracleRequest (mbpOracle params)
            pk <- ownPubKey
            logInfo @Haskell.String "Await next"

            let currentGameSignedTx = find (\(_, _, oracleData) -> isRight $ isGameCompleted pk params oracleData) txs
            case currentGameSignedTx of
                Nothing -> do { logInfo @Haskell.String "Not completed"; waitEnd; }
                Just (_, _, oracleData) -> return oracleData

     
data BettorEvent =
        MutualBetIsOver [Bet] -- ^ The mutual bet is over
        | MakeBet NewBetParams -- ^ make a bet bet
        | OtherBet [Bet] -- ^ Another bettor make a bet
        | NoChange [Bet] -- ^ Nothing has changed
        deriving (Haskell.Show)
waitForChange :: SlotConfig -> MutualBetParams -> StateMachineClient MutualBetState MutualBetInput -> [Bet] -> Contract MutualBetOutput BettorSchema MutualBetError BettorEvent
waitForChange slotCfg params client bets = do
    t <- currentTime
    logInfo @Haskell.String "waitForChange"
    let
        makeBet = endpoint @"bet" $ \params -> do 
                                        logInfo ("last bets" ++ Haskell.show params)
                                        pure $ MakeBet params
        otherBid = do
            let address = Scripts.validatorAddress (SM.typedValidator (SM.scInstance client))
                targetTime = TimeSlot.slotToBeginPOSIXTime slotCfg
                           $ Haskell.succ
                           $ TimeSlot.posixTimeToEnclosingSlot slotCfg t
            promiseBind
                (addressChangeRequest
                    AddressChangeRequest
                    { acreqSlotRangeFrom = TimeSlot.posixTimeToEnclosingSlot slotCfg targetTime
                    , acreqSlotRangeTo = TimeSlot.posixTimeToEnclosingSlot slotCfg targetTime
                    , acreqAddress = address
                    })
                $ \AddressChangeResponse{acrTxns} ->
                    case acrTxns of
                        [] -> do
                            state <- currentState client
                            case state of
                               Nothing -> pure (MutualBetIsOver bets)
                               _       -> pure (NoChange bets)
                
                        _  -> maybe (MutualBetIsOver bets) OtherBet <$> currentState client

    selectList [makeBet, otherBid]

handleEvent :: StateMachineClient MutualBetState MutualBetInput -> [Bet] -> BettorEvent -> Contract MutualBetOutput BettorSchema MutualBetError (Either [Bet] ())
handleEvent client bets change =
    let continue = pure . Left
        stop     = pure (Right ())
    -- see note [Buyer client]
    in case change of
        MutualBetIsOver s -> do
            logInfo @Haskell.String "Mutual bet over 11"  
            tell (mutualBetStateOut $ Finished s)
            stop
        MakeBet betParams -> do
            logInfo @Haskell.String "Submitting bid"
            self <- Ledger.pubKeyHash <$> ownPubKey
            logInfo @Haskell.String "Received pubkey"
            let newBet = NewBet{newBet = nbpAmount betParams, newBettor = self, newOutcome = nbpOutcome betParams}
                bet1 = Bet{amount = nbpAmount betParams, bettor = self, outcome = nbpOutcome betParams} 
            r <- SM.runStep client newBet
            logInfo @Haskell.String "SM: runStep done"
            case r of
                SM.TransitionFailure i -> logError (TransitionFailed i) >> continue bets
                SM.TransitionSuccess (Ongoing bets) -> logInfo (BetSubmitted bets) >> continue bets
                SM.TransitionSuccess (Finished bets) -> logError (MutualBetEnded bets) >> stop
        OtherBet s -> do
            logInfo @Haskell.String "SM: OtherBet"
            tell (mutualBetStateOut $ Ongoing s)
            continue s
        NoChange s -> do
            logInfo @Haskell.String "SM: No Change"
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
        Just bets -> loop bets

        -- If the state can't be found we wait for it to appear.
        Nothing -> SM.waitForUpdate client >>= \case
            Just SM.OnChainState{SM.ocsTxOut=TypedScriptTxOut{tyTxOutData=Ongoing bets}} -> loop bets
            _                -> logWarn CurrentStateNotFound
