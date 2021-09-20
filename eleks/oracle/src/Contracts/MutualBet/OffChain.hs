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
    MutualBetStartSchema,
    BettorSchema,
    MutualBetParams(..),
    NewBetParams(..),
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
import           Data.Maybe                       (fromJust)
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

data NewBetParams = 
    NewBetParams
        { nbpAmount  :: Integer
        , nbpOutcome :: Integer
        }
    deriving stock (Haskell.Eq, Haskell.Show, Generic)
    deriving anyclass (ToJSON, FromJSON, ToSchema)

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
    self <- Ledger.pubKeyHash <$> ownPubKey
    let inst         = typedValidator (threadToken, params)
        client       = machineClient inst threadToken params
        oracle       = mbpOracle params

    _ <- handleError
            (\e -> do { logError (MutualBetFailed e); throwError (StateMachineContractError e) })
            (SM.runInitialise client (initialState self) $ Ada.toValue 0)

    logInfo $ MutualBetStarted params
    _ <- mapError OracleError $ requestOracleForAddress (mbpOracle params) (mbpGame params)
    (oref, o, ov) <- waitForGameEnd params
    logInfo ("Payout " ++ Haskell.show ov)
    let lookups = ScriptLookups
                { slMPS = Map.empty
                , slTxOutputs = Map.singleton oref o
                , slOtherScripts = Map.singleton (oracleAddress oracle) (oracleValidator oracle)
                , slOtherData = Map.empty
                , slTypedValidator = Nothing
                , slOwnPubkey = Nothing
                }
    r <- SM.runStepWith lookups mempty client Payout{oracleValue = ov, oracleRef = oref}
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
    -> Contract w s MutualBetError (TxOutRef, TxOutTx, OracleData)
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
                Just d -> return d

     
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
            logInfo @Haskell.String "Mutual bet over"  
            tell (mutualBetStateOut $ Finished s)
            stop
        MakeBet betParams -> do
            logInfo @Haskell.String "Submitting bid"
            self <- Ledger.pubKeyHash <$> ownPubKey
            logInfo @Haskell.String "Received pubkey"
            let betAda = Ada.lovelaceOf $ nbpAmount betParams
                newBet = NewBet{newBet = betAda, newBettor = self, newOutcome = nbpOutcome betParams}
                bet1 = Bet{amount = betAda, bettor = self, outcome = nbpOutcome betParams} 
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
