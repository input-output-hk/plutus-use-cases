{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost        #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE NumericUnderscores         #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
-- | Implements a custom currency with a monetary policy that allows
--   the forging of a fixed amount of units.
module Contracts.MutualBet.OffChain
   where

import Contracts.MutualBet.OnChain
import Contracts.MutualBet.Types
import Contracts.Oracle
import Control.Lens (view)
import Control.Monad hiding (fmap)
import Data.ByteString.Char8 qualified as B
import Data.List (sortOn)
import Data.Map qualified as Map
import Data.Maybe (catMaybes)
import Data.Monoid (Last (..))
import Data.Ord (comparing)
import Data.Proxy (Proxy (..))
import Data.String (fromString)
import Data.Text (Text, pack)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Void (Void, absurd)
import Ledger hiding (singleton)
import Ledger.Ada qualified as Ada
import Ledger.Constraints (adjustUnbalancedTx)
import Ledger.Constraints qualified as Constraints
import Ledger.Constraints.OnChain as Constraints
import Ledger.Constraints.TxConstraints as Constraints
import Ledger.Contexts qualified as Validation
import Ledger.Scripts (unitRedeemer)
import Ledger.Typed.Scripts (TypedValidator)
import Ledger.Typed.Scripts qualified as Scripts
import Ledger.Value (AssetClass (..), CurrencySymbol (..), assetClass, assetClassValue, assetClassValueOf, symbols,
                     toString, unCurrencySymbol, unTokenName, valueOf)
import Ledger.Value qualified as Value
import Playground.Contract
import Plutus.Contract hiding (when)
import Plutus.Contract.Oracle (SignedMessage (..), verifySignedMessageOffChain)
-- import qualified Contracts.NFT.Currency        as Currency
import Contracts.MutualBet.Currency as Currency
import Contracts.Oracle
import Plutus.Contract.Util (loopM)
import PlutusTx qualified
import PlutusTx.Prelude hiding (Semigroup (..), unless)
import Prelude (Char, Semigroup (..), String, read, show)
import Prelude qualified as Haskell
import Text.Printf (printf)
import Types.Game

mutualBetTokenName :: TokenName
mutualBetTokenName =  "MutualBet"

data BetParams =
    BetParams
        { nbpAmount   :: Integer -- Bet lovelace amount
        , nbpWinnerId :: Integer -- Bet on this team to win
        }
    deriving stock (Haskell.Eq, Haskell.Show, Generic)
    deriving anyclass (ToJSON, FromJSON, ToSchema)

start ::
    forall w s.
    AssetClass
    -> MutualBetStartParams
    -> Contract w s Text MutualBetParams
start mutualBetTokenClass startParams = do
    let mutualBet = MutualBetParams {
                  mbpGame = mbspGame startParams
                , mbpOracle = mbspOracle startParams
                , mbpOwner = mbspOwner startParams
                , mbpTeam1 = mbspTeam1 startParams
                , mbpTeam2 = mbspTeam2 startParams
                , mbpMinBet = mbspMinBet startParams
                , mbpBetFee = mbspBetFee startParams
                , mbpMutualBetId = mutualBetTokenClass
                }
        oracle = mbspOracle startParams
        inst = typedMutualBetValidator mutualBet
        tx   = mustPayToTheScript (MutualBetDatum [] BettingOpen) (assetClassValue mutualBetTokenClass 1 <> (Ada.toValue Ledger.minAdaTxOut))
               -- todo: add oracle integration ,. pay to oracle
               <> Constraints.mustPayToPubKey (oOperator oracle) (Ada.toValue (oFee oracle))
    mkTxConstraints (Constraints.typedValidatorLookups inst) tx
      >>= submitTxConfirmed . adjustUnbalancedTx

    logInfo @String $ printf "started Mutual bet %s at address %s" (show mutualBet) (show $ mutualBetAddress mutualBet)
    return mutualBet

startWithOracle ::
    forall s.
    AssetClass
    -> MutualBetStartParams
    -> Contract (Last (Either Text MutualBetParams)) s Text ()
startWithOracle mutualBetTokenClass startParams = do
    logInfo @String $ printf "startWithOracle"
    let params = MutualBetParams {
                  mbpGame = mbspGame startParams
                , mbpOracle = mbspOracle startParams
                , mbpOwner = mbspOwner startParams
                , mbpTeam1 = mbspTeam1 startParams
                , mbpTeam2 = mbspTeam2 startParams
                , mbpMinBet = mbspMinBet startParams
                , mbpBetFee = mbspBetFee startParams
                , mbpMutualBetId = mutualBetTokenClass
                }
        inst = typedMutualBetValidator params
        tx   = mustPayToTheScript (MutualBetDatum [] BettingOpen) (assetClassValue mutualBetTokenClass 1 <> (Ada.toValue Ledger.minAdaTxOut))
    mkTxConstraints (Constraints.typedValidatorLookups inst) tx
      >>= submitTxConfirmed . adjustUnbalancedTx
    logInfo @String $ printf "started Mutual bet %s at address %s" (show params) (show $ mutualBetAddress params)
    tell $ Last $ Just $ Right params

    logInfo ("Request oracle for game " ++ (Haskell.show $ mbpGame params))
    _ <- requestOracleForAddress (mbpOracle params) (mbpGame params)
    waitGameStateChange params
    where
        waitGameStateChange:: MutualBetParams -> Contract (Last (Either Text MutualBetParams)) s Text ()
        waitGameStateChange params = do
            gameState <- waitForGameStateChange params
            let signedMessage = gmsSignedMessage gameState
            let message = gmsSignedMessageData gameState
            let gameId = (osmGameId message)
            let oracle = (mbpOracle params)
            case osmGameStatus message of
                NS -> waitGameStateChange params
                LIVE -> do
                    logInfo @Haskell.String "Closing betting"
                    _ <- startGame params signedMessage
                    waitGameStateChange params
                FT -> do
                    logInfo @Haskell.String "Payout"
                    payout params signedMessage (osmWinnerId message)
                    redeemOracleRequest oracle gameId
                    void $ deleteGame params signedMessage
                CANC -> do
                    logInfo @Haskell.String "Cancel game"
                    cancel params signedMessage
                    redeemOracleRequest oracle gameId
                    void $ deleteGame params signedMessage



isCurrentGame :: Ledger.PubKeyHash -> MutualBetParams -> OracleData -> Either Haskell.String OracleData
isCurrentGame pkh params oracleData
    | pkh /= (ovRequestAddress oracleData) = Left "Not signed by owner wallet"
    | (mbpGame params) /= (ovGame oracleData) = Left "Not current game"
    | otherwise = Right oracleData

mapSignedMessage :: MutualBetParams -> (TxOutRef, ChainIndexTxOut, OracleData) -> Maybe GameStateChange
mapSignedMessage params (oref, o, od) = case ovSignedMessage od of
    Just signed -> case verifySignedMessageOffChain (oOperatorKey $ mbpOracle params) signed of
        Left _       -> Nothing
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
    -> Contract w s Text GameStateChange
waitForGameStateChange params = do
        waitEnd
    where
        isCurrentGameState pkh GameStateChange{gmsOracleData} = isRight $ isCurrentGame pkh params gmsOracleData
        waitEnd = do
            txs <- awaitNextOracleRequest (mbpOracle params)
            pkh <- ownPubKeyHash
            logInfo @Haskell.String "Await next"
            let currentGameSignedTx = find (isCurrentGameState pkh) . catMaybes . map (mapSignedMessage params) $ txs
            case currentGameSignedTx of
                Nothing -> do { logInfo @Haskell.String "Not current game state change"; waitEnd; }
                Just d  -> do { logInfo ("State changes " ++ Haskell.show d); return d; }

startGame :: forall w s. MutualBetParams
    -> SignedMessage OracleSignedMessage
    -> Contract w s Text ()
startGame mbParams signedMessage = do
    (oref, o, MutualBetDatum bets _) <- findMutualBetInstance mbParams

    --when (not isActiveBetting) $ throwError "game is already started"

    let inst = typedMutualBetValidator mbParams
        mrScript = mutualBetValidator mbParams
        betsAmount = betsValueAmount bets
        mutualBetDatum   = MutualBetDatum bets BettingClosed
        mutualBetToken    = assetClassValue (mbpMutualBetId mbParams) 1
        lookups  = Constraints.typedValidatorLookups inst
                   <> Constraints.otherScript mrScript
                   <> Constraints.unspentOutputs (Map.singleton oref o)
        tx       = Constraints.mustPayToTheScript mutualBetDatum (mutualBetToken <> Ada.toValue(betsAmount) <> (Ada.toValue Ledger.minAdaTxOut))
                   <> Constraints.mustPayToPubKey (mbpOwner mbParams) (Ada.toValue (mbpBetFee mbParams))
                   <> Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData $ StartGame signedMessage)
    mkTxConstraints lookups tx >>= submitTxConfirmed . adjustUnbalancedTx
    logInfo @String $ "game started"

payout ::
    forall w s. MutualBetParams
    -> SignedMessage OracleSignedMessage
    -> TeamId
    -> Contract w s Text ()
payout mbParams signedMessage winnerId = do
    (oref, o, MutualBetDatum bets _) <- findMutualBetInstance mbParams

    let winners = getWinners winnerId bets
        payConstraints = if null winners
            then mkTxReturnBets bets
            else mkTxPayWinners winners
    let bets' = mapWinshare winnerId bets

    logInfo $ "winners: " ++ show winners
    let inst = typedMutualBetValidator mbParams
        mrScript = mutualBetValidator mbParams
        mutualBetDatum   = MutualBetDatum bets'  GameFinished
        mutualBetToken   = assetClassValue (mbpMutualBetId mbParams) 1
        lookups  = Constraints.typedValidatorLookups inst
                   <> Constraints.otherScript mrScript
                   <> Constraints.unspentOutputs (Map.singleton oref o)
        tx       = Constraints.mustPayToTheScript mutualBetDatum (mutualBetToken <> (Ada.toValue Ledger.minAdaTxOut))
                   <> Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData $ Payout signedMessage)
                   <> payConstraints
    mkTxConstraints lookups tx >>= submitTxConfirmed . adjustUnbalancedTx
    logInfo @String $ "payout completed" ++ show bets'

cancel ::
    forall w s. MutualBetParams
    -> SignedMessage OracleSignedMessage
    -> Contract w s Text ()
cancel mbParams signedMessage = do
    (oref, o, MutualBetDatum bets _) <- findMutualBetInstance mbParams

    let payBackConstraints = mkTxReturnBets bets

    let inst = typedMutualBetValidator mbParams
        mrScript = mutualBetValidator mbParams
        mutualBetDatum   = MutualBetDatum bets GameCancelled
        mutualBetToken   = assetClassValue (mbpMutualBetId mbParams) 1
        lookups  = Constraints.typedValidatorLookups inst
                   <> Constraints.otherScript mrScript
                   <> Constraints.unspentOutputs (Map.singleton oref o)
        tx       = Constraints.mustPayToTheScript mutualBetDatum (mutualBetToken <> (Ada.toValue Ledger.minAdaTxOut))
                   <> Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData $ CancelGame signedMessage)
                   <> payBackConstraints
    mkTxConstraints lookups tx >>= submitTxConfirmed . adjustUnbalancedTx
    logInfo $ "cancel game: " ++ show bets

deleteGame ::
    forall w s. MutualBetParams
    -> SignedMessage OracleSignedMessage
    -> Contract w s Text ()
deleteGame mbParams signedMessage = do
    (oref, o, MutualBetDatum bets _) <- findMutualBetInstance mbParams

    let inst = typedMutualBetValidator mbParams
        mrScript = mutualBetValidator mbParams
        lookups  = Constraints.typedValidatorLookups inst
                   <> Constraints.otherScript mrScript
                   <> Constraints.unspentOutputs (Map.singleton oref o)
        tx       = Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData $ DeleteGame signedMessage)

    mkTxConstraints lookups tx >>= submitTxConfirmed . adjustUnbalancedTx
    logInfo $ "delete game: " ++ show bets

mkTxPayWinners :: [(PubKeyHash, Ada, Ada)]-> TxConstraints MutualBetRedeemer MutualBetDatum
mkTxPayWinners = foldMap (\(winnerAddressHash, winnerBetAmount, winnerPrize) -> Constraints.mustPayToPubKey winnerAddressHash $ Ada.toValue $ winnerBetAmount + winnerPrize)

mkTxReturnBets :: [Bet] -> TxConstraints MutualBetRedeemer MutualBetDatum
mkTxReturnBets = foldMap (\betVal -> Constraints.mustPayToPubKey (betBettor betVal) $ Ada.toValue (betAmount betVal))

-- | Make a bet
bet ::
    forall w s. MutualBetParams
    -> BetParams
    -> Contract w s Text [Bet]
bet mbParams betParams = do
    (oref, o, MutualBetDatum bets _) <- findMutualBetInstance mbParams
    ownPK <- ownPubKeyHash

    let newBet = Bet {
            betAmount =  Ada.lovelaceOf $ nbpAmount betParams,
            betBettor = ownPK,
            betTeamId = nbpWinnerId betParams,
            betWinShare = Ada.lovelaceOf 0
            }
    logInfo $ "bets: " ++ show bets

    let inst = typedMutualBetValidator mbParams
        mrScript = mutualBetValidator mbParams
        bets' = newBet : bets
        betsAmount = betsValueAmount bets'
        mutualBetDatum   = MutualBetDatum bets' BettingOpen
        mutualBetToken    = assetClassValue (mbpMutualBetId mbParams) 1
        lookups  = Constraints.typedValidatorLookups inst
                   <> Constraints.otherScript mrScript
                   <> Constraints.unspentOutputs (Map.singleton oref o)
        tx       = Constraints.mustPayToTheScript mutualBetDatum (mutualBetToken <> Ada.toValue(betsAmount) <> (Ada.toValue Ledger.minAdaTxOut))
                   <> Constraints.mustPayToPubKey (mbpOwner mbParams) (Ada.toValue (mbpBetFee mbParams))
                   <> Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData $ MakeBet newBet)
    mkTxConstraints lookups tx >>= submitTxConfirmed . adjustUnbalancedTx
    logInfo $ "bet placed: " ++ show bets'
    return bets'

-- | Make a bet
cancelBet ::
    forall w s. MutualBetParams
    -> BetParams
    -> Contract w s Text [Bet]
cancelBet mbParams betParams = do
    (oref, o, MutualBetDatum bets _) <- findMutualBetInstance mbParams
    ownPK <- ownPubKeyHash

    let betToCancel = Bet {
            betAmount =  Ada.lovelaceOf $ nbpAmount betParams,
            betBettor = ownPK,
            betTeamId = nbpWinnerId betParams,
            betWinShare = Ada.lovelaceOf 0
            }
    when (not (betToCancel `elem` bets)) $ throwError "bet not exists"
    logInfo $ "bet canceled: " ++ show betToCancel
    let inst = typedMutualBetValidator mbParams
        mrScript = mutualBetValidator mbParams
        bets' = (deleteFirstOccurence betToCancel bets)
        betsAmount = betsValueAmount  bets'
        mutualBetDatum   = MutualBetDatum bets' BettingOpen
        mutualBetToken    = assetClassValue (mbpMutualBetId mbParams) 1
        lookups  = Constraints.typedValidatorLookups inst
                   <> Constraints.otherScript mrScript
                   <> Constraints.unspentOutputs (Map.singleton oref o)
        tx       = Constraints.mustPayToTheScript mutualBetDatum (mutualBetToken <> Ada.toValue(betsAmount) <> (Ada.toValue Ledger.minAdaTxOut))
                   <> Constraints.mustPayToPubKey (betBettor betToCancel) (Ada.toValue (betAmount betToCancel))
                   <> Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData $ CancelBet betToCancel)
    mkTxConstraints lookups tx >>= submitTxConfirmed . adjustUnbalancedTx
    logInfo $ "bet canceled: " ++ show betToCancel
    return bets'

getMutualBetDatum :: ChainIndexTxOut -> Contract w s Text MutualBetDatum
getMutualBetDatum o =
  case o of
      PublicKeyChainIndexTxOut {} ->
        throwError "no datum for a txout of a public key address"
      ScriptChainIndexTxOut { _ciTxOutDatum } -> do
        (Datum e) <- either getDatum pure _ciTxOutDatum
        maybe (throwError "datum hash wrong type")
              pure
              (PlutusTx.fromBuiltinData e)
  where
    getDatum :: DatumHash -> Contract w s Text Datum
    getDatum dh =
      datumFromHash dh >>= \case Nothing -> throwError "datum not found"
                                 Just d  -> pure d

findMutualBetInstance ::
    forall w s. MutualBetParams
    -> Contract w s Text (TxOutRef, ChainIndexTxOut, MutualBetDatum)
findMutualBetInstance mutualBet = do
    let addr = mutualBetAddress mutualBet
        mutualBetToken = mbpMutualBetId mutualBet
    utxos <- utxosAt addr
    go  [x | x@(_, o) <- Map.toList utxos, assetClassValueOf (view ciTxOutValue o) mutualBetToken == 1]
  where
    go [(oref, o)] = do
        logInfo @String $ printf "getDatum"
        d <- getMutualBetDatum o
        return (oref, o, d)
    go [] = do
        logInfo @String $ printf "Mutual bet instance not found1"
        throwError "Mutual bet instance not found"
    go _ = do
        logInfo @String $ printf "Mutual bet instance not found"
        throwError "Mutual bet instance not found"

forgeIdToken::
    forall w s. TokenName
    -> PubKeyHash
    -> Contract w s Text CurrencySymbol
forgeIdToken tokenName pk = fmap Currency.currencySymbol $
    mapError (pack . show @Currency.CurrencyError) $
    Currency.mintContract pk [(tokenName, 1)]

mutualBetStart ::
    MutualBetStartParams
    -> Contract (Last (Either Text MutualBetParams)) EmptySchema Text ()
mutualBetStart params = do
    ownPK <- ownPubKeyHash
    cs <- forgeIdToken mutualBetTokenName ownPK
    let mutualBetAsset = assetClass cs mutualBetTokenName
    mutualBetStart' mutualBetAsset params

mutualBetStart' ::
    AssetClass
    -> MutualBetStartParams
    -> Contract (Last (Either Text MutualBetParams)) EmptySchema Text ()
mutualBetStart' mutualBetAsset params = do
    e <- mapError absurd $ runError $ start mutualBetAsset params
    tell $ Last $ Just e

mutualBetStartWithOracle ::
    MutualBetStartParams
    -> Contract (Last (Either Text MutualBetParams)) EmptySchema Text ()
mutualBetStartWithOracle params = do
    ownPK <- ownPubKeyHash
    cs <- forgeIdToken mutualBetTokenName ownPK
    let mutualBetAsset = assetClass cs mutualBetTokenName
    mutualBetStartWithOracle' mutualBetAsset params

mutualBetStartWithOracle' ::
    AssetClass
    -> MutualBetStartParams
    -> Contract (Last (Either Text MutualBetParams)) EmptySchema Text ()
mutualBetStartWithOracle' mutualBetAsset params = do
    void $ startWithOracle mutualBetAsset params

-- | Schema for the endpoints for users of NFTMarket.
type BettorSchema =
         Endpoint "bet" BetParams
        .\/ Endpoint "cancelBet" BetParams

data BettorEvent =
      MakeBetEvent BetParams
      | CancelBetEvent BetParams
      | GameChanged [Bet] GameStatus
    deriving (Show)

awaitMutualBetChanged:: MutualBetParams -> Promise MutualBetOutput BettorSchema Text BettorEvent
awaitMutualBetChanged params = do
    let addr = mutualBetAddress params
    promiseBind (utxoIsProduced addr) $ \_ -> do
                        logInfo @String $ printf "awaitMutualBetChanged"
                        (_, _, MutualBetDatum bets gameStatus) <- findMutualBetInstance params
                        pure $ GameChanged bets gameStatus

waitForChange :: MutualBetParams -> [Bet] -> Contract MutualBetOutput BettorSchema Text BettorEvent
waitForChange params _ = do
    let
        makeBetPromise = endpoint @"bet" $ \betParams -> do
                                pure $ MakeBetEvent betParams
        cancelBetPromise = endpoint @"cancelBet" $ \ betParams -> do
                                pure $ CancelBetEvent betParams

    selectList [makeBetPromise, cancelBetPromise, awaitMutualBetChanged params]

handleEvent :: MutualBetParams -> [Bet] -> BettorEvent -> Contract MutualBetOutput BettorSchema Text (Either [Bet] ())
handleEvent params bets change =
    let continue = pure . Left
        stop     = pure (Right ())

    in case change of
        MakeBetEvent betParams -> do
            e <- runError $ bet params betParams
            case e of
                Left err -> do
                    logInfo ("Bet error" ++ Haskell.show err)
                    continue bets
                Right bets'  -> do
                    tell (mutualBetStateOut $ BetState bets')
                    continue bets
        CancelBetEvent betParams -> do
            e <- runError $ cancelBet params betParams
            case e of
                Left err -> do
                    logInfo ("Cancel bet error" ++ Haskell.show err)
                    continue bets
                Right bets'  -> do
                    tell (mutualBetStateOut $ BetState bets')
                    continue bets
        GameChanged bets' gameStatus -> case gameStatus of
            -- other bets places
            BettingOpen -> do
                logInfo ("BettingOpen update bets " ++ Haskell.show bets')
                tell (mutualBetStateOut $ BetState bets)
                continue bets'
            BettingClosed -> do
                logInfo ("BettingClosed" ++ Haskell.show bets')
                tell (mutualBetStateOut $ BettingClosedState bets')
                continue bets
            GameCancelled -> do
                logInfo ("GameCancelled " ++ Haskell.show bets')
                tell (mutualBetStateOut $ CancelGameState bets')
                stop
            GameFinished -> do
                logInfo ("GameFinished " ++ Haskell.show bets')
                tell (mutualBetStateOut $ Finished bets')
                stop

mutualBetBettor::
    MutualBetParams
    -> Contract MutualBetOutput BettorSchema Text ()
mutualBetBettor params = do
    let loop  = loopM (\bets -> waitForChange params bets >>= handleEvent params bets)
    loop []

