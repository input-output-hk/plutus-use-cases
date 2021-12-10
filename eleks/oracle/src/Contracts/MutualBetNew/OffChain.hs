{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE NumericUnderscores         #-}

-- | Implements a custom currency with a monetary policy that allows
--   the forging of a fixed amount of units.
module Contracts.MutualBetNew.OffChain
   where

import           Contracts.MutualBetNew.Types
import           Contracts.MutualBetNew.OnChain   
import           Contracts.Oracle
import           Control.Lens                     (view)
import           Control.Monad                    hiding (fmap)
import qualified Data.ByteString.Char8            as B
import           Data.List                        (sortOn)
import qualified Data.Map                         as Map
import           Data.Maybe                       (catMaybes)
import           Data.Monoid                      (Last (..))
import           Data.String                      (fromString)
import           Data.Ord                         (comparing)
import           Data.Proxy                       (Proxy (..))
import           Data.Text                        (Text, pack)
import qualified Data.Text                        as T
import qualified Data.Text.Encoding               as T
import           Data.Void                        (Void, absurd)
import           Ledger                           hiding (singleton)
import qualified Ledger.Ada                       as Ada
import qualified Ledger.Constraints               as Constraints
import           Ledger.Constraints               (adjustUnbalancedTx)
import           Ledger.Constraints.OnChain       as Constraints
import           Ledger.Constraints.TxConstraints as Constraints
import           Ledger.Scripts                   (unitRedeemer)
import           Ledger.Typed.Scripts             (TypedValidator)
import qualified Ledger.Typed.Scripts             as Scripts
import           Ledger.Value                     (AssetClass (..), assetClass, assetClassValue, assetClassValueOf, valueOf,
                                                    symbols, unCurrencySymbol, unTokenName, CurrencySymbol (..), toString)
import qualified Ledger.Value                     as Value
import qualified Ledger.Contexts                  as Validation
import           Playground.Contract
import           Plutus.Contract                  hiding (when)
import           Plutus.Contract.Oracle           (SignedMessage(..), verifySignedMessageOffChain)
-- import qualified Contracts.NFT.Currency        as Currency
import qualified PlutusTx                   
import           PlutusTx.Prelude                 hiding (Semigroup (..), unless)
import           Prelude                          (Semigroup (..), String, Char, read, show)
import qualified Prelude                          as Haskell
import           Text.Printf                      (printf)
import           Contracts.MutualBetNew.Currency   as Currency
import           Types.Game  
import           Contracts.Oracle 

mutualBetTokenName :: TokenName
mutualBetTokenName =  "MutualBet"

data BetParams = 
    BetParams
        { nbpAmount  :: Integer -- Bet lovelace amount 
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
        tx   = mustPayToTheScript (MutualBetDatum [] True) (assetClassValue mutualBetTokenClass 1 <> (Ada.toValue Ledger.minAdaTxOut))
               -- todo: add oracle integration ,. pay to oracle
               <> Constraints.mustPayToPubKey (oOperator oracle) (Ada.toValue (oFee oracle))
    mkTxConstraints (Constraints.typedValidatorLookups inst) tx
      >>= submitTxConfirmed . adjustUnbalancedTx

    logInfo @String $ printf "started Mutual bet %s at address %s" (show mutualBet) (show $ mutualBetAddress mutualBet)
    return mutualBet

startWithOracle ::
    forall w s.
    AssetClass
    -> MutualBetStartParams
    -> Contract w s Text ()
startWithOracle mutualBetTokenClass startParams = do
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
        oracle = mbspOracle startParams
        inst = typedMutualBetValidator params
        tx   = mustPayToTheScript (MutualBetDatum [] True) (assetClassValue mutualBetTokenClass 1 <> (Ada.toValue Ledger.minAdaTxOut))
            --    -- todo: add oracle integration ,. pay to oracle
            --    <> Constraints.mustPayToPubKey (oOperator oracle) (Ada.toValue (oFee oracle))
    mkTxConstraints (Constraints.typedValidatorLookups inst) tx
      >>= submitTxConfirmed . adjustUnbalancedTx
    logInfo @String $ printf "started Mutual bet %s at address %s" (show params) (show $ mutualBetAddress params)
    --tell $ Last $ Just params

    logInfo ("Request oracle for game " ++ (Haskell.show $ mbpGame params))
    _ <- requestOracleForAddress (mbpOracle params) (mbpGame params)
    waitGameStateChange params
    where 
        waitGameStateChange:: MutualBetParams -> Contract w s Text ()
        waitGameStateChange params = do 
            gameState <- waitForGameStateChange params
            let signedMessage = gmsSignedMessage gameState
            let message = gmsSignedMessageData gameState
            let gameId = (osmGameId message)
            let oracle = (mbpOracle params)
            case osmGameStatus message of
                NS -> waitGameStateChange params
                FT -> do
                    logInfo @Haskell.String "Payout"
                    payout params signedMessage (osmWinnerId message)
                    void $ redeemOracleRequest oracle gameId
            
                LIVE -> do
                    logInfo @Haskell.String "Make bet over"
                    _ <- startGame params signedMessage
                    waitGameStateChange params
                CANC -> do
                    logInfo @Haskell.String "Cancel game"
                    cancel params signedMessage
                    void $ redeemOracleRequest oracle gameId
                 

    
isCurrentGame :: Ledger.PubKeyHash -> MutualBetParams -> OracleData -> Either Haskell.String OracleData
isCurrentGame pkh params oracleData
    | pkh /= (ovRequestAddress oracleData) = Left "Not signed by owner wallet"
    | (mbpGame params) /= (ovGame oracleData) = Left "Not current game"
    | otherwise = Right oracleData

mapSignedMessage :: MutualBetParams -> (TxOutRef, ChainIndexTxOut, OracleData) -> Maybe GameStateChange
mapSignedMessage params (oref, o, od) = case ovSignedMessage od of
    Just signed -> case verifySignedMessageOffChain (oOperatorKey $ mbpOracle params) signed of
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
    -> Contract w s Text GameStateChange
waitForGameStateChange params = do
        waitEnd
    where
        isCurrentGameState pkh params GameStateChange{gmsOracleData} = isRight $ isCurrentGame pkh params gmsOracleData
        waitEnd = do  
            txs <- awaitNextOracleRequest (mbpOracle params)
            pkh <- ownPubKeyHash
            logInfo @Haskell.String "Await next"
            let currentGameSignedTx = find (isCurrentGameState pkh params) . catMaybes . map (mapSignedMessage params) $ txs 
            case currentGameSignedTx of
                Nothing -> do { logInfo @Haskell.String "Not current game state change"; waitEnd; }
                Just d  -> do { logInfo ("State changes " ++ Haskell.show d); return d; }

startGame :: forall w s. MutualBetParams 
    -> SignedMessage OracleSignedMessage
    -> Contract w s Text ()
startGame mbParams signedMessage = do 
    (oref, o, MutualBetDatum bets isActiveBetting) <- findMutualBetInstance mbParams
    ownPK <- ownPubKeyHash

    when (not isActiveBetting) $ throwError "game is already started"   

    let inst = typedMutualBetValidator mbParams
        mrScript = mutualBetValidator mbParams
        betsAmount = betsValueAmount bets
        mutualBetDatum   = MutualBetDatum bets False
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
    (oref, o, MutualBetDatum bets isActiveBetting) <- findMutualBetInstance mbParams
    ownPK <- ownPubKeyHash

    let winners = getWinners winnerId bets
        payConstraints = if null winners
            then mkTxReturnBets bets
            else mkTxPayWinners winners 

    logInfo $ "winners: " ++ show winners
    let inst = typedMutualBetValidator mbParams
        mrScript = mutualBetValidator mbParams
        lookups  = Constraints.typedValidatorLookups inst 
                   <> Constraints.otherScript mrScript
                   <> Constraints.unspentOutputs (Map.singleton oref o)
        tx       = Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData $ Payout signedMessage)   
                   <> payConstraints 
    mkTxConstraints lookups tx >>= submitTxConfirmed . adjustUnbalancedTx
    logInfo @String $ "payout completed"

cancel :: 
    forall w s. MutualBetParams 
    -> SignedMessage OracleSignedMessage
    -> Contract w s Text ()
cancel mbParams signedMessage = do
    (oref, o, MutualBetDatum bets isActiveBetting) <- findMutualBetInstance mbParams
    ownPK <- ownPubKeyHash

    let payBackConstraints = mkTxReturnBets bets

    let inst = typedMutualBetValidator mbParams
        mrScript = mutualBetValidator mbParams
        lookups  = Constraints.typedValidatorLookups inst 
                   <> Constraints.otherScript mrScript
                   <> Constraints.unspentOutputs (Map.singleton oref o)
        tx       = Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData $ CancelGame signedMessage) 
                   <> payBackConstraints       
    mkTxConstraints lookups tx >>= submitTxConfirmed . adjustUnbalancedTx
    logInfo $ "cancel game: " ++ show bets

mkTxPayWinners :: [(PubKeyHash, Ada, Ada)]-> TxConstraints MutualBetRedeemer MutualBetDatum 
mkTxPayWinners = foldMap (\(winnerAddressHash, winnerBetAmount, winnerPrize) -> Constraints.mustPayToPubKey winnerAddressHash $ Ada.toValue $ winnerBetAmount + winnerPrize)

mkTxReturnBets :: [Bet] -> TxConstraints MutualBetRedeemer MutualBetDatum
mkTxReturnBets = foldMap (\bet -> Constraints.mustPayToPubKey (betBettor bet) $ Ada.toValue (betAmount bet))

-- | Make a bet 
bet :: 
    forall w s. MutualBetParams 
    -> BetParams 
    -> Contract w s Text [Bet]
bet mbParams betParams = do
    (oref, o, MutualBetDatum bets isActiveBetting) <- findMutualBetInstance mbParams
    ownPK <- ownPubKeyHash

    when (not isActiveBetting) $ throwError "bettings is closed" 
    let bet = Bet { 
            betAmount =  Ada.lovelaceOf $ nbpAmount betParams, 
            betBettor = ownPK, 
            betTeamId = nbpWinnerId betParams, 
            betWinShare = Ada.lovelaceOf 0
            }
    logInfo $ "bets: " ++ show bets   

    let inst = typedMutualBetValidator mbParams
        mrScript = mutualBetValidator mbParams
        allBets = bet : bets
        betsAmount = betsValueAmount allBets
        mutualBetDatum   = MutualBetDatum allBets True
        mutualBetToken    = assetClassValue (mbpMutualBetId mbParams) 1
        lookups  = Constraints.typedValidatorLookups inst 
                   <> Constraints.otherScript mrScript
                   <> Constraints.unspentOutputs (Map.singleton oref o)
        tx       = Constraints.mustPayToTheScript mutualBetDatum (mutualBetToken <> Ada.toValue(betsAmount) <> (Ada.toValue Ledger.minAdaTxOut))
                   <> Constraints.mustPayToPubKey (mbpOwner mbParams) (Ada.toValue (mbpBetFee mbParams))
                   <> Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData $ MakeBet bet)          
    mkTxConstraints lookups tx >>= submitTxConfirmed . adjustUnbalancedTx
    logInfo $ "bet placed: " ++ show bet
    return allBets

-- | Make a bet 
cancelBet :: 
    forall w s. MutualBetParams 
    -> BetParams 
    -> Contract w s Text [Bet]
cancelBet mbParams betParams = do
    (oref, o, MutualBetDatum bets isActiveBetting) <- findMutualBetInstance mbParams
    ownPK <- ownPubKeyHash

    when (not isActiveBetting) $ throwError "bettings is closed"
    let cancelBet = Bet { 
            betAmount =  Ada.lovelaceOf $ nbpAmount betParams, 
            betBettor = ownPK, 
            betTeamId = nbpWinnerId betParams, 
            betWinShare = Ada.lovelaceOf 0
            }
    when (not (cancelBet `elem` bets)) $ throwError "bet not exists"
    logInfo $ "bet canceled: " ++ show cancelBet
    let inst = typedMutualBetValidator mbParams
        mrScript = mutualBetValidator mbParams
        bets' = (deleteFirstOccurence cancelBet bets)
        betsAmount = betsValueAmount  bets'
        mutualBetDatum   = MutualBetDatum bets' True
        mutualBetToken    = assetClassValue (mbpMutualBetId mbParams) 1
        lookups  = Constraints.typedValidatorLookups inst 
                   <> Constraints.otherScript mrScript
                   <> Constraints.unspentOutputs (Map.singleton oref o)
        tx       = Constraints.mustPayToTheScript mutualBetDatum (mutualBetToken <> Ada.toValue(betsAmount) <> (Ada.toValue Ledger.minAdaTxOut))
                   <> Constraints.mustPayToPubKey (betBettor cancelBet) (Ada.toValue (betAmount cancelBet))
                   <> Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData $ CancelBet cancelBet)           
    mkTxConstraints lookups tx >>= submitTxConfirmed . adjustUnbalancedTx
    logInfo $ "bet canceled: " ++ show cancelBet
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
    go _ = do
        logInfo @String $ printf "Mutual bet instance not found"
        throwError "Mutul bet instance not found"

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
    mutualBetStart' mutualBetAsset params

mutualBetStartWithOracle' :: 
    AssetClass
    -> MutualBetStartParams
    -> Contract (Last (Either Text MutualBetParams)) EmptySchema Text ()
mutualBetStartWithOracle' mutualBetAsset params = do
    void $ startWithOracle mutualBetAsset params
    -- e <- mapError absurd $ runError $ startWithOracle mutualBetAsset params
    -- tell $ Last $ Just ()

-- | Schema for the endpoints for users of NFTMarket.
type BettorSchema =
         Endpoint "bet" BetParams
        .\/ Endpoint "cancelBet" BetParams

data BettorState =
      BetState [Bet]
      | CancelBetState [Bet]
      | Stopped
    deriving (Show, Generic, FromJSON, ToJSON)
-- | Provides the following endpoints for users of a NFT marketplace instance:
--
-- [@bet@]: Creates an nft token.
-- [@cancelBet@]: Cancel token selling.
mutualBetBettor ::
    MutualBetParams 
    -> Promise (Last (Either Text BettorState)) BettorSchema Void ()
mutualBetBettor params =
    (void (f (Proxy @"bet") BetState bet  `select`
      f (Proxy @"cancelBet") BetState cancelBet)    
    <> mutualBetBettor params)
  where
    f :: forall l a p.
         (HasEndpoint l p BettorSchema, FromJSON p)
      => Proxy l
      -> (a -> BettorState)
      -> (MutualBetParams -> p -> Contract (Last (Either Text BettorState)) BettorSchema Text a)
      -> Promise (Last (Either Text BettorState)) BettorSchema Void ()
    f _ g c = handleEndpoint @l $ \p -> do
        e <- either (pure . Left) (runError . c params) p
        tell $ Last $ Just $ case e of
            Left err -> Left err
            Right a  -> Right $ g a
            

