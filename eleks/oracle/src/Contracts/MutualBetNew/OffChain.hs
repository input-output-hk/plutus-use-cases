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
import           Control.Lens                     (view)
import           Control.Monad                    hiding (fmap)
import qualified Data.ByteString.Char8            as B
import           Data.List                        (sortOn)
import qualified Data.Map                         as Map
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

startGame :: forall w s. MutualBetParams 
    -> Contract w s Text ()
startGame mbParams = do 
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
                   <> Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData $ StartGame)          
    mkTxConstraints lookups tx >>= submitTxConfirmed . adjustUnbalancedTx
    logInfo @String $ "game started"

payout :: 
    forall w s. MutualBetParams 
    -> TeamId
    -> Contract w s Text ()
payout mbParams winnerId = do
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
        tx       = Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData $ Payout winnerId)   
                   <> payConstraints 
    mkTxConstraints lookups tx >>= submitTxConfirmed . adjustUnbalancedTx
    logInfo @String $ "payout completed"

cancel :: 
    forall w s. MutualBetParams 
    -> Contract w s Text ()
cancel mbParams = do
    (oref, o, MutualBetDatum bets isActiveBetting) <- findMutualBetInstance mbParams
    ownPK <- ownPubKeyHash

    let payBackConstraints = mkTxReturnBets bets

    let inst = typedMutualBetValidator mbParams
        mrScript = mutualBetValidator mbParams
        lookups  = Constraints.typedValidatorLookups inst 
                   <> Constraints.otherScript mrScript
                   <> Constraints.unspentOutputs (Map.singleton oref o)
        tx       = Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData $ CancelGame) 
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
    logInfo $ "bet canceled1: " ++ show cancelBet
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
    -> Contract (Last (Either Text MutualBetParams)) BettorOwnerSchema Text ()
mutualBetStart params = do
    ownPK <- ownPubKeyHash
    cs <- forgeIdToken mutualBetTokenName ownPK
    let mutualBetAsset = assetClass cs mutualBetTokenName
    mutualBetStart' mutualBetAsset params
 
mutualBetStart' :: 
    AssetClass 
    -> MutualBetStartParams
    -> Contract (Last (Either Text MutualBetParams)) BettorOwnerSchema Text ()
mutualBetStart' mutualBetAsset params = do
    start mutualBetAsset params
    e <- mapError absurd $ runError $ start mutualBetAsset params
    tell $ Last $ Just e
type BettorOwnerSchema =
        Endpoint "start" MutualBetStartParams

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
            

