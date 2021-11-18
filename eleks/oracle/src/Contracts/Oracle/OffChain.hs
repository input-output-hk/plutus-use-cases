{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE DerivingStrategies    #-}

module Contracts.Oracle.OffChain
    ( OracleSchema
    , OracleParams (..)
    , runOracle
    , oracleRequestTokenName
    , requestOracleForAddress
    , findOracleRequest
    , awaitNextOracleRequest
    , getActiveOracleRequests 
    , getActiveGames
    , oracleScriptAsShortBs
    , oraclePlutusScript
    , UseOracleSchema
    , UseOracleParams (..)
    , UpdateOracleParams (..)
    , OracleContractState (..)
    , useOracle
    , startOracle1
    ) where

import           Cardano.Api.Shelley       (PlutusScript (..), PlutusScriptV1) 
import           Control.Lens              (view)
import           Control.Monad             hiding (fmap)
import           Contracts.Oracle.Types
import           Contracts.Oracle.RequestToken
import           Contracts.Oracle.OnChain
import           Codec.Serialise
import           Data.Aeson                (FromJSON, ToJSON)
import qualified Data.ByteString.Short     as SBS
import qualified Data.ByteString.Lazy      as LBS
import qualified Data.Map                  as Map
import           Data.Maybe                (catMaybes)
import           Data.Monoid               (Last (..))
import           Data.Text                 (Text, pack)
import qualified Data.List.NonEmpty        as NonEmpty
import           GHC.Generics              (Generic)
import           Plutus.Contract           as Contract
import qualified PlutusTx
import           PlutusTx.Prelude          hiding (Semigroup(..), unless)
import           Ledger                    hiding (singleton, MintingPolicyHash)
import qualified Ledger.Scripts            as LedgerScripts
import qualified Ledger.Tx                 as LedgerScripts
import           Ledger.Constraints        as Constraints
import qualified Ledger.Contexts           as Validation
import           Ledger.Oracle             (Observation, SignedMessage(..), signMessage, SignedMessageCheckError(..), checkSignature)
import qualified Ledger.Typed.Scripts      as Scripts
import           Ledger.Value              as Value
import           Ledger.Ada                as Ada
import           Types.Game
import           Plutus.Contracts.Currency as Currency
import           Plutus.Contract.Types     (Promise (..))
import           Prelude                   (Semigroup (..), Show (..), String)
import qualified Prelude                   as Haskell
import           Schema                    (ToSchema)
import           Plutus.ChainIndex         ()
startOracle :: forall w s. OracleParams -> Contract w s Text Oracle
startOracle op = do
    let pk = opPublicKey op
    pkh <- Contract.ownPubKeyHash
    let oracleRequestTokenInfo = OracleRequestToken
            { ortOperator = pkh
            , ortFee = opFees op
            , ortCollateral = opCollateral op
            }
    let oracle = Oracle
            { --oSymbol = opSymbol op
              oRequestTokenSymbol = requestTokenSymbol oracleRequestTokenInfo
            , oOperator = pkh
            , oOperatorKey = pk
            , oFee = opFees op
            , oCollateral = opCollateral op
            }
    logInfo @String $ "started oracle " ++ show oracle
    return oracle

startOracle1 :: OracleParams1 -> Contract () Empty Text ()
startOracle1 d = do
    --let pk = opPublicKey1 op
    pkh <- Contract.ownPubKeyHash
    -- let oracleRequestTokenInfo = OracleRequestToken
    --         { ortOperator = pkh
    --         , ortFee = opFees1 op
    --         , ortCollateral = opCollateral1 op
    --         }
    -- let oracle = Oracle
    --         { --oSymbol = opSymbol op
    --           oRequestTokenSymbol = requestTokenSymbol oracleRequestTokenInfo
    --         , oOperator = pkh
    --         , oOperatorKey = pk
    --         , oFee = opFees1 op
    --         , oCollateral = opCollateral1 op
    --         }
    void $ logInfo @String $ "started oracle "-- ++ show oracle
    -- return oracle

updateOracle :: forall w s. Oracle -> PrivateKey -> UpdateOracleParams -> Contract w s Text ()
updateOracle oracle operatorPrivateKey params = do
    let gameId = uoGameId params
        winnerId = uoWinnerId params
        gameStatus = uoGameStatus params

    activeRequests <- getActiveOracleRequests oracle
    let requests = filter (isGameOracleRequest gameId) activeRequests 
    forM_ requests $ \(oref, o, oracleData) -> do
                        let oracleSignMessage = OracleSignedMessage 
                                    { osmWinnerId = winnerId
                                    , osmGameId = gameId
                                    , osmGameStatus = gameStatus
                                    }
                        let oracleData' = oracleData{ ovSignedMessage = Just $ signMessage oracleSignMessage operatorPrivateKey }
                        when (oracleData' /= oracleData) $ do
                            let requestTokenVal = assetClassValue (requestTokenClassFromOracle oracle) 1
                                collateralVal = Ada.toValue $ oCollateral oracle 
                            let lookups = Constraints.unspentOutputs (Map.singleton oref o)     
                                        <> Constraints.typedValidatorLookups (typedOracleValidator oracle) 
                                        <> Constraints.otherScript (oracleValidator oracle)
                                tx      = Constraints.mustPayToTheScript oracleData' (requestTokenVal <> collateralVal) 
                                        <> Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData Update)

                            logInfo ("submit transaction " ++ (show $ oracleData'))
                            mkTxConstraints lookups tx >>= submitTxConfirmed . adjustUnbalancedTx

oracleValueFromTxOutTx :: ChainIndexTxOut -> Maybe OracleData
oracleValueFromTxOutTx o = do
    Datum d <- either (const Nothing) Just (_ciTxOutDatum o)
    PlutusTx.fromBuiltinData d

data UpdateOracleParams = UpdateOracleParams
    { uoGameId      :: !GameId              -- ^ Game
    , uoWinnerId    :: !TeamId              -- ^ Winner team id
    , uoGameStatus  :: !FixtureStatusShort  -- ^ Game state
    } deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

data OracleContractState =
      OracleState Oracle
    | Games [GameId]
    | Updated GameId
    deriving (Show, Generic, FromJSON, ToJSON)

type OracleSchema = Endpoint "update" UpdateOracleParams   
                    .\/ Endpoint "games" ()     

requestOracleForAddress :: forall w s. Oracle -> GameId -> Contract w s Text ()
requestOracleForAddress oracle gameId = do 
    pkh <- Contract.ownPubKeyHash
    let inst = typedOracleValidator oracle
        mrScript = oracleValidator oracle
        tokenMintingPolicy = requestTokenPolicy $ oracleToRequestToken oracle
        forgedToken = requestTokenValue oracle
        oracleFee = oFee oracle 
        feeVal = Ada.toValue oracleFee
        collateralVal = Ada.toValue $ oCollateral oracle
        oracleData = OracleData 
            { ovRequestAddress = pkh
            , ovGame = gameId
            , ovSignedMessage = Nothing
            }
        mintRedeemer = Redeemer $ PlutusTx.toBuiltinData $ Request
    let lookups = Constraints.typedValidatorLookups inst 
                <> Constraints.otherScript mrScript
                <> Constraints.mintingPolicy tokenMintingPolicy

        tx      = Constraints.mustPayToTheScript oracleData (forgedToken <> collateralVal)
                <> Constraints.mustPayToPubKey (oOperator oracle) (feeVal)
                <> Constraints.mustMintValueWithRedeemer mintRedeemer forgedToken

    mkTxConstraints lookups tx >>= submitTxConfirmed . adjustUnbalancedTx

--get active request lists for oracle to process
getActiveOracleRequests:: Oracle -> Contract w s Text [(TxOutRef, ChainIndexTxOut, OracleData)]
getActiveOracleRequests oracle = do
    xs <- utxosAt (oracleAddress oracle)
    let requests = filter (isActiveRequest oracle) . filterOracleRequest oracle . Map.toList $ xs
    return requests 

getActiveGames:: Oracle -> Contract w s Text ([GameId])
getActiveGames oracle = do
    requests <- nub . map (\(_, _, or) -> ovGame or) <$> getActiveOracleRequests oracle
    return requests

awaitNextOracleRequest:: Oracle -> Contract w s Text [(TxOutRef, ChainIndexTxOut, OracleData)]
awaitNextOracleRequest oracle =
    awaitNext
    where
    convertChainIndexOut:: (Maybe ChainIndexTxOut, TxOutRef) -> Maybe (TxOutRef, ChainIndexTxOut)
    convertChainIndexOut (o, oref) =  (\to -> (oref, to)) <$> o
    awaitNext :: Contract w s Text [(TxOutRef, ChainIndexTxOut, OracleData)]
    awaitNext = do
        utxos <- awaitUtxoProduced $ oracleAddress oracle
        let txs = map (\tx -> map convertChainIndexOut $ chainIndexTxOutsWithRef tx) (NonEmpty.toList $ utxos)
        let filterValidTx = catMaybes . concat
        let filtered = filterOracleRequest oracle . filterValidTx $ txs
        return filtered
            
runOracle :: OracleParams -> Contract (Last OracleContractState) OracleSchema Text ()
runOracle op = do
    oracle <- startOracle op
    tell $ Last $ Just $ OracleState oracle
    forever $ selectList[(update oracle), (games oracle)]
  where
    update :: Oracle -> Promise (Last OracleContractState) OracleSchema Text ()
    update oracle = endpoint @"update" $ \updateOracleParams -> do
        logInfo @String "update called"
        logInfo $ show updateOracleParams
        updateOracle oracle (opSigner op) updateOracleParams
        tell $ Last $ Just $ Updated $ uoGameId updateOracleParams
    games :: Oracle -> Promise (Last OracleContractState) OracleSchema Text ()
    games oracle = endpoint @"games" $ \_ -> do
        gamesIds <- getActiveGames oracle
        tell $ Last $ Just $ Games gamesIds

hasOracleRequestToken :: Oracle -> (TxOutRef, ChainIndexTxOut) -> Bool
hasOracleRequestToken oracle (oref, o) = 
    assetClassValueOf (view ciTxOutValue o) (requestTokenClassFromOracle oracle) == 1

hasOracelRequestDatum :: (TxOutRef, ChainIndexTxOut) -> Bool
hasOracelRequestDatum (oref, o) = isJust . oracleValueFromTxOutTx $ o

filterOracleRequest :: Oracle -> [(TxOutRef, ChainIndexTxOut)] -> [(TxOutRef, ChainIndexTxOut, OracleData)]
filterOracleRequest oracle txs = catMaybes . map mapDatum . filter (hasOracleRequestToken oracle) $ txs

mapDatum :: (TxOutRef, ChainIndexTxOut) -> Maybe (TxOutRef, ChainIndexTxOut, OracleData)
mapDatum (oref, o) = case oracleValueFromTxOutTx o of
    Just datum -> Just (oref, o, datum)
    Nothing -> Nothing

isGameOracleRequest :: GameId -> (TxOutRef, ChainIndexTxOut, OracleData) -> Bool
isGameOracleRequest gameId (_, _, od) = gameId == (ovGame od) 

isOwnerOracleRequest :: PubKeyHash -> (TxOutRef, ChainIndexTxOut, OracleData) -> Bool
isOwnerOracleRequest owner (_, _, od) = owner == (ovRequestAddress od)

isActiveSignedMessage :: OracleSignedMessage -> Bool
isActiveSignedMessage message = osmGameStatus message /= FT

isActiveRequest:: Oracle -> (TxOutRef, ChainIndexTxOut, OracleData) -> Bool
isActiveRequest oracle (_, _, od) = case ovSignedMessage od of
                                -- not processed
                                Nothing -> True
                                Just message -> case verifyOracleValueSigned (oOperatorKey oracle) message of
                                    --oracle cannot parse signed message
                                    Nothing -> False
                                    Just (oracleMessage, _) -> isActiveSignedMessage oracleMessage

findOracleRequest :: 
    forall w s. Oracle
    -> GameId
    -> PubKeyHash
    -> Contract w s Text (Maybe (TxOutRef, ChainIndexTxOut, OracleData))
findOracleRequest oracle gameId owner = do
    xs <- utxosAt (oracleAddress oracle)
    let findCriteria = find (\tx -> isOwnerOracleRequest owner tx && isGameOracleRequest gameId tx)
    let request = findCriteria . filterOracleRequest oracle . Map.toList $ xs
    pure request 

data UseOracleParams = UseOracleParams
    { uoGame           :: Integer -- use owned oracle request
    }
    deriving stock (Haskell.Eq, Haskell.Show, Generic)
    deriving anyclass (ToJSON, FromJSON, ToSchema)
    
type UseOracleSchema = Endpoint "use" UseOracleParams

-- example endpoint how to consume oracle request owned by user
useOracle :: Oracle -> Contract Text UseOracleSchema Text ()
useOracle oracle =
    selectList[(use oracle)]
  where
    use :: Oracle -> Promise Text UseOracleSchema Text ()
    use oracle = endpoint @"use" $ \oracleParams -> do
        pkh <- Contract.ownPubKeyHash
        oracleRequestMaybe <- findOracleRequest oracle (uoGame oracleParams) pkh
        case oracleRequestMaybe of
            Nothing -> throwError "no oracle request"
            Just (oref, o, t) -> do
                let inst = typedOracleValidator oracle
                    mrScript = oracleValidator oracle
                    tokenMintingPolicy = requestTokenPolicy $ oracleToRequestToken oracle
                    redeemer = Redeemer $ PlutusTx.toBuiltinData $ OracleRedeem
                    requestTokenVal = requestTokenValue oracle
                    collateralValue = Ada.toValue $ oCollateral oracle
                    mintRedeemer = Redeemer $ PlutusTx.toBuiltinData $ RedeemToken

                let lookups = Constraints.typedValidatorLookups inst 
                            <> Constraints.otherScript mrScript
                            <> Constraints.unspentOutputs (Map.singleton oref o)
                            <> Constraints.mintingPolicy tokenMintingPolicy
                    tx  = Constraints.mustSpendScriptOutput oref redeemer
                             <> Constraints.mustMintValueWithRedeemer mintRedeemer (inv requestTokenVal)
                             <> Constraints.mustPayToPubKey pkh collateralValue

                mkTxConstraints lookups tx >>= submitTxConfirmed . adjustUnbalancedTx