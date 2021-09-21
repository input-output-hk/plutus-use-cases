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
    , oracleTokenName
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
    ) where

import           Cardano.Api.Shelley       (PlutusScript (..), PlutusScriptV1)
import           Types.Game    
import           Control.Monad             hiding (fmap)
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
import           Plutus.Contracts.Currency as Currency
import           Plutus.Contract.Types     (Promise (..))
import           Prelude                   (Semigroup (..), Show (..), String)
import qualified Prelude                   as Haskell
import           Schema                    (ToSchema)
import Contracts.Oracle.Types
import Contracts.Oracle.RequestToken
import Contracts.Oracle.OnChain

data OracleParams = OracleParams
    { opSymbol :: !CurrencySymbol
    , opFees   :: !Integer
    , opSigner :: !PrivateKey
    } deriving (Haskell.Eq, Haskell.Show, Generic, FromJSON, ToJSON)

startOracle :: forall w s. OracleParams -> Contract w s Text Oracle
startOracle op = do
    pk <- Contract.ownPubKey
    pkh <- pubKeyHash <$> Contract.ownPubKey
    let oracleRequestTokenInfo = OracleRequestToken
            { ortOperator = pkh
            , ortFee =opFees op
            }
    let oracle = Oracle
            { --oSymbol = opSymbol op
              oRequestTokenSymbol = requestTokenSymbol oracleRequestTokenInfo
            , oOperator = pkh
            , oOperatorKey = pk
            , oFee = opFees op
            }
    logInfo @String $ "started oracle " ++ show oracle
    return oracle

updateOracle :: forall w s. Oracle -> PrivateKey -> UpdateOracleParams -> Contract w s Text ()
updateOracle oracle operatorPrivateKey params = do
    let gameId = uoGameId params
        winnerId = uoWinnerId params

    activeRequests <- getActiveOracleRequests oracle
    let requests = filter (isGameOracleRequest gameId) activeRequests 
    forM_ requests $ \(oref, o, oracleData) -> do
                        let oracleData' = oracleData{ ovWinner = winnerId, ovWinnerSigned = Just $ signMessage winnerId operatorPrivateKey }
                        let requestTokenVal = assetClassValue (requestTokenClassFromOracle oracle) 1
                        let lookups = Constraints.unspentOutputs (Map.singleton oref o)     
                                    <> Constraints.typedValidatorLookups (typedOracleValidator oracle) 
                                    <> Constraints.otherScript (oracleValidator oracle)
                            tx      = Constraints.mustPayToTheScript oracleData' requestTokenVal 
                                    <> Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData Update)

                        logInfo ("submit transaction " ++ (show $ oracleData'))
                        ledgerTx <- submitTxConstraintsWith lookups tx
                        awaitTxConfirmed $ txId ledgerTx

oracleValueFromTxOutTx :: TxOutTx -> Maybe OracleData
oracleValueFromTxOutTx o = oracleValue (txOutTxOut o) $ \dh -> Map.lookup dh $ txData $ txOutTxTx o

data UpdateOracleParams = UpdateOracleParams
    { uoGameId     :: Integer   -- ^ Game
    , uoWinnerId  ::  Integer  --  ^ Winner team id
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
    pkh <- pubKeyHash <$> ownPubKey
    let inst = typedOracleValidator oracle
        mrScript = oracleValidator oracle
        tokenMintingPolicy = requestTokenPolicy $ oracleToRequestToken oracle
        forgedToken = assetClassValue (requestTokenClassFromOracle oracle) 1
        oracleFee = oFee oracle 
        feeVal = Ada.toValue . Ada.lovelaceOf $ oracleFee
        oracleData = OracleData 
            { ovRequestAddress = pkh
            , ovGame = gameId
            , ovWinner = 0
            , ovWinnerSigned = Nothing
            }

    let red = Ledger.Redeemer (PlutusTx.toBuiltinData (0 :: Integer))
    let lookups = Constraints.typedValidatorLookups inst 
                <> Constraints.otherScript mrScript
                <> Constraints.mintingPolicy tokenMintingPolicy

        tx      = Constraints.mustPayToTheScript oracleData forgedToken
                <> Constraints.mustPayToPubKey (oOperator oracle) feeVal
                <> Constraints.mustMintValueWithRedeemer red forgedToken

    ledgerTx <- submitTxConstraintsWith lookups tx
    void $ awaitTxConfirmed $ txId ledgerTx

getActiveOracleRequests:: Oracle -> Contract w s Text [(TxOutRef, TxOutTx, OracleData)]
getActiveOracleRequests oracle = do
    xs <- utxoAt (oracleAddress oracle)
    let requests = filter (isActiveRequest) . filterOracleRequest oracle . Map.toList $ xs
    return requests 

getActiveGames:: Oracle -> Contract w s Text ([GameId])
getActiveGames oracle = do
    requests <- nub . map (\(_, _, or) -> ovGame or) <$> getActiveOracleRequests oracle
    return requests

awaitNextOracleRequest:: Oracle -> Contract w s Text [(TxOutRef, TxOutTx, OracleData)]
awaitNextOracleRequest oracle =
    awaitNext
    where
    awaitNext :: Contract w s Text [(TxOutRef, TxOutTx, OracleData)]
    awaitNext = do
        utxos <- awaitUtxoProduced $ oracleAddress oracle
        let txs = (flip map) ( NonEmpty.toList $ utxos) (\onchainTx -> do
                    case onchainTx of
                        (Valid tx) -> Just $ map (\(o, oref) -> (oref, TxOutTx tx o)) $ txOutRefs tx
                        _          -> Nothing)
        let filterValidTx = concat . catMaybes
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
        updateOracle oracle (opSigner op) updateOracleParams
        tell $ Last $ Just $ Updated $ uoGameId updateOracleParams
    games :: Oracle -> Promise (Last OracleContractState) OracleSchema Text ()
    games oracle = endpoint @"games" $ \_ -> do
        gamesIds <- getActiveGames oracle
        tell $ Last $ Just $ Games gamesIds

hasOracleRequestToken :: Oracle -> (TxOutRef, TxOutTx) -> Bool
hasOracleRequestToken oracle (oref, o) = 
    assetClassValueOf (txOutValue $ txOutTxOut o) (requestTokenClassFromOracle oracle) == 1

hasOracelRequestDatum :: (TxOutRef, TxOutTx) -> Bool
hasOracelRequestDatum (oref, o) = isJust . oracleValueFromTxOutTx $ o

filterOracleRequest :: Oracle -> [(TxOutRef, TxOutTx)] -> [(TxOutRef, TxOutTx, OracleData)]
filterOracleRequest oracle txs = catMaybes . map mapDatum . filter (hasOracleRequestToken oracle) $ txs

mapDatum :: (TxOutRef, TxOutTx) -> Maybe (TxOutRef, TxOutTx, OracleData)
mapDatum (oref, o) = case oracleValueFromTxOutTx o of
    Just datum -> Just (oref, o, datum)
    Nothing -> Nothing

isGameOracleRequest :: GameId -> (TxOutRef, TxOutTx, OracleData) -> Bool
isGameOracleRequest gameId (_, _, od) = gameId == (ovGame od) 

isOwnerOracleRequest:: PubKeyHash -> (TxOutRef, TxOutTx, OracleData) -> Bool
isOwnerOracleRequest owner (_, _, od) = owner == (ovRequestAddress od)

isActiveRequest:: (TxOutRef, TxOutTx, OracleData) -> Bool
isActiveRequest (_, _, od) = isNothing (ovWinnerSigned od)

findOracleRequest :: 
    forall w s. Oracle
    -> GameId
    -> PubKeyHash
    -> Contract w s Text (Maybe (TxOutRef, TxOutTx, OracleData))
findOracleRequest oracle gameId owner = do
    xs <- utxoAt (oracleAddress oracle)
    let findCriteria = find (\tx -> isOwnerOracleRequest owner tx && isGameOracleRequest gameId tx)
    let request = findCriteria . filterOracleRequest oracle . Map.toList $ xs
    pure request 

data UseOracleParams = UseOracleParams
    { uoGame           :: Integer
    }
    deriving stock (Haskell.Eq, Haskell.Show, Generic)
    deriving anyclass (ToJSON, FromJSON, ToSchema)
    
type UseOracleSchema = Endpoint "use" UseOracleParams

useOracle :: Oracle -> Contract Text UseOracleSchema Text ()
useOracle oracle =
    selectList[(use oracle)]
  where
    use :: Oracle -> Promise Text UseOracleSchema Text ()
    use oracle = endpoint @"use" $ \oracleParams -> do
        pkh <- pubKeyHash <$> Contract.ownPubKey
        oracleRequestMaybe <- findOracleRequest oracle (uoGame oracleParams) pkh
        case oracleRequestMaybe of
            Nothing -> throwError "no oracle request"
            Just (oref, o, t) -> do
                let inst = typedOracleValidator oracle
                    mrScript = oracleValidator oracle
                    redeemer = Redeemer $ PlutusTx.toBuiltinData $ Use
                    requestTokenVal = assetClassValue (requestTokenClassFromOracle oracle) 1

                let lookups = Constraints.typedValidatorLookups inst 
                            <> Constraints.otherScript mrScript
                            <> Constraints.unspentOutputs (Map.singleton oref o)
                    tx  = Constraints.mustSpendScriptOutput oref redeemer
                            <> Constraints.mustPayToPubKey pkh requestTokenVal

                ledgerTx <- submitTxConstraintsWith lookups tx
                void $ awaitTxConfirmed $ txId ledgerTx