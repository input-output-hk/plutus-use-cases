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
{-# LANGUAGE LambdaCase            #-}

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
    , redeemOracleRequest
    , useOracle
    ) where

import           Cardano.Api.Shelley       (PlutusScript (..), PlutusScriptV1) 
import           Control.Lens              (view)
import           Control.Monad             hiding (fmap)
import           Contracts.Oracle.Conversion
import           Contracts.Oracle.Types
import           Contracts.Oracle.RequestToken
import           Contracts.Oracle.OnChain
import           Codec.Serialise
import           Data.Aeson                (FromJSON, ToJSON)
import           Data.Void                 (absurd)
import qualified Data.ByteString.Short     as SBS
import qualified Data.ByteString.Lazy      as LBS
import           Data.Either               (fromRight, rights)
import qualified Data.Map                  as Map
import           Data.Maybe                (catMaybes)
import           Data.Monoid               (Last (..))
import           Data.Text                 (Text, pack, unpack)
import qualified Data.List.NonEmpty        as NonEmpty
import           Data.Void (Void)
import           GHC.Generics              (Generic)
import           Plutus.Contract           as Contract
import qualified PlutusTx
import           PlutusTx.Prelude          hiding (Semigroup(..), unless)
import           Ledger                    hiding (singleton, MintingPolicyHash)
import           Ledger.Crypto             (Passphrase, toPublicKey, pubKeyHash)
import qualified Ledger.Scripts            as LedgerScripts
import qualified Ledger.Tx                 as LedgerScripts
import           Ledger.Constraints        as Constraints
import qualified Ledger.Contexts           as Validation
import           Plutus.Contract.Oracle    (Observation, SignedMessage(..), signMessage, SignedMessageCheckError(..), checkSignature)
import qualified Ledger.Typed.Scripts      as Scripts
import           Ledger.Value              as Value
import           Ledger.Ada                as Ada
import           Types.Game
import           Plutus.Contracts.Currency as Currency
import           Plutus.Contract.Types     (Promise (..), _OtherError)
import           Prelude                   (Semigroup (..), Show (..), String)
import qualified Prelude                   as Haskell
import           Schema                    (ToSchema)
import           Plutus.ChainIndex         ()
import           Cardano.Crypto.Wallet     (xprv, xpub, XPrv, XPub)

startOracle :: forall w s. OracleParams -> PubKey -> Contract w s Text Oracle
startOracle op pk = do
    pkh <- Contract.ownPubKeyHash
    when (opFees op < minAdaTxOut) $ throwError "fee should be grater than min ada"
    when (opCollateral op < minAdaTxOut) $ throwError "collateral should be grater than min ada"
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
                        let oracleData' = oracleData{ ovSignedMessage = Just $ signMessage oracleSignMessage operatorPrivateKey ""   }
                        when (oracleData' /= oracleData) $ do
                            let requestTokenVal = requestTokenValue oracle
                                collateralVal = Ada.toValue $ oCollateral oracle 
                            let lookups = Constraints.unspentOutputs (Map.singleton oref o)     
                                        <> Constraints.typedValidatorLookups (typedOracleValidator oracle) 
                                        <> Constraints.otherScript (oracleValidator oracle)
                                tx      = Constraints.mustPayToTheScript oracleData' (requestTokenVal <> collateralVal) 
                                        <> Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData Update)

                            result <- runError @_ @_ @Text $ submitTxConstraintsWith @Oracling lookups tx
                            case result of
                                Left err -> do
                                    logWarn @Haskell.String "An error occurred. Request oracle for address failed."
                                    logWarn err
                                Right tx -> do
                                    let txi = getCardanoTxId tx
                                    logInfo @Haskell.String $ "Waiting for tx " <> Haskell.show txi <> " to complete"
                                    awaitTxConfirmed txi
                                    logInfo @Haskell.String "Tx confirmed. Request oracle for address complete."
                            -- logInfo ("submit transaction " ++ (show $ oracleData'))
                            -- mkTxConstraints lookups tx >>= submitTxConfirmed . adjustUnbalancedTx

-- oracleValueFromTxOutTx :: ChainIndexTxOut -> Maybe OracleData
-- oracleValueFromTxOutTx o = do
--     Datum d <- either (const Nothing) Just (_ciTxOutDatum o)
--     PlutusTx.fromBuiltinData d

oracleValueFromTxOutTx :: ChainIndexTxOut -> Contract w s Text OracleData
oracleValueFromTxOutTx o = 
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
    logInfo @Haskell.String "Request oracle for address"
    pkh <- Contract.ownPubKeyHash
    let inst = typedOracleValidator oracle
        operatorKey = (oOperator oracle)
        mrScript = oracleValidator oracle
        address = oracleAddress oracle
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
    logInfo @Haskell.String $ "Oracle address: " ++ show address
    logInfo @Haskell.String $ "Oracle data: " ++ show oracleData
    let lookups = 
                Constraints.typedValidatorLookups inst 
                <> Constraints.otherScript mrScript
                <> Constraints.mintingPolicy tokenMintingPolicy

        tx      = Constraints.mustPayToTheScript oracleData (forgedToken <> collateralVal)
                <> Constraints.mustPayToPubKey (oOperator oracle) (feeVal)
                <> Constraints.mustMintValueWithRedeemer mintRedeemer forgedToken

    handleError (\err -> logInfo $ "caught error: " ++ unpack err) $ mkTxConstraints @Oracling lookups tx >>= submitTxConfirmed . adjustUnbalancedTx
    logInfo @Haskell.String "Request oracle for address complete."
    -- result <- runError @_ @_ @Text $ submitTxConstraintsWith @Oracling lookups tx
    -- case result of
    --     Left err -> do
    --         logWarn @Haskell.String "An error occurred. Request oracle for address failed."
    --         logWarn err
    --     Right tx -> do
    --         let txi = getCardanoTxId tx
    --         logInfo @Haskell.String $ "Waiting for tx " <> Haskell.show txi <> " to complete"
    --         awaitTxConfirmed txi
    --         logInfo @Haskell.String "Tx confirmed. Request oracle for address complete."

--get active request lists for oracle to process
getActiveOracleRequests:: Oracle -> Contract w s Text [(TxOutRef, ChainIndexTxOut, OracleData)]
getActiveOracleRequests oracle = do
    let address = oracleAddress oracle
    logInfo @Haskell.String $ "Oracle address: " ++ show address
    xs <- utxosAt address
    logInfo @Haskell.String ("Get active utxos: " ++ show xs)
    filtered <- Control.Monad.mapM mapDatum . filterOracleRequest oracle . Map.toList $ xs
    logInfo @Haskell.String ("Filtered: " ++ show filtered)
    let requests = filter (isActiveRequest oracle) . rights $ filtered
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
        oracleRequests <- Control.Monad.mapM mapDatum . filterOracleRequest oracle . filterValidTx $ txs
        let requestWithDatum = rights oracleRequests
        return requestWithDatum
            
runOracle :: OracleParams -> Contract (Last OracleContractState) OracleSchema Text ()
runOracle op = do
    let signKeyE:: Either String XPrv = decodeKeyFromDto $ opSigner op
    signKey <- either (throwError . pack) pure signKeyE
    let pk = toPublicKey signKey
    let pkh = pubKeyHash pk
    ownPkh <- Contract.ownPubKeyHash
    when(ownPkh /= pkh) $ throwError "private key not equal to pab starter"
    oracle <- startOracle op pk

    tell $ Last $ Just $ OracleState oracle
    forever $ selectList[(update oracle signKey), (games oracle)]
  where
    update :: Oracle -> PrivateKey -> Promise (Last OracleContractState) OracleSchema Text ()
    update oracle signKey = endpoint @"update" $ \updateOracleParams -> do
        logInfo @String "update called"
        logInfo $ show updateOracleParams
        updateOracle oracle signKey updateOracleParams
        tell $ Last $ Just $ Updated $ uoGameId updateOracleParams
    games :: Oracle -> Promise (Last OracleContractState) OracleSchema Text ()
    games oracle = endpoint @"games" $ \_ -> do
        gamesIds <- getActiveGames oracle
        tell $ Last $ Just $ Games gamesIds

hasOracleRequestToken :: Oracle -> (TxOutRef, ChainIndexTxOut) -> Bool
hasOracleRequestToken oracle (oref, o) = 
    assetClassValueOf (view ciTxOutValue o) (requestTokenClassFromOracle oracle) == 1

filterOracleRequest :: Oracle -> [(TxOutRef, ChainIndexTxOut)] -> [(TxOutRef, ChainIndexTxOut)]
filterOracleRequest oracle txs = filter (hasOracleRequestToken oracle) $ txs

-- mapDatum :: (TxOutRef, ChainIndexTxOut) -> Either Text (TxOutRef, ChainIndexTxOut, OracleData)
-- mapDatum (oref, o) = case oracleValueFromTxOutTx o of
--     Just datum -> Right (oref, o, datum)
--     Nothing -> Left "No datum"

mapDatum :: forall w s. (TxOutRef, ChainIndexTxOut) -> Contract w s Text (Either Text (TxOutRef, ChainIndexTxOut, OracleData))
mapDatum (oref, o) = do
    datumE <- runError @_ @_ @Text $ oracleValueFromTxOutTx o
    case datumE of
        Right datum -> pure $ Right (oref, o, datum)
        Left err -> pure $ Left err

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
    requestsWithDatum <- Control.Monad.mapM mapDatum . filterOracleRequest oracle . Map.toList $ xs
    let request = findCriteria . rights $ requestsWithDatum
    pure request 
    
type UseOracleSchema = Endpoint "use" UseOracleParams

redeemOracleRequest :: forall w s. Oracle -> GameId -> Contract w s Text ()
redeemOracleRequest oracle gameId = do
    pkh <- Contract.ownPubKeyHash
    oracleRequestMaybe <- findOracleRequest oracle gameId pkh
    (oref, o, t) <- maybe (throwError "no oracle request") 
                     pure oracleRequestMaybe
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

-- example endpoint how to consume oracle request owned by user
useOracle :: Oracle -> Contract Text UseOracleSchema Text ()
useOracle oracle =
    selectList[(use oracle)]
  where
    use :: Oracle -> Promise Text UseOracleSchema Text ()
    use oracle = endpoint @"use" $ \oracleParams -> do
        redeemOracleRequest oracle (uoGame oracleParams)