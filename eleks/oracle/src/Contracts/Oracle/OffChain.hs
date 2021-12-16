{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE ImportQualifiedPost   #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

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
    , RedeemOracleSchema
    , RedeemOracleParams (..)
    , UpdateOracleParams (..)
    , OracleContractState (..)
    , redeemOracleRequest
    , redeemOracle
    ) where

import Cardano.Crypto.Wallet (XPrv)
import Contracts.Oracle.Conversion
import Contracts.Oracle.OnChain
import Contracts.Oracle.RequestToken
import Contracts.Oracle.Types
import Control.Lens (view)
import Control.Monad hiding (fmap)
import Data.Aeson (FromJSON, ToJSON)
import Data.Either (rights)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map qualified as Map
import Data.Maybe (catMaybes)
import Data.Monoid (Last (..))
import Data.Text (Text, pack, unpack)
import GHC.Generics (Generic)
import Ledger hiding (MintingPolicyHash, singleton)
import Ledger.Ada as Ada
import Ledger.Constraints as Constraints
import Ledger.Value as Value
import Plutus.ChainIndex ()
import Plutus.Contract as Contract
import Plutus.Contract.Oracle (signMessage)
import PlutusTx qualified
import PlutusTx.Prelude hiding (Semigroup (..), unless)
import Prelude (Semigroup (..), Show (..), String)
import Prelude qualified as Haskell
import Schema (ToSchema)
import Types.Game

startOracle :: forall w s. OracleParams -> PaymentPubKey -> Contract w s Text Oracle
startOracle op pk = do
    pkh <- Contract.ownPaymentPubKeyHash
    when (opFees op < Ledger.minAdaTxOut) $ throwError "fee should be grater than min ada"
    let oracleRequestTokenInfo = OracleRequestToken
            { ortOperator = pkh
            , ortFee = opFees op
            }
    let oracle = Oracle
            {
              oRequestTokenSymbol = requestTokenSymbol oracleRequestTokenInfo
            , oOperator = pkh
            , oOperatorKey = pk
            , oFee = opFees op
            }
    logInfo @String $ "started oracle " ++ show oracle
    return oracle

updateOracle :: forall w s. Oracle -> PaymentPrivateKey -> UpdateOracleParams -> Contract w s Text ()
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
                                collateralVal = Ada.toValue $ Ledger.minAdaTxOut
                            let lookups = Constraints.unspentOutputs (Map.singleton oref o)
                                        <> Constraints.typedValidatorLookups (typedOracleValidator oracle)
                                        <> Constraints.otherScript (oracleValidator oracle)
                                constraints      = Constraints.mustPayToTheScript oracleData' (requestTokenVal <> collateralVal)
                                        <> Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData Update)
                                        <> Constraints.mustBeSignedBy (oOperator oracle)

                            result <- runError @_ @_ @Text $ submitTxConstraintsWith @Oracling lookups constraints
                            case result of
                                Left err -> do
                                    logWarn @Haskell.String "An error occurred. Request oracle for address failed."
                                    logWarn err
                                Right tx -> do
                                    let txi = getCardanoTxId tx
                                    logInfo @Haskell.String $ "Waiting for tx " <> Haskell.show txi <> " to complete"
                                    awaitTxConfirmed txi
                                    logInfo @Haskell.String $ "Tx confirmed.Update oracle complete. gameId:" ++ show gameId

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
    { uoGameId     :: !GameId              -- ^ Game
    , uoWinnerId   :: !TeamId              -- ^ Winner team id
    , uoGameStatus :: !FixtureStatusShort  -- ^ Game state
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
    pkh <- Contract.ownPaymentPubKeyHash
    let inst = typedOracleValidator oracle
        mrScript = oracleValidator oracle
        address = oracleAddress oracle
        tokenMintingPolicy = requestTokenPolicy $ oracleToRequestToken oracle
        forgedToken = requestTokenValue oracle
        oracleFee = oFee oracle
        feeVal = Ada.toValue oracleFee
        collateralVal = Ada.toValue $ Ledger.minAdaTxOut
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
                <> Constraints.mustBeSignedBy pkh

    handleError (\err -> logInfo $ "caught error: " ++ unpack err) $ mkTxConstraints @Oracling lookups tx >>= submitTxConfirmed . adjustUnbalancedTx
    logInfo @Haskell.String "Request oracle for address complete."

--get active request lists for oracle to process
getActiveOracleRequests:: Oracle -> Contract w s Text [(TxOutRef, ChainIndexTxOut, OracleData)]
getActiveOracleRequests oracle = do
    let address = oracleAddress oracle
    logInfo @Haskell.String $ "getActiveOracleRequests Oracle address: " ++ show address
    xs <- utxosAt address
    filtered <- Control.Monad.mapM mapDatum . filterOracleRequest oracle . Map.toList $ xs
    let requests = filter (isActiveRequest oracle) . rights $ filtered
    return requests

getActiveGames:: Oracle -> Contract w s Text ([GameId])
getActiveGames oracle = do
    requests <- nub . map (\(_, _, oracleReq) -> ovGame oracleReq) <$> getActiveOracleRequests oracle
    return requests

awaitNextOracleRequest:: Oracle -> Contract w s Text [(TxOutRef, ChainIndexTxOut, OracleData)]
awaitNextOracleRequest oracle =
    awaitNext
    where
    convertChainIndexOut:: (Maybe ChainIndexTxOut, TxOutRef) -> Maybe (TxOutRef, ChainIndexTxOut)
    convertChainIndexOut (o, oref) =  (\o' -> (oref, o')) <$> o
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
    let signPaymentKey = PaymentPrivateKey signKey
    let pk = PaymentPubKey $ toPublicKey signKey
    let pkh = paymentPubKeyHash pk
    ownPkh <- Contract.ownPaymentPubKeyHash
    when(ownPkh /= pkh) $ throwError "private key not equal to pab starter"
    oracle <- startOracle op pk

    tell $ Last $ Just $ OracleState oracle
    forever $ selectList[(update oracle signPaymentKey), (games oracle)]
  where
    update :: Oracle -> PaymentPrivateKey -> Promise (Last OracleContractState) OracleSchema Text ()
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
hasOracleRequestToken oracle (_, o) =
    assetClassValueOf (view ciTxOutValue o) (requestTokenClassFromOracle oracle) == 1

filterOracleRequest :: Oracle -> [(TxOutRef, ChainIndexTxOut)] -> [(TxOutRef, ChainIndexTxOut)]
filterOracleRequest oracle txs = filter (hasOracleRequestToken oracle) $ txs

mapDatum :: forall w s. (TxOutRef, ChainIndexTxOut) -> Contract w s Text (Either Text (TxOutRef, ChainIndexTxOut, OracleData))
mapDatum (oref, o) = do
    datumE <- runError @_ @_ @Text $ oracleValueFromTxOutTx o
    case datumE of
        Right datum -> pure $ Right (oref, o, datum)
        Left err    -> pure $ Left err

isGameOracleRequest :: GameId -> (TxOutRef, ChainIndexTxOut, OracleData) -> Bool
isGameOracleRequest gameId (_, _, od) = gameId == (ovGame od)

isOwnerOracleRequest :: PaymentPubKeyHash -> (TxOutRef, ChainIndexTxOut, OracleData) -> Bool
isOwnerOracleRequest owner (_, _, od) = owner == (ovRequestAddress od)

isActiveSignedMessage :: OracleSignedMessage -> Bool
isActiveSignedMessage message = osmGameStatus message /= FT

isActiveRequest:: Oracle -> (TxOutRef, ChainIndexTxOut, OracleData) -> Bool
isActiveRequest oracle (_, _, od) = case ovSignedMessage od of
                                -- not processed
                                Nothing -> True
                                Just message -> case verifyOracleValueSigned (oOperatorKey oracle) message of
                                    --oracle cannot parse signed message
                                    Nothing                 -> False
                                    Just (oracleMessage, _) -> isActiveSignedMessage oracleMessage

findOracleRequest ::
    forall w s. Oracle
    -> GameId
    -> PaymentPubKeyHash
    -> Contract w s Text (Maybe (TxOutRef, ChainIndexTxOut, OracleData))
findOracleRequest oracle gameId owner = do
    xs <- utxosAt (oracleAddress oracle)
    let findCriteria = find (\tx -> isOwnerOracleRequest owner tx && isGameOracleRequest gameId tx)
    requestsWithDatum <- Control.Monad.mapM mapDatum . filterOracleRequest oracle . Map.toList $ xs
    logInfo @Haskell.String $ "gameId: " ++ (show requestsWithDatum)
    logInfo @Haskell.String $ "requestsWithDatum: " ++ (show gameId)
    let request = findCriteria . rights $ requestsWithDatum
    pure request

type RedeemOracleSchema = Endpoint "redeem" RedeemOracleParams

redeemOracleRequest :: forall w s. Oracle -> GameId -> Contract w s Text ()
redeemOracleRequest oracle gameId = do
    pkh <- Contract.ownPaymentPubKeyHash
    oracleRequestMaybe <- findOracleRequest oracle gameId pkh
    (oref, o, _) <- maybe (throwError "no oracle request")
                     pure oracleRequestMaybe
    let inst = typedOracleValidator oracle
        mrScript = oracleValidator oracle
        tokenMintingPolicy = requestTokenPolicy $ oracleToRequestToken oracle
        redeemer = Redeemer $ PlutusTx.toBuiltinData $ OracleRedeem
        requestTokenVal = requestTokenValue oracle
        mintRedeemer = Redeemer $ PlutusTx.toBuiltinData $ RedeemToken

    let lookups = Constraints.typedValidatorLookups inst
                <> Constraints.otherScript mrScript
                <> Constraints.unspentOutputs (Map.singleton oref o)
                <> Constraints.mintingPolicy tokenMintingPolicy
        tx  = Constraints.mustSpendScriptOutput oref redeemer
                <> Constraints.mustMintValueWithRedeemer mintRedeemer (inv requestTokenVal)
                <> Constraints.mustBeSignedBy pkh

    mkTxConstraints lookups tx >>= submitTxConfirmed . adjustUnbalancedTx

-- example endpoint how to consume oracle request owned by user
redeemOracle :: Oracle -> Contract Text RedeemOracleSchema Text ()
redeemOracle oracle =
    selectList[redeem]
  where
    redeem :: Promise Text RedeemOracleSchema Text ()
    redeem = endpoint @"redeem" $ \oracleParams -> do
        redeemOracleRequest oracle (roGame oracleParams)
