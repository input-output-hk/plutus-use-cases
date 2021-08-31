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

module Contracts.Oracle
    ( Oracle (..)
    , OracleRequestToken (..)
    , OracleRedeemer (..)
    , oracleTokenName
    , oracleValue
    , oracleAsset
    , typedOracleValidator
    , oracleValidator
    , oracleAddress
    , OracleSchema
    , OracleParams (..)
    , OracleData (..)
    , runOracle
    , findOracle
    , oracleTokenName
    , requestOracleForAddress
    , requestTokenSymbol
    , requestTokenClass
    , requestTokenClassFromOracle
    , listenOracleRequest
    , findOracleRequest
    , UseOracleSchema
    , UseOracleParams (..)
    , useOracle
    ) where

import           Contracts.Types     
import           Control.Monad             hiding (fmap)
import           Data.Aeson                (FromJSON, ToJSON)
import qualified Data.Map                  as Map
import           Data.Monoid               (Last (..))
import           Data.Text                 (Text, pack)
import           Data.Maybe                (fromJust)
import           Data.List.NonEmpty        (NonEmpty)
import           GHC.Generics              (Generic)
import           Plutus.Contract           as Contract
import qualified PlutusTx
import           PlutusTx.Prelude          hiding (Semigroup(..), unless)
import           Ledger                    hiding (singleton, MintingPolicyHash)
import qualified Ledger.Scripts            as LedgerScripts
import qualified Ledger.Tx            as LedgerScripts
import           Ledger.Constraints        as Constraints
import qualified Ledger.Contexts           as Validation
import           Ledger.Oracle             (Observation, SignedMessage(..), signMessage, SignedMessageCheckError(..), verifySignedMessageOnChain, verifySignedMessageConstraints)
import qualified Ledger.Typed.Scripts      as Scripts
import           Ledger.Value              as Value
import           Ledger.Ada                as Ada
import           Plutus.Contracts.Currency as Currency
import           Plutus.Contract.Types     (Promise (..))
import           Prelude                   (Semigroup (..), Show (..), String)
import qualified Prelude                   as Haskell
import           Schema                    (ToSchema)

data Oracle = Oracle
    { oSymbol   :: !CurrencySymbol
    , oRequestTokenSymbol   :: !CurrencySymbol
    , oOperator :: !PubKeyHash
    , oOperatorKey :: !PubKey
    , oFee      :: !Integer
    } deriving (Show, Generic, FromJSON, ToJSON, ToSchema, Haskell.Eq, Haskell.Ord)

PlutusTx.makeLift ''Oracle

data OracleRequestToken = OracleRequestToken
    { ortOperator :: !PubKeyHash
    , ortFee      :: !Integer
    } deriving (Show, Generic, FromJSON, ToJSON, ToSchema, Haskell.Eq, Haskell.Ord)

PlutusTx.makeLift ''OracleRequestToken

oracleToRequestToken:: Oracle -> OracleRequestToken
oracleToRequestToken oracle = OracleRequestToken
    { ortOperator = oOperator oracle
    , ortFee = oFee oracle
    }

data OracleData = OracleData
    { ovGame           :: Integer
    , ovWinner         :: Integer
    , ovRequestAddress :: PubKeyHash
    , ovWinnerSigned   :: Maybe (SignedMessage Integer)
    }
    deriving (Show, Generic, FromJSON, ToJSON, Haskell.Eq)

PlutusTx.unstableMakeIsData ''OracleData
PlutusTx.makeLift ''OracleData


instance Eq OracleData where
    {-# INLINABLE (==) #-}
    l == r = (ovGame l == ovGame r) && 
             (ovWinner l == ovWinner r) &&
             (ovRequestAddress l == ovRequestAddress r) &&
             (ovWinnerSigned l PlutusTx.Prelude.== ovWinnerSigned r)

instance Eq a => Eq (SignedMessage a) where
    l == r =
        osmSignature l == osmSignature r
        && osmMessageHash l == osmMessageHash r
        && osmDatum l == osmDatum r

data OracleRedeemer = Update | Use
    deriving Show

PlutusTx.unstableMakeIsData ''OracleRedeemer

{-# INLINABLE oracleTokenName #-}
oracleTokenName :: TokenName
oracleTokenName = TokenName "oracleTokenName"

{-# INLINABLE oracleAsset #-}
oracleAsset :: Oracle -> AssetClass
oracleAsset oracle = AssetClass (oSymbol oracle, oracleTokenName)

{-# INLINABLE oracleValue #-}
oracleValue :: TxOut -> (DatumHash -> Maybe Datum) -> Maybe OracleData
oracleValue o f = do
    dh      <- txOutDatum o
    Datum d <- f dh
    PlutusTx.fromBuiltinData d

{-# INLINABLE mkOracleValidator #-}
mkOracleValidator :: Oracle -> OracleData -> OracleRedeemer -> ScriptContext -> Bool
mkOracleValidator oracle oracleData r ctx =
    traceIfFalse "request token missing from input" inputHasRequestToken  &&
    case r of
        Use    -> traceIfFalse "signed by request owner"             (txSignedBy info $ ovRequestAddress oracleData )
                  -- && traceIfFalse "value signed by oracle"      (isValueSigned)
                  && traceIfFalse "expected requester to get oracle token" 
                     (sentToAddress (Just $ ovRequestAddress oracleData) $ requestTokenExpectedVal)
        Update -> traceIfFalse "operator signature missing"     (txSignedBy info $ oOperator oracle) 
                  && traceIfFalse "invalid output datum"        validOutputDatum

  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    forged :: Value
    forged = txInfoMint $ scriptContextTxInfo ctx

    requestTokenExpectedVal:: Value
    requestTokenExpectedVal = Value.singleton (oRequestTokenSymbol oracle) oracleTokenName 1

    requestTokenValOf:: Value -> Integer 
    requestTokenValOf value = valueOf (txOutValue ownInput) (oRequestTokenSymbol oracle) oracleTokenName

    sentToAddress :: Maybe PubKeyHash -> Value -> Bool
    sentToAddress h v =
        let
        [o] = [ o'
              | o' <- txInfoOutputs info
              , txOutValue o' == v
              ]
        in
        fromMaybe False ((==) <$> Validation.pubKeyOutput o <*> h )

    ownInput :: TxOut
    ownInput = case findOwnInput ctx of
        Nothing -> traceError "oracle input missing"
        Just i  -> txInInfoResolved i

    inputHasRequestToken :: Bool
    inputHasRequestToken = requestTokenValOf (txOutValue ownInput) == 1

    ownOutput :: TxOut
    ownOutput = case getContinuingOutputs ctx of
        [o] -> o
        _   -> traceError "expected exactly one oracle request output"

    -- outputHasToken :: Bool
    -- outputHasToken = assetClassValueOf (txOutValue ownOutput) (oracleAsset oracle) == 1

    outputDatumMaybe :: Maybe OracleData
    outputDatumMaybe = oracleValue ownOutput (`findDatum` info)

    validOutputDatum :: Bool
    validOutputDatum = isJust outputDatumMaybe

    outputDatum :: OracleData
    outputDatum = case outputDatumMaybe of
        Nothing -> traceError "Input data is invalid"
        Just h  -> h

    verifyValueSigned :: Maybe (SignedMessage Integer) -> PubKey -> Maybe Integer
    verifyValueSigned smMaybe pk = case smMaybe of
        Just sm -> case verifySignedMessageOnChain ctx pk sm of
            Left err -> case err of
                SignatureMismatch sig pk hash -> traceError "SignatureMismatch"
                DatumMissing hash ->  traceError "DatumMissing"
                DecodingError -> traceError "DecodingError"
                DatumNotEqualToExpected -> traceError "DatumNotEqualToExpected"
            Right res -> Just res
        Nothing -> traceError "No signed oracle value message"

    extractSignedValue ::  Maybe (SignedMessage Integer) -> PubKey -> Integer
    extractSignedValue sm pk = case verifyValueSigned sm pk of
       Nothing    -> traceError "checkSignatureAndDecode failed"
       Just res  -> res

    isValueSigned = isJust $ verifyValueSigned (ovWinnerSigned oracleData) (oOperatorKey oracle)

    feesPaid :: Bool
    feesPaid =
      let
        inVal  = txOutValue ownInput
        outVal = txOutValue ownOutput
      in
        outVal `geq` (inVal <> Ada.lovelaceValueOf (oFee oracle))

data Oracling
instance Scripts.ValidatorTypes Oracling where
    type instance DatumType Oracling = OracleData
    type instance RedeemerType Oracling = OracleRedeemer

typedOracleValidator :: Oracle -> Scripts.TypedValidator Oracling
typedOracleValidator oracle = Scripts.mkTypedValidator @Oracling
    ($$(PlutusTx.compile [|| mkOracleValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode oracle)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @OracleData @OracleRedeemer

oracleValidator :: Oracle -> Validator
oracleValidator = Scripts.validatorScript . typedOracleValidator

oracleValidatorHash :: Oracle -> Ledger.ValidatorHash
oracleValidatorHash oracle = LedgerScripts.validatorHash . oracleValidator $ oracle

oracleAddress :: Oracle -> Ledger.Address
oracleAddress = scriptAddress . oracleValidator

{-# INLINABLE checkRequesTokenPolicy #-}
checkRequesTokenPolicy :: OracleRequestToken -> () -> ScriptContext -> Bool
checkRequesTokenPolicy requestToken _ ctx@ScriptContext{scriptContextTxInfo=TxInfo{txInfoInputs}, scriptContextPurpose=Minting _} = 
    traceIfFalse "Should forge one token" (forgedSymbolsCount == 1)
    && traceIfFalse "Is fee paid" (isFeePaid (Just $ ortOperator requestToken))
    where
        ownSymbol = ownCurrencySymbol ctx
        info = scriptContextTxInfo ctx
        forged = txInfoMint info
        forgedSymbolsCount = length $ symbols forged
        feeValue = Ada.toValue . Ada.lovelaceOf $ ortFee requestToken
        isFeePaid :: Maybe PubKeyHash -> Bool
        isFeePaid feeAddr = isJust . find (\o ->
            txOutValue o == feeValue &&
            feeAddr == Validation.pubKeyOutput o) $ txInfoOutputs info

requestTokenPolicy :: OracleRequestToken -> LedgerScripts.MintingPolicy
requestTokenPolicy oracle = LedgerScripts.mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \oracle -> Scripts.wrapMintingPolicy (checkRequesTokenPolicy oracle) ||])
        `PlutusTx.applyCode`
            PlutusTx.liftCode oracle

oracleRequestMintPolicyHash :: OracleRequestToken -> LedgerScripts.MintingPolicyHash
oracleRequestMintPolicyHash = mintingPolicyHash . requestTokenPolicy

requestTokenSymbol :: OracleRequestToken -> CurrencySymbol
requestTokenSymbol = Value.mpsSymbol . oracleRequestMintPolicyHash

requestTokenClass :: OracleRequestToken -> AssetClass
requestTokenClass oracleRequest = AssetClass (requestTokenSymbol oracleRequest, oracleTokenName)

requestTokenClassFromOracle :: Oracle -> AssetClass
requestTokenClassFromOracle = requestTokenClass . oracleToRequestToken

data OracleParams = OracleParams
    { opSymbol   :: !CurrencySymbol
    , opFees   :: !Integer
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
            { oSymbol = opSymbol op
            , oRequestTokenSymbol = requestTokenSymbol oracleRequestTokenInfo
            , oOperator = pkh
            , oOperatorKey = pk
            , oFee = opFees op
            }
    logInfo @String $ "started oracle " ++ show oracle
    return oracle

updateOracle :: forall w s. Oracle -> OracleData -> Contract w s Text ()
updateOracle oracle oracleData = do
    m <- findOracle oracle
    let c = Constraints.mustPayToTheScript oracleData $ assetClassValue (oracleAsset oracle) 1
    case m of
        Nothing -> do
            ledgerTx <- submitTxConstraints (typedOracleValidator oracle) c
            awaitTxConfirmed $ txId ledgerTx
            logInfo @String $ "set initial oracle value to " ++ show oracleData
        Just (oref, o,  _) -> do
            let lookups = Constraints.unspentOutputs (Map.singleton oref o)     <>
                          Constraints.typedValidatorLookups (typedOracleValidator oracle) <>
                          Constraints.otherScript (oracleValidator oracle)
                tx      = c <> Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData Update)
            ledgerTx <- submitTxConstraintsWith lookups tx
            awaitTxConfirmed $ txId ledgerTx
            logInfo @String $ "updated oracle value to " ++ show oracleData


oracleValueFromTxOutTx :: TxOutTx -> Maybe OracleData
oracleValueFromTxOutTx o = oracleValue (txOutTxOut o) $ \dh -> Map.lookup dh $ txData $ txOutTxTx o

findOracle :: forall w s. Oracle -> Contract w s Text (Maybe (TxOutRef, TxOutTx, OracleData))
findOracle oracle = do
    utxos <- Map.filter f <$> utxoAt (oracleAddress oracle)
    return $ case Map.toList utxos of
        [(oref, o)] -> do
            x <- oracleValueFromTxOutTx o
            return (oref, o, x)
        _           -> Nothing
  where
    f :: TxOutTx -> Bool
    f o = assetClassValueOf (txOutValue $ txOutTxOut o) (oracleAsset oracle) == 1

type OracleSchema = Endpoint "update" OracleData

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

    let lookups = Constraints.typedValidatorLookups inst 
                <> Constraints.otherScript mrScript
                <> Constraints.mintingPolicy tokenMintingPolicy

        tx      = Constraints.mustPayToTheScript oracleData forgedToken
                <> Constraints.mustPayToPubKey (oOperator oracle) feeVal
                <> Constraints.mustMintValue forgedToken

    ledgerTx <- submitTxConstraintsWith lookups tx
    void $ awaitTxConfirmed $ txId ledgerTx

listenOracleRequest :: Oracle -> PrivateKey -> Contract w s Text ()
listenOracleRequest oracle oraclePrivKey= do 
    listen
  where
    signUtxo:: Oracle -> Tx -> Contract w s Text ()
    signUtxo oracle listenTx = do
        let test = txOutRefs $ listenTx 
        let requetTokenRefM = find (\(o, or) -> assetClassValueOf (txOutValue o) (requestTokenClassFromOracle oracle) == 1) . txOutRefs $ listenTx 
        logInfo ("process tx " ++ (show $ listenTx))
        when (PlutusTx.Prelude.isNothing requetTokenRefM) $ throwError "no request token in utxo"
        let (o, oref) = fromJust requetTokenRefM 
            txOutTx = TxOutTx listenTx o
            oracleDataM = oracleValueFromTxOutTx txOutTx
        when (PlutusTx.Prelude.isNothing oracleDataM) $ throwError "no oracle data in utxo"
        let oracleData = fromJust oracleDataM 
        logInfo ("my oracle data " ++ (show $ oracleData))
        when (PlutusTx.Prelude.isJust $ ovWinnerSigned oracleData) $ throwError "already signed"

        let winnerId = 1 -- TODO calculate
            oracleData' = oracleData { ovWinner = winnerId, ovWinnerSigned = Just $ signMessage 1 oraclePrivKey }
            requestTokenVal = assetClassValue (requestTokenClassFromOracle oracle) 1
        let 
            lookups = Constraints.unspentOutputs (Map.singleton oref txOutTx)     
                    <> Constraints.typedValidatorLookups (typedOracleValidator oracle) 
                    <> Constraints.otherScript (oracleValidator oracle)
            tx      = Constraints.mustPayToTheScript oracleData' requestTokenVal 
                    <> Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData Update)

        ledgerTx <- submitTxConstraintsWith lookups tx
        void $ awaitTxConfirmed $ txId ledgerTx

    listen = do
        txs <- awaitUtxoProduced $ oracleAddress oracle
        forM_ txs $ \onchainTx -> do
             case onchainTx of
                (Valid tx) -> 
                    handleError (\e -> logInfo e ) $ signUtxo oracle tx
                _          -> return ()
        listen
       
runOracle :: OracleParams -> Contract (Last Oracle) OracleSchema Text ()
runOracle op = do
    oracle <- startOracle op
    tell $ Last $ Just oracle
    selectList[(go oracle)]
  where
    go :: Oracle -> Promise (Last Oracle) OracleSchema Text ()
    go oracle = endpoint @"update" $ \oracleData -> do
            updateOracle oracle oracleData

findOracleRequest :: 
    forall w s. Oracle
    -> Integer
    -> PubKeyHash
    -> Contract w s Text (Maybe (TxOutRef, TxOutTx, OracleData))
findOracleRequest oracle gameId owner = do
    utxos <- Map.filter (hasOracleRequestToken oracle) <$> utxoAt (oracleAddress oracle)
    let requestToken = find (isOracleRequest gameId owner) $ getUtxoWithDatum $ Map.toList utxos
    pure requestToken 
  where
    getUtxoWithDatum :: [(TxOutRef, TxOutTx)] -> [(TxOutRef, TxOutTx, OracleData)] 
    getUtxoWithDatum utxo = [(oref, o, fromJust datum) 
        | (oref, o) <- utxo, 
        let datum = oracleValueFromTxOutTx o, isJust datum]
        
    hasOracleRequestToken :: Oracle -> TxOutTx -> Bool
    hasOracleRequestToken oracle o = 
        assetClassValueOf (txOutValue $ txOutTxOut o) (requestTokenClassFromOracle oracle) == 1

    isOracleRequest :: GameId -> PubKeyHash -> (TxOutRef, TxOutTx, OracleData) -> Bool
    isOracleRequest gameId owner (_, _, od) = 
        gameId == (ovGame od) 
        && owner == (ovRequestAddress od)

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
        when (PlutusTx.Prelude.isNothing oracleRequestMaybe) $ throwError "no oracle request"
        let (oref, o, t) = fromJust oracleRequestMaybe  

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