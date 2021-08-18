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

module Week06.Oracle.Core
    ( Oracle (..)
    , OracleRedeemer (..)
    , oracleTokenName
    , oracleValue
    , oracleAsset
    , typedOracleValidator
    , oracleValidator
    , oracleAddress
    , OracleSchema
    , OracleParams (..)
    , runOracle
    , findOracle
    ) where

import           Control.Monad             hiding (fmap)
import           Data.Aeson                (FromJSON, ToJSON)
import qualified Data.Map                  as Map
import           Data.Monoid               (Last (..))
import           Data.Text                 (Text, pack)
import           GHC.Generics              (Generic)
import           Plutus.Contract           as Contract
import qualified PlutusTx
import           PlutusTx.Prelude          hiding (Semigroup(..), unless)
import           Ledger                    hiding (singleton)
import           Ledger.Constraints        as Constraints
import qualified Ledger.Typed.Scripts      as Scripts
import           Ledger.Value              as Value
import           Ledger.Ada                as Ada
import           Plutus.Contracts.Currency as Currency
import           Prelude                   (Semigroup (..), Show (..), String)
import qualified Prelude

type Game = Integer

data Oracle = Oracle
    { oOperator :: !PubKeyHash
    , oFee      :: !Integer
    , oGame     :: !Game
    } deriving (Show, Generic, FromJSON, ToJSON, Prelude.Eq, Prelude.Ord)

PlutusTx.makeLift ''Oracle

data OracleValue = OracleValue
    { 
      ovGame:: Integer
    , ovWinner: Integer
    , ovWinnerSigned: SignedMessage (Observation Integer)
    }
deriving (Show, Generic, ToJSON, FromJSON)

PlutusTx.makeIsDataIndexed ''OracleValue [('OracleValue, 0)]
PlutusTx.makeLift ''OracleValue

data OracleRedeemer = Update | Use
    deriving Show

PlutusTx.unstableMakeIsData ''OracleRedeemer

{-# INLINABLE oracleTokenName #-}
oracleTokenName :: TokenName
oracleTokenName = TokenName emptyByteString

{-# INLINABLE oracleAsset #-}
oracleAsset :: Oracle -> AssetClass
oracleAsset oracle = AssetClass (oSymbol oracle, oracleTokenName)

{-# INLINABLE oracleValue #-}
oracleValue :: TxOut -> (DatumHash -> Maybe Datum) -> Maybe OracleValue
oracleValue o f = do
    dh      <- txOutDatum o
    Datum d <- f dh
    PlutusTx.fromBuiltinData d
sc@Stablecoin{scOracle,scStablecoinTokenName,scReservecoinTokenName}
{-# INLINABLE mkOracleValidator #-}
mkOracleValidator :: Oracle -> OracleValue -> OracleRedeemer -> ScriptContext -> Bool
mkOracleValidator oracle@Oracle{oOperator} x r ctx =
    traceIfFalse "oracle value not signed"  isValueSigned $$ 
    case r of
        Update -> traceIfFalse "operator signature missing" (txSignedBy info oOperator) &&
                  traceIfFalse "invalid output datum"       validOutputDatum
        Use    -> traceIfFalse "oracle value changed"       (outputDatum == Just x) &&
                  traceIfFalse "fees not paid"              feesPaid
                  
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    ownInput :: TxOut
    ownInput = case findOwnInput ctx of
        Nothing -> traceError "oracle input missing"
        Just i  -> txInInfoResolved i

    ownOutput :: TxOut
    ownOutput = case getContinuingOutputs ctx of
        [o] -> o
        _   -> traceError "expected exactly one oracle output"

    outputDatum :: Maybe OracelValue
    outputDatum = oracleValue ownOutput (`findDatum` info)

    validOutputDatum :: Bool
    validOutputDatum = isJust outputDatum

    feesPaid :: Bool
    feesPaid =
      let
        inVal  = txOutValue ownInput
        outVal = txOutValue ownOutput
      in
        outVal `geq` (inVal <> Ada.lovelaceValueOf (oFee oracle))

     verifySigned :: SignedMessage (Observation Integer) -> PubKey -> Integer
        extractVerifyAt sm pk time =
            case Oracle.verifySignedMessageOnChain ctx pk sm of
                Left _ -> traceError "checkSignatureAndDecode failed"
                Right Observation{obsValuez} -> True

    isValueSigned :: Bool
    isValueSigned = case outputDatum of
                Just (OracelValue ov) ->  verifySigned (ovWinnerSigned ov) oOperator
                _ -> False

    


data Oracling
instance Scripts.ValidatorTypes Oracling where
    type instance DatumType Oracling = Integer
    type instance RedeemerType Oracling = OracleRedeemer

typedOracleValidator :: Oracle -> Scripts.TypedValidator Oracling
typedOracleValidator oracle = Scripts.mkTypedValidator @Oracling
    ($$(PlutusTx.compile [|| mkOracleValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode oracle)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @Integer @OracleRedeemer

oracleValidator :: Oracle -> Validator
oracleValidator = Scripts.validatorScript . typedOracleValidator

oracleAddress :: Oracle -> Ledger.Address
oracleAddress = scriptAddress . oracleValidator

data OracleParams = OracleParams
    { opFees   :: !Integer
      opGame   :: !Game
    } deriving (Show, Generic, FromJSON, ToJSON)

startOracle :: forall w s. OracleParams -> Contract w s Text Oracle
startOracle op = do
    pkh <- pubKeyHash <$> Contract.ownPubKey
    let oracle = Oracle
            { oOperator = pkh
            , oFee      = opFees op
            , oGame  
            }
    logInfo @String $ "started oracle " ++ show oracle
    return oracle

updateOracle :: forall w s. Oracle -> Integer -> Contract w s Text ()
updateOracle oracle x = do
    m <- findGameOracle oracle
    let c = Constraints.mustPayToTheScript x $ assetClassValue (oracleAsset oracle) 1
    case m of
        Nothing -> do
            ledgerTx <- submitTxConstraints (typedOracleValidator oracle) c
            awaitTxConfirmed $ txId ledgerTx
            logInfo @String $ "set initial oracle value to " ++ show x
        Just (oref, o,  _) -> do
            let lookups = Constraints.unspentOutputs (Map.singleton oref o)     <>
                          Constraints.typedValidatorLookups (typedOracleValidator oracle) <>
                          Constraints.otherScript (oracleValidator oracle)
                tx      = c <> Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData Update)
            ledgerTx <- submitTxConstraintsWith @Oracling lookups tx
            awaitTxConfirmed $ txId ledgerTx
            logInfo @String $ "updated oracle value to " ++ show x

-- findGameOracle :: forall w s. Oracle -> Contract w s Text (Maybe (TxOutRef, TxOutTx, Integer))
-- findGameOracle oracle = do
--     utxos <- Map.filter f <$> utxoAt (oracleAddress oracle)
--     return $ case Map.toList utxos of
--         [(oref, o)] -> do
--             x <- oracleValue (txOutTxOut o) $ \dh -> Map.lookup dh $ txData $ txOutTxTx o
--             return (oref, o, x)
--         _           -> Nothing
--   where
--     f :: TxOutTx -> Bool
--     f o = assetClassValueOf (txOutValue $ txOutTxOut o) (oracleAsset oracle) == 1


findGameOracle :: 
    forall w s. Oracle
    -> Game
    -> Contract w s Text (Maybe (TxOutRef, TxOutTx, OracleValue))
findGameOracle oracle = do
    utxos <- utxoAt (oracleAddress oracle)
    let sellingUserTokens = find (isOracleGame game) $ map getOracleValueFromTxOut (snd <$> Map.toList utxos)

  where
    getOracleValueFromTxOut :: TxOutTx -> Maybe OracleValue
    getOracleValueFromTxOut o = oracleValue (txOutTxOut o) $ \dh -> Map.lookup dh $ txData $ txOutTxTx o
    isOracleGame :: Game -> Maybe OracleValue -> Bool
    isOracleGame game oracleValue = case oracleValue of
        (Just OracleValue ov) -> case Oracle.verifySignedMessageOffChain ftPriceOracle sm of
            Left _                               -> False
            Right Observation{obsValue, obsTime} -> True
        _ -> False


type OracleSchema = Endpoint "update" Integer

runOracle :: OracleParams -> Contract (Last Oracle) OracleSchema Text ()
runOracle op = do
    oracle <- startOracle op
    tell $ Last $ Just oracle
    go oracle
  where
    go :: Oracle -> Contract (Last Oracle) OracleSchema Text a
    go oracle = do
        x <- endpoint @"update"
        updateOracle oracle x
        go oracle
