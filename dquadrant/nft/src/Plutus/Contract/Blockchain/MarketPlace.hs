{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE MonoLocalBinds     #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE ViewPatterns       #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
-- | Implements a custom currency with a monetary policy that allows
--   the forging of a fixed amount of units.
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Plutus.Contract.Blockchain.MarketPlace
where

import Control.Monad ( Monad((>>), (>>=)), void )
import           GHC.Generics              (Generic)
import qualified Data.Map               as Map
import           Data.Text              (Text)
import           Data.Void              (Void)
import           Plutus.Contract        as Contract hiding (when)
import           Plutus.Trace.Emulator  as Emulator
import qualified PlutusTx
import qualified PlutusTx.AssocMap as AssocMap
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import           Ledger                 hiding (singleton)
import qualified PlutusTx.Prelude  as PlutusPrelude
import           Ledger.Constraints     as Constraints
import qualified Ledger.Typed.Scripts   as Scripts
import           Ledger.Value           as Value
import           Playground.Contract    (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema, adaCurrency)
import           Playground.TH          (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types       (KnownCurrency (..))
import qualified Prelude
import           Prelude                (Semigroup (..))
import           Text.Printf            (printf)
import Data.Aeson (FromJSON, ToJSON, Value (Bool))
import  Ledger.Ada
import qualified Ledger.Ada as Ada
import Data.Semigroup (Last)
import PlutusTx.These

newtype Payment = Payment ( AssocMap.Map PubKeyHash Ledger.Value )
    deriving stock (Generic)
    deriving anyclass (ToJSON, FromJSON)
    deriving Show


instance PlutusPrelude.Semigroup Payment where
    {-# INLINABLE (<>) #-}
    (<>) = combinePayments


{-# INLINABLE combinePayments #-}
combinePayments ::  Payment -> Payment -> Payment
combinePayments  (Payment a) (Payment b) = Payment (a PlutusPrelude.<> b)
{-# INLINABLE foldPaymnents #-}
foldPaymnents :: [Payment] ->Payment
foldPaymnents = PlutusPrelude.foldl (PlutusPrelude.<>) emptyPayment

{-# INLINABLE payment  #-}
payment :: PubKeyHash -> Ledger.Value -> Payment
payment pkHash value=Payment  (AssocMap.singleton pkHash value)

{-# INLINABLE lovelacePayment #-}
lovelacePayment:: PubKeyHash -> Integer ->Payment
lovelacePayment pkh lovelace=payment pkh (lovelaceValueOf lovelace)


{-# INLINABLE hasLovelace  #-}
hasLovelace::Payment -> PubKeyHash -> Integer -> Bool
hasLovelace (Payment p) pkh pay = case AssocMap.lookup pkh p of
        Nothing -> False
        Just x -> valueOf x adaSymbol  adaToken >=pay

{-# INLINABLE paymentPkhs #-}
paymentPkhs :: Payment -> [PubKeyHash]
paymentPkhs (Payment x) =  AssocMap.keys x


{-# INLINABLE emptyPayment  #-}
emptyPayment ::Payment
emptyPayment = Payment AssocMap.empty

data MarketOperation =MarketOperation{
    action:: MarketOperation,
    asset  :: AssetClass,
    lovelaceCost  :: Integer
} deriving(Show ,Generic,FromJSON,ToJSON)

data Market = Market
    { operator   :: !PubKeyHash
    , fee      :: !Integer
    } deriving (Show,Generic, FromJSON, ToJSON, Prelude.Eq)

data SellParam =SellParam
    {
        sellParamAsset :: AssetClass
    ,    sellParamCost  :: Integer
    } deriving(Show, Generic , FromJSON,ToJSON)

PlutusTx.makeLift ''SellParam
PlutusTx.makeLift ''Market

sellParam :: AssetClass -> Integer -> SellParam
sellParam = SellParam

data MarketAction = Showcase | Buy | ClaimFees deriving (FromJSON,ToJSON,Show,Generic)

PlutusTx.makeLift ''MarketAction
PlutusTx.unstableMakeIsData ''MarketAction

newtype MarketUtxoData=MarketUtxoData (Maybe (Integer,PubKeyHash))
PlutusTx.unstableMakeIsData ''MarketUtxoData
PlutusTx.makeLift ''MarketUtxoData

marketUtxoData::forall w s e. (HasOwnPubKey  s,AsContractError e )=>Integer  -> Contract w s e MarketUtxoData
marketUtxoData cost=do
        pk<-  ownPubKey
        pure $ MarketUtxoData (Just (cost,pubKeyHash pk))

{-# INLINABLE marketUtxoCost #-}
marketUtxoCost :: MarketUtxoData -> Ledger.Value
marketUtxoCost (MarketUtxoData (Just (cost,_))) = lovelaceValueOf  cost

{-# INLINABLE emptyMarketUtxoData  #-}
emptyMarketUtxoData :: MarketUtxoData
emptyMarketUtxoData=MarketUtxoData Nothing

{-# INLINABLE mkMarket #-}
mkMarket :: Market -> MarketUtxoData  -> MarketAction -> ScriptContext -> Bool
mkMarket nft@Market{operator=operator,fee=fee} x action ctx@ScriptContext{scriptContextTxInfo=info@TxInfo{txInfoInputs=inputs, txInfoOutputs=outputs},scriptContextPurpose=Spending txoutRef} =
    case action of
        Buy       -> traceIfFalse "Insufficient payment" allSellersPaid
        ClaimFees -> traceIfFalse "Missing Operator signature" signedByOperator
    where
        signedByOperator=True
        ownAddress=scriptHashAddress (ownHash ctx)

        marketInputs::[TxOut]
        marketInputs = filter (\x->txOutAddress x==ownAddress) (map (\x->txInInfoResolved x) inputs)

        allSellersPaid::Bool
        allSellersPaid = foldl (\truth pkh ->truth && isSellerPaid pkh) True  (paymentPkhs totalPayment)

        isSellerPaid:: PubKeyHash ->Bool
        isSellerPaid pkh=hasLovelace totalPayment pkh $ valueOf  (valuePaidTo info pkh) adaSymbol adaToken


        totalPayment :: Payment
        totalPayment =foldPaymnents $ map (\a->paymentInfo a) marketInputs

        paymentInfo :: TxOut  ->  Payment
        paymentInfo  ( TxOut _ _ (Just x)) = case findDatum x info of
                        Just (Datum d) ->  case PlutusTx.fromData  d of
                                                Just (MarketUtxoData (Just (v,hash))) -> lovelacePayment hash v
                                                _ -> traceError "No cost info"
                        _ -> traceError "No Cost info"
        costOf  _                      = traceError "No Cost info"

data MarketType
instance Scripts.ScriptType MarketType where
    type instance DatumType MarketType = MarketUtxoData
    type instance RedeemerType MarketType =  MarketAction

marketScript :: Market -> Scripts.ScriptInstance MarketType
marketScript market = Scripts.validator @MarketType
    ($$(PlutusTx.compile [|| mkMarket ||]) `PlutusTx.applyCode` PlutusTx.liftCode market)
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.wrapValidator @MarketUtxoData @MarketAction

marketValidator :: Market -> Validator
marketValidator = Scripts.validatorScript . marketScript

marketAddress :: Market -> Ledger.Address
marketAddress = scriptAddress . marketValidator

moveToMarket :: forall w s e. (HasOwnPubKey s, HasBlockchainActions s,HasWriteTx s,AsContractError e ) =>Market  -> AssetClass ->Integer -> Contract w s e ()
moveToMarket market@Market{operator=operator,fee=fee} asset price= do
    cost <- marketUtxoData fee
    ledgerTx <- submitTxConstraints inst (Constraints.mustPayToTheScript cost value)
    awaitTxConfirmed ( txId ledgerTx)
    Contract.logInfo @String "Wow"
    where
        value  = assetClassValue asset 1
        inst = marketScript market

buyFromMarket :: forall w s. HasBlockchainActions s => Market -> AssetClass -> Contract w s Text ()
buyFromMarket market asset = do
    utxoInfo <- findInMarket market asset
    case utxoInfo of
        Nothing -> do
            logError  @String $ "It was either not on sale or is not on sale anymore " ++ show asset
        Just (oref, o,  cost) -> do
            cost<-marketUtxoData cost
            let tx = Constraints.mustPayToTheScript emptyMarketUtxoData   (marketUtxoCost cost) <>
                     Constraints.mustSpendScriptOutput oref (Redeemer ( PlutusTx.toData Buy))
                lookups = Constraints.unspentOutputs (Map.singleton oref o)     <>
                          Constraints.scriptInstanceLookups (marketScript market)
            ledgerTx <- submitTxConstraintsWith @MarketType lookups tx
            awaitTxConfirmed $ txId ledgerTx
            logInfo @String $ "Bought asset "++ show asset ++ "at the cost of " ++ show (marketUtxoCost cost) ++" Lovelace"

findInMarket :: forall w s. HasBlockchainActions s => Market -> AssetClass  -> Contract w s Text (Maybe (TxOutRef, TxOutTx, Integer))
findInMarket market asset = do
    utxos <- Map.filter hasAsset <$> utxoAt (marketAddress market)
    return $ case Map.toList utxos of
        [(oref, o)] -> do
            x <- nftCost (txOutTxOut o) $ \dh -> Map.lookup dh $ txData $ txOutTxTx o
            return (oref, o, x)
        _           -> Nothing
  where
    hasAsset :: TxOutTx -> Bool
    hasAsset o = assetClassValueOf (txOutValue $ txOutTxOut o) asset == 1

    nftCost :: TxOut -> (DatumHash -> Maybe Datum) -> Maybe Integer
    nftCost o f = do
        dh      <- txOutDatum o
        Datum d <- f dh
        PlutusTx.fromData d

type MarketSchema =
    BlockchainActions
        .\/ Endpoint "sell"     SellParam
--        .\/ Endpoint "withdraw" (AssetClass)
        .\/ Endpoint "buy"      AssetClass
--        .\/ Endpoint "collect"  ()

openTheMarket :: Market -> Contract () MarketSchema Text  ()
openTheMarket market = (sell `select` buy ) >> openTheMarket market
  where
    sell :: Contract w MarketSchema Text ()
    sell= h $ do
        sp <- endpoint @"sell"
        moveToMarket market (sellParamAsset sp) (sellParamCost sp)
    buy ::  Contract w MarketSchema Text ()
    buy = h $ do
        asset <-endpoint @"buy"
        buyFromMarket market asset

    h :: Contract w MarketSchema Text () -> Contract w MarketSchema Text ()
    h = handleError logError