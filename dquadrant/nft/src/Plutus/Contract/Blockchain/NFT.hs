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
module Plutus.Contract.Blockchain.NFT(
      mint
    ) where
import Control.Monad ( Monad((>>), (>>=)), void )
import           GHC.Generics              (Generic)
import qualified Data.Map               as Map
import           Data.Text              (Text)
import           Data.Void              (Void)
import           Plutus.Contract        as Contract hiding (when)
import           Plutus.Trace.Emulator  as Emulator
import qualified PlutusTx
import qualified PlutusTx.AssocMap
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import           Ledger                 hiding (singleton)
import           Ledger.Constraints     as Constraints
import qualified Ledger.Typed.Scripts   as Scripts
import           Ledger.Value           as Value
import           Playground.Contract    (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema)
import           Playground.TH          (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types       (KnownCurrency (..))
import qualified Prelude
import           Prelude                (Semigroup (..))
import           Text.Printf            (printf)
import Data.Aeson (FromJSON, ToJSON, Value (Bool))
import  Ledger.Ada
import qualified Ledger.Ada as Ada


{-# ANN module ("HLint: ignore Use uncurry" :: String) #-}

policyScript :: TxOutRef  -> ScriptContext -> Bool
policyScript utxo ctx@ScriptContext{scriptContextTxInfo=txinfo} =
    traceIfFalse "UTXo missing" $ any (\i -> txInInfoOutRef i == utxo) $ txInfoInputs txinfo

policy :: TxOutRef  -> MonetaryPolicy
policy r = mkMonetaryPolicyScript $
    $$(PlutusTx.compile [|| \c -> Scripts.wrapMonetaryPolicy (policyScript c) ||])
        `PlutusTx.applyCode`
            PlutusTx.liftCode r


computeNftSymbol :: TxOutRef  -> CurrencySymbol
computeNftSymbol = scriptCurrencySymbol . policy

datumToOwner :: TxOut -> (DatumHash -> Maybe Datum) -> Maybe PubKeyHash
datumToOwner o f = do
    dh      <- txOutDatum o
    Datum d <- f dh
    PlutusTx.fromData d

mint:: forall w s e. (HasWriteTx s,HasOwnPubKey s,HasUtxoAt s,
     HasTxConfirmation s
    ,AsContractError e, HasOwnPubKey s) => Contract w s e (Maybe CurrencySymbol )
mint   = do
    pk    <- Contract.ownPubKey
    utxos <- utxoAt $ pubKeyAddress pk
    case Map.keys utxos of
        []       -> do
            Contract.logError @String "no utxo found"
            pure $ Nothing
        oref : _ -> do
            let val     = Value.singleton (computeNftSymbol oref ) (TokenName emptyByteString)  1
                lookups = Constraints.monetaryPolicy (policy oref) <> Constraints.unspentOutputs utxos
                tx      = Constraints.mustForgeValue val <> Constraints.mustSpendPubKeyOutput oref
            ledgerTx <- submitTxConstraintsWith @Void lookups tx
            void $ awaitTxConfirmed $ txId ledgerTx
            Contract.logInfo @String $ printf "forged %s" (show val)
            pure $ Just (computeNftSymbol oref)

data NFT = NFT
    { nftSymbol   :: !CurrencySymbol
    , nftFoundation :: !PubKeyHash
    , nftFee      :: !Integer
    } deriving (Show,Generic, FromJSON, ToJSON, Prelude.Eq, Prelude.Ord)

PlutusTx.makeLift ''NFT

{-# INLINABLE mkHolder #-}
mkHolder :: NFT -> PubKeyHash -> Bool -> ScriptContext -> Bool
mkHolder nft@NFT{nftSymbol=sym,nftFoundation=foundation, nftFee=fee} x redeem ctx@ScriptContext{scriptContextTxInfo=info} =
    if redeem 
        then traceIfFalse "Missing Foundation signature" hasFoundationSignature 
        else traceIfFalse "token missing from input"  outputsToSameScript  &&
             traceIfFalse "missing fee"               feesPaid         &&
             traceIfFalse  "Owner signature missing"   hasOwnerSignature
  where

    ownInput :: TxOut
    ownInput = case findOwnInput ctx of
        Nothing -> traceError "oracle input missing"
        Just i  -> txInInfoResolved i

    ownOutput :: TxOut
    ownOutput = case getContinuingOutputs ctx of
        [o] -> o
        _   -> traceError "expected exactly one oracle output"

    outputsToSameScript :: Bool
    outputsToSameScript = assetClassValueOf (txOutValue ownOutput) (AssetClass (sym, TokenName emptyByteString)) == 1

    feesPaid :: Bool
    feesPaid =
      let
        inVal  = txOutValue ownInput
        outVal = txOutValue ownOutput
      in
        outVal `geq` (inVal <> Ada.lovelaceValueOf fee)

    hasFoundationSignature::Bool
    hasFoundationSignature= txSignedBy info foundation

    hasOwnerSignature :: Bool
    hasOwnerSignature=case datumToOwner  ownInput  (`findDatum` info) of
        Nothing -> False
        Just currentOwner ->txSignedBy info currentOwner

data NFTType
instance Scripts.ScriptType NFTType where
    type instance DatumType NFTType = PubKeyHash
    type instance RedeemerType NFTType = Bool 

oracleInst :: NFT -> Scripts.ScriptInstance NFTType
oracleInst nft = Scripts.validator @NFTType
    ($$(PlutusTx.compile [|| mkHolder ||]) `PlutusTx.applyCode` PlutusTx.liftCode nft)
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.wrapValidator @PubKeyHash @Bool



enableHolder :: forall w s. HasBlockchainActions s => NFT -> Contract w s Text ()
enableHolder op =do
    case Map.keys utxos of
        []       -> do
            Contract.logError @String "no utxo found"
        oref : _ -> do 
            let val     = Value.singleton (computeNftSymbol oref ) (TokenName emptyByteString)  1
                lookups = Constraints.monetaryPolicy (policy oref) 
                            <> Constraints.unspentOutputs utxos
                tx      = Constraints.mustForgeValue val <> Constraints.mustSpendPubKeyOutput oref
                            <>mustPayToOtherScript valHash (Datum $ Constr 0 []) $ singleton (CurrencySymbol (policy oref)) (TokenName emptyByteString) 1
            ledgerTx <- submitTxConstraintsWith @Void lookups tx
            void $ awaitTxConfirmed $ tsxId ledgerTx
            Contract.logInfo @String $ printf "forged %s" (show val)
            pure $ Just (computeNftSymbol oref)
    pkh <- pubKeyHash <$> Contract.ownPubKey
    pure ()



 