{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Plutus.AbstractTestnetMVP.TxUtils where

import           Control.Lens                           (review)
import           Control.Monad                          (void)
import           Data.ByteString                        (ByteString)
import qualified Data.Map                               as Map
import           Data.Text                              (Text)
import           Data.Void                              (Void)
import           Ledger                                 hiding (singleton)
import qualified Ledger.Constraints                     as Constraints
import qualified Ledger.Constraints.OnChain             as Constraints
import qualified Ledger.Constraints.TxConstraints       as Constraints
import           Ledger.Typed.Scripts                   (DatumType,
                                                         MintingPolicy,
                                                         RedeemerType,
                                                         TypedValidator)
import qualified Ledger.Typed.Scripts                   as Scripts
import           Plutus.AbstractTestnetMVP.OutputValue            (OutputValue (..))
import           Plutus.Contract
import qualified Plutus.V1.Ledger.Scripts               as Scripts
import           Plutus.V1.Ledger.Value                 (AssetClass (unAssetClass),
                                                         TokenName (..),
                                                         assetClass,
                                                         assetClassValue,
                                                         assetClassValueOf)
import qualified PlutusTx
import           PlutusTx.Prelude                       hiding (Semigroup (..))
import           Prelude                                (Semigroup (..))
import qualified Prelude

type TxPair a = (Constraints.ScriptLookups a, Constraints.TxConstraints (RedeemerType a) (DatumType a))

type IsScriptData a = (
    PlutusTx.FromData (RedeemerType a), PlutusTx.ToData (RedeemerType a),
    PlutusTx.FromData (DatumType a), PlutusTx.ToData (DatumType a))

submitTxPair :: (AsContractError e, IsScriptData a) =>
    TxPair a
    -> Contract w s e CardanoTx
submitTxPair = Prelude.uncurry submitTxConstraintsWith

mustForgeValue :: (IsScriptData a) =>
    MintingPolicy
    -> Value
    -> TxPair a
mustForgeValue policy value = (lookups, tx)
    where
        lookups = Constraints.mintingPolicy policy
        tx = Constraints.mustMintValue value

mustPayToScript :: (IsScriptData a) =>
  TypedValidator a
  -> PubKeyHash
  -> DatumType a
  -> Value
  -> TxPair a
mustPayToScript script pkh datum value = (lookups, tx)
    where
        lookups = Constraints.ownPubKeyHash pkh <> Constraints.typedValidatorLookups script
        tx = Constraints.mustPayToTheScript datum value

mustSpendScriptOutputs :: (IsScriptData a) =>
    TypedValidator a
    -> [OutputValue (RedeemerType a)]
    -> TxPair a
mustSpendScriptOutputs script inputs = (lookups, tx)
    where
        unspent = Map.fromList $ fmap (\(OutputValue ref tx _) -> (ref, tx)) inputs
        lookups = Constraints.otherScript (Scripts.validatorScript script) <> Constraints.unspentOutputs unspent
        tx = Prelude.mconcat $
            fmap (\(OutputValue ref _ redeemer) -> Constraints.mustSpendScriptOutput ref (Redeemer $ PlutusTx.toBuiltinData redeemer)) inputs

mustSpendFromScript :: (IsScriptData a) =>
  TypedValidator a
  -> [OutputValue (RedeemerType a)]
  -> PubKeyHash
  -> Value
  -> TxPair a
mustSpendFromScript script inputs pkh value = (lookups, tx) <> mustSpendScriptOutputs script inputs
    where
        lookups = Constraints.ownPubKeyHash pkh
        tx = Constraints.mustPayToPubKey pkh value

mustRoundTripToScript :: (IsScriptData a) =>
  TypedValidator a
  -> [OutputValue (RedeemerType a)]
  -> DatumType a
  -> PubKeyHash
  -> Value
  -> TxPair a
mustRoundTripToScript script inputs datum pkh value = mustSpendScriptOutputs script inputs <> mustPayToScript script pkh datum value
