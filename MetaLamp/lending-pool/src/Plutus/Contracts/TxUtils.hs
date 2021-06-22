{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Plutus.Contracts.TxUtils where

import           Control.Lens                    (review)
import           Control.Monad                    (void)
import           Data.ByteString                  (ByteString)
import qualified Data.Map                         as Map
import           Data.Text                        (Text)
import           Data.Void                        (Void)
import           Ledger                           hiding (singleton)
import qualified Ledger.Constraints               as Constraints
import qualified Ledger.Constraints.OnChain       as Constraints
import qualified Ledger.Constraints.TxConstraints as Constraints
import           Ledger.Typed.Scripts             (DatumType, MonetaryPolicy,
                                                   RedeemerType, TypedValidator)
import qualified Ledger.Typed.Scripts             as Scripts
import           Plutus.Contract
import           Plutus.Contracts.Core            (Aave, Reserve (..))
import qualified Plutus.Contracts.Core            as Core
import qualified Plutus.Contracts.FungibleToken   as FungibleToken
import           Plutus.OutputValue               (OutputValue (..))
import           Plutus.V1.Ledger.Contexts        (ScriptContext,
                                                   scriptCurrencySymbol)
import qualified Plutus.V1.Ledger.Scripts         as Scripts
import           Plutus.V1.Ledger.Value           (AssetClass (unAssetClass),
                                                   TokenName (..), assetClass,
                                                   assetClassValue,
                                                   assetClassValueOf)
import qualified PlutusTx
import           PlutusTx.Prelude                 hiding (Semigroup (..))
import           Prelude                          (Semigroup (..))
import qualified Prelude

type TxPair a = (Constraints.ScriptLookups a, Constraints.TxConstraints (RedeemerType a) (DatumType a))

type IsScriptData a = (PlutusTx.IsData (RedeemerType a), PlutusTx.IsData (DatumType a))

submitTxPair :: (AsContractError e, HasWriteTx s, IsScriptData a) =>
    TxPair a
    -> Contract w s e Tx
submitTxPair = Prelude.uncurry submitTxConstraintsWith

concatTxPairs :: (IsScriptData a, IsScriptData b) => TxPair a -> TxPair b -> Either Constraints.MkTxError Constraints.UnbalancedTx
concatTxPairs (lookupsA, txA) (lookupsB, txB) =
    Constraints.mkSomeTx [Constraints.SomeLookupsAndConstraints lookupsA txA, Constraints.SomeLookupsAndConstraints lookupsB txB]

submitRawUnbalancedTx
  :: forall w s e.
  ( HasWriteTx s
  , AsContractError e
  )
  => Either Constraints.MkTxError Constraints.UnbalancedTx
  -> Contract w s e Tx
submitRawUnbalancedTx rawUnbalancedTx = do
  tx <- either (throwError . review _ConstraintResolutionError) pure rawUnbalancedTx
  submitUnbalancedTx tx

mustForgeValue :: (IsScriptData a) =>
    MonetaryPolicy
    -> Value
    -> TxPair a
mustForgeValue policy value = (lookups, tx)
    where
        lookups = Constraints.monetaryPolicy policy
        tx = Constraints.mustForgeValue value

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
            fmap (\(OutputValue ref _ redeemer) -> Constraints.mustSpendScriptOutput ref (Redeemer $ PlutusTx.toData redeemer)) inputs

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
