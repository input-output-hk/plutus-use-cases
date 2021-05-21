{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Plutus.Contracts.TxUtils where

import           Control.Monad                    (void)
import           Data.ByteString                  (ByteString)
import qualified Data.Map                         as Map
import           Data.Text                        (Text)
import           Data.Void                        (Void)
import           Ledger                           hiding (singleton)
import qualified Ledger.Constraints               as Constraints
import qualified Ledger.Constraints.OnChain       as Constraints
import qualified Ledger.Constraints.TxConstraints as Constraints
import           Ledger.Typed.Scripts             (MonetaryPolicy,
                                                   ScriptInstance,
                                                   ScriptType (..))
import qualified Ledger.Typed.Scripts             as Scripts
import           Plutus.Contract
import           Plutus.Contracts.Core            (Aave, Reserve (..))
import qualified Plutus.Contracts.Core            as Core
import qualified Plutus.Contracts.FungibleToken   as FungibleToken
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

submitTxPair :: (AsContractError e, HasWriteTx s, PlutusTx.IsData (RedeemerType a), PlutusTx.IsData (DatumType a)) =>
    TxPair a
    -> Contract w s e Tx
submitTxPair (lookups, tx) = submitTxConstraintsWith lookups tx

mustForgeValue :: (PlutusTx.IsData (RedeemerType a), PlutusTx.IsData (DatumType a)) =>
    MonetaryPolicy
    -> Value
    -> TxPair a
mustForgeValue policy value = (lookups, tx)
    where
        lookups = Constraints.monetaryPolicy policy
        tx = Constraints.mustForgeValue value

mustPayToScript :: (PlutusTx.IsData (RedeemerType a), PlutusTx.IsData (DatumType a)) =>
  ScriptInstance a
  -> PubKeyHash
  -> DatumType a
  -> Value
  -> TxPair a
mustPayToScript script pkh datum value = (lookups, tx)
    where
        lookups = Constraints.ownPubKeyHash pkh <> Constraints.scriptInstanceLookups script
        tx = Constraints.mustPayToTheScript datum value

data StateInput a = StateInput {
    txOutRef :: TxOutRef,
    txOutTx  :: TxOutTx,
    redeemer :: a
}

mustSpendScriptOutputs :: (PlutusTx.IsData (RedeemerType a), PlutusTx.IsData (DatumType a)) =>
    ScriptInstance a
    -> [StateInput (RedeemerType a)]
    -> TxPair a
mustSpendScriptOutputs script hmm = (lookups, tx)
    where
        unspent = Map.fromList $ fmap (\(StateInput ref tx _) -> (ref, tx)) hmm
        lookups = Constraints.otherScript (Scripts.validatorScript script) <> Constraints.unspentOutputs unspent
        tx = Prelude.mconcat $
            fmap (\(StateInput ref _ redeemer) -> Constraints.mustSpendScriptOutput ref (Redeemer $ PlutusTx.toData redeemer)) hmm

mustSpendFromScript :: (PlutusTx.IsData (RedeemerType a), PlutusTx.IsData (DatumType a)) =>
  ScriptInstance a
  -> [StateInput (RedeemerType a)]
  -> PubKeyHash
  -> Value
  -> TxPair a
mustSpendFromScript script hmm pkh value = (lookups, tx) <> mustSpendScriptOutputs script hmm
    where
        lookups = Constraints.ownPubKeyHash pkh
        tx = Constraints.mustPayToPubKey pkh value

mustRoundTripToScript :: (PlutusTx.IsData (RedeemerType a), PlutusTx.IsData (DatumType a)) =>
  ScriptInstance a
  -> [StateInput (RedeemerType a)]
  -> DatumType a
  -> PubKeyHash
  -> Value
  -> TxPair a
mustRoundTripToScript script hmm datum pkh value = mustSpendScriptOutputs script hmm <> mustPayToScript script pkh datum value
