{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module Plutus.Abstract.State.Update where

import           Control.Monad                    hiding (fmap)
import qualified Data.ByteString                  as BS
import qualified Data.Map                         as Map
import           Data.Text                        (Text, pack)
import qualified Data.Text                        as Text
import           Data.Void                        (Void)
import           Ext.Plutus.Ledger.Contexts       (scriptInputsAt)
import           Ledger                           hiding (getDatum, singleton)
import           Ledger.Constraints               as Constraints
import           Ledger.Constraints.OnChain       as Constraints
import           Ledger.Constraints.TxConstraints as Constraints
import qualified Ledger.Scripts                   as UntypedScripts
import           Ledger.Typed.Scripts             (DatumType, RedeemerType)
import qualified Ledger.Typed.Scripts             as Scripts
import           Playground.Contract
import           Plutus.Abstract.OutputValue      (OutputValue (..))
import qualified Plutus.Abstract.TxUtils          as TxUtils
import           Plutus.Contract                  hiding (when)
import           Plutus.V1.Ledger.Value
import           PlutusTx                         (IsData)
import qualified PlutusTx
import           PlutusTx.Prelude                 hiding (Semigroup (..),
                                                   unless)
import           Prelude                          (Semigroup (..))
import qualified Prelude

type OwnerToken = AssetClass

-- State token can be only be forged when there is an input and output containing an owner token belonging to a script
{-# INLINABLE validateStateForging #-}
validateStateForging :: ValidatorHash -> OwnerToken -> TokenName -> ScriptContext -> Bool
validateStateForging ownerScript ownerToken tokenName ctx = traceIfFalse "State forging not authorized" $
    hasOneOwnerToken outputValues && hasOneOwnerToken inputValues && hasOneStateToken forgedValue && hasOneStateToken (mconcat outputValues)
  where
    txInfo = scriptContextTxInfo ctx
    stateToken = assetClass (ownCurrencySymbol ctx) tokenName

    outputValues = snd <$> scriptOutputsAt ownerScript txInfo
    inputValues = snd <$> scriptInputsAt ownerScript txInfo
    forgedValue = txInfoForge txInfo

    hasOneOwnerToken values = assetClassValueOf (mconcat values) ownerToken == 1
    hasOneStateToken value = assetClassValueOf value stateToken == 1

makeStatePolicy :: ValidatorHash -> OwnerToken -> TokenName -> MonetaryPolicy
makeStatePolicy ownerScript ownerToken tokenName = mkMonetaryPolicyScript $
    $$(PlutusTx.compile [|| \os ot tn -> Scripts.wrapMonetaryPolicy $ validateStateForging os ot tn||])
        `PlutusTx.applyCode` PlutusTx.liftCode ownerScript
        `PlutusTx.applyCode` PlutusTx.liftCode ownerToken
        `PlutusTx.applyCode` PlutusTx.liftCode tokenName

makeStateCurrency :: ValidatorHash -> OwnerToken -> TokenName -> CurrencySymbol
makeStateCurrency ownerScript ownerToken tokenName = scriptCurrencySymbol $ makeStatePolicy ownerScript ownerToken tokenName

makeStateToken :: ValidatorHash -> OwnerToken -> TokenName -> AssetClass
makeStateToken ownerScript ownerToken tokenName = assetClass (makeStateCurrency ownerScript ownerToken tokenName) tokenName

data PutStateHandle scriptType = PutStateHandle {
    script           :: Scripts.TypedValidator scriptType,
    ownerToken       :: AssetClass,
    ownerTokenOutput :: OutputValue (DatumType scriptType)
}

data StateHandle scriptType a = StateHandle {
    stateToken :: AssetClass,
    toDatum    :: a -> DatumType scriptType,
    toRedeemer :: a -> RedeemerType scriptType
}

putState ::
    (IsData (DatumType scriptType), IsData (RedeemerType scriptType)) =>
    PutStateHandle scriptType ->
    StateHandle scriptType a ->
    a ->
    Contract w s Text (TxUtils.TxPair scriptType)
putState PutStateHandle {..} StateHandle{..} newState = do
    pkh <- pubKeyHash <$> ownPubKey
    let (_, stateTokenName) = unAssetClass stateToken
    pure $
        TxUtils.mustForgeValue (makeStatePolicy (Scripts.validatorHash script) ownerToken stateTokenName) (assetClassValue stateToken 1)
        <> TxUtils.mustPayToScript script pkh (toDatum newState) (assetClassValue stateToken 1)
        <> TxUtils.mustRoundTripToScript
            script
            [toRedeemer newState Prelude.<$ ownerTokenOutput]
            (ovValue ownerTokenOutput)
            pkh
            (assetClassValue ownerToken 1)

updateState ::
    (IsData (DatumType scriptType), IsData (RedeemerType scriptType)) =>
    Scripts.TypedValidator scriptType ->
    StateHandle scriptType a ->
    OutputValue a ->
    Contract w s Text (TxUtils.TxPair scriptType, a)
updateState script StateHandle{..} output = do
    pkh <- pubKeyHash <$> ownPubKey
    let tx = TxUtils.mustRoundTripToScript
                script
                [toRedeemer Prelude.<$> output]
                (toDatum . ovValue $ output)
                pkh
                (assetClassValue stateToken 1)
    pure (tx, ovValue output)
