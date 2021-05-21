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

module Plutus.State.Update where

import           Control.Monad                    hiding (fmap)
import qualified Data.ByteString                  as BS
import qualified Data.Map                         as Map
import           Data.Text                        (Text, pack)
import qualified Data.Text                        as Text
import           Data.Void                        (Void)
import           Ledger                           hiding (getDatum, singleton)
import           Ledger.Constraints               as Constraints
import           Ledger.Constraints.OnChain       as Constraints
import           Ledger.Constraints.TxConstraints as Constraints
import qualified Ledger.Scripts                   as Scripts
import           Ledger.Typed.Scripts             (ScriptType (..))
import qualified Ledger.Typed.Scripts             as Scripts

import           Playground.Contract
import           Plutus.Contract                  hiding (when)
import qualified Plutus.Contracts.TxUtils         as TxUtils
import           Plutus.OutputValue               (OutputValue (..))
import           Plutus.V1.Ledger.Value
import           PlutusTx                         (IsData)
import qualified PlutusTx
import           PlutusTx.Prelude                 hiding (Semigroup (..),
                                                   unless)
import           Prelude                          (Semigroup (..))
import qualified Prelude

type OwnerToken = AssetClass

-- State token can be only be forged when there is an input containing an owner token
{-# INLINABLE validateStateForging #-}
validateStateForging :: OwnerToken -> ScriptContext -> Bool
validateStateForging ownerToken ctx =
    any hasOwnerToken inputValues || traceError "State forging without OwnerToken input"
  where
    inputs = txInfoInputs (scriptContextTxInfo ctx)
    inputValues = txOutValue . txInInfoResolved <$> inputs
    hasOwnerToken value = assetClassValueOf value ownerToken == 1

makeStatePolicy :: OwnerToken -> MonetaryPolicy
makeStatePolicy ownerToken = mkMonetaryPolicyScript $
    $$(PlutusTx.compile [|| Scripts.wrapMonetaryPolicy . validateStateForging ||])
        `PlutusTx.applyCode` PlutusTx.liftCode ownerToken

makeStateCurrency :: OwnerToken -> CurrencySymbol
makeStateCurrency = scriptCurrencySymbol . makeStatePolicy

makeStateToken :: OwnerToken -> TokenName -> AssetClass
makeStateToken ownerToken = assetClass (makeStateCurrency ownerToken)

data PutStateHandle scriptType = PutStateHandle {
    script           :: Scripts.ScriptInstance scriptType,
    ownerToken       :: AssetClass,
    ownerTokenOutput :: OutputValue (DatumType scriptType)
}

data StateHandle scriptType a = StateHandle {
    stateToken :: AssetClass,
    toDatum    :: a -> DatumType scriptType,
    toRedeemer :: a -> RedeemerType scriptType
}

putState ::
    (HasBlockchainActions s, IsData (DatumType scriptType), IsData (RedeemerType scriptType)) =>
    PutStateHandle scriptType ->
    StateHandle scriptType a ->
    a ->
    Contract w s Text ()
putState PutStateHandle {..} StateHandle{..} newState = do
    pkh <- pubKeyHash <$> ownPubKey
    ledgerTx <- TxUtils.submitTxPair $
        TxUtils.mustForgeValue (makeStatePolicy ownerToken) (assetClassValue stateToken 1)
        <> TxUtils.mustPayToScript script pkh (toDatum newState) (assetClassValue stateToken 1)
        <> TxUtils.mustRoundTripToScript
            script
            [toRedeemer newState Prelude.<$ ownerTokenOutput]
            (ovValue ownerTokenOutput)
            pkh
            (assetClassValue ownerToken 1)
    _ <- awaitTxConfirmed $ txId ledgerTx
    pure ()

updateState ::
    (HasBlockchainActions s, IsData (DatumType scriptType), IsData (RedeemerType scriptType)) =>
    Scripts.ScriptInstance scriptType ->
    StateHandle scriptType a ->
    OutputValue a ->
    Contract w s Text ()
updateState script StateHandle{..} output = do
    pkh <- pubKeyHash <$> ownPubKey
    ledgerTx <- TxUtils.submitTxPair $
        TxUtils.mustRoundTripToScript
            script
            [toRedeemer Prelude.<$> output]
            (toDatum . ovValue $ output)
            pkh
            (assetClassValue stateToken 1)
    _ <- awaitTxConfirmed $ txId ledgerTx
    pure ()
