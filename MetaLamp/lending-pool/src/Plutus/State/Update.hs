{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
import           Plutus.State.Select              (StateOutput (..))
import           Plutus.V1.Ledger.Value
import           PlutusTx                         (IsData)
import qualified PlutusTx
import           PlutusTx.Prelude                 hiding (Semigroup (..),
                                                   unless)
import           Prelude                          (Semigroup (..))
import qualified Prelude

type RootToken = AssetClass

-- State token can be only be forged either when input has a root token(first-time creation)
-- or any of the state tokens(modification), assuming that state token was created with a root token at some point
validateStateForging :: RootToken -> TokenName -> ScriptContext -> Bool
validateStateForging rootToken tn ctx = case [ i
                                          | i <- txInfoInputs $ scriptContextTxInfo ctx
                                          , let v = valueWithin i
                                          , (assetClassValueOf v rootToken == 1) ||
                                            (assetClassValueOf v stateToken == 1)
                                          ] of
    [_]    -> True
    [_, _] -> True
    _      -> traceError "State forging without RootToken input"
  where
    stateToken = assetClass (ownCurrencySymbol ctx) tn
    valueWithin = txOutValue . txInInfoResolved

makeStatePolicy :: RootToken -> TokenName -> MonetaryPolicy
makeStatePolicy rootToken tokenName = mkMonetaryPolicyScript $
    $$(PlutusTx.compile [|| \r t -> Scripts.wrapMonetaryPolicy (validateStateForging r t) ||])
        `PlutusTx.applyCode` PlutusTx.liftCode rootToken
        `PlutusTx.applyCode` PlutusTx.liftCode tokenName

makeStateCurrency :: RootToken -> TokenName -> CurrencySymbol
makeStateCurrency rootToken = scriptCurrencySymbol . makeStatePolicy rootToken

makeStateToken :: RootToken -> TokenName -> AssetClass
makeStateToken rootToken tokenName = assetClass (makeStateCurrency rootToken tokenName) tokenName

data RootStateHandle a b = RootStateHandle {
    script    :: Scripts.ScriptInstance a,
    rootToken :: AssetClass,
    output    :: StateOutput b
}

data StateHandle i o a = StateHandle {
    stateToken :: AssetClass,
    toDatum    :: a -> o,
    toRedeemer :: a -> i
}

putState ::
    (HasBlockchainActions s, IsData (DatumType o), IsData (RedeemerType o)) =>
    RootStateHandle o (DatumType o) ->
    StateHandle (RedeemerType o) (DatumType o) a ->
    a ->
    Contract w s Text a
putState RootStateHandle {..} StateHandle{..} newState = do
    let oref = soOutRef output
        otx = soOutTx output
        lookups = Constraints.scriptInstanceLookups script
            <> Constraints.monetaryPolicy (makeStatePolicy rootToken (Prelude.snd . unAssetClass $ stateToken))
            <> Constraints.otherScript (Scripts.validatorScript script)
            <> Constraints.unspentOutputs (Map.singleton oref otx)
        tx = mustForgeValue (assetClassValue stateToken 1)
            <> Constraints.mustPayToTheScript (toDatum newState) (assetClassValue stateToken 1)
            <> Constraints.mustPayToTheScript (soDatum output) (assetClassValue rootToken 1)
            <> Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toData $ toRedeemer newState)
    ledgerTx <- submitTxConstraintsWith lookups tx
    _ <- awaitTxConfirmed $ txId ledgerTx
    pure newState

updateState ::
    (HasBlockchainActions s, IsData (DatumType o), IsData (RedeemerType o)) =>
    RootStateHandle o (DatumType o) ->
    StateHandle (RedeemerType o) (DatumType o) a ->
    StateOutput a ->
    Contract w s Text a
updateState RootStateHandle{..} StateHandle{..} (StateOutput oref o datum) = do
    let lookups = Constraints.scriptInstanceLookups script
            <> Constraints.otherScript (Scripts.validatorScript script)
            <> Constraints.unspentOutputs (Map.singleton oref o)
        tx = Constraints.mustPayToTheScript (toDatum datum) (assetClassValue stateToken 1)
            <> Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toData (toRedeemer datum))
    ledgerTx <- submitTxConstraintsWith lookups tx
    _ <- awaitTxConfirmed $ txId ledgerTx
    pure datum
