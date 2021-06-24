{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}

module Fixtures.Aave where

import           Control.Monad              (void)
import           Data.Text                  (Text)
import           Data.Void                  (Void)
import qualified Ledger
import qualified Ledger.Constraints         as Constraints
import           Ledger.Typed.Scripts       (MonetaryPolicy)
import qualified Ledger.Typed.Scripts       as Scripts
import           Plutus.Contract
import qualified Plutus.Contracts.Core      as Aave
import qualified Plutus.Contracts.Endpoints as Aave
import qualified Plutus.Contracts.TxUtils   as TxUtils
import           Plutus.V1.Ledger.Contexts  (ScriptContext)
import qualified Plutus.V1.Ledger.Scripts   as Scripts
import           Plutus.V1.Ledger.Value     (CurrencySymbol, TokenName,
                                             assetClass, assetClassValue)
import qualified PlutusTx
import           PlutusTx.Prelude
import qualified Prelude

{-# INLINABLE validator #-}
validator :: TokenName -> ScriptContext -> Bool
validator _ _ = True

makeLiquidityPolicy :: TokenName -> MonetaryPolicy
makeLiquidityPolicy tokenName = Scripts.mkMonetaryPolicyScript $
  $$(PlutusTx.compile [|| Scripts.wrapMonetaryPolicy . validator ||])
    `PlutusTx.applyCode`
        PlutusTx.liftCode tokenName

aavePolicy :: MonetaryPolicy
aavePolicy = makeLiquidityPolicy Aave.aaveProtocolName

aaveSymbol :: CurrencySymbol
aaveSymbol = Ledger.scriptCurrencySymbol aavePolicy

aaveAddress :: Ledger.Address
aaveAddress = Aave.aaveAddress . Aave.aave $ aaveSymbol

aave :: Aave.Aave
aave = Aave.aave aaveSymbol

aaveHash :: Ledger.ValidatorHash
aaveHash = Aave.aaveHash aave

start :: [Aave.CreateParams] -> Contract () Aave.AaveOwnerSchema Text Aave.Aave
start = Aave.start' $ do
    pkh <- Ledger.pubKeyHash <$> ownPubKey
    let forgeValue = assetClassValue (assetClass aaveSymbol Aave.aaveProtocolName) 1
    ledgerTx <-
        TxUtils.submitTxPair $
            TxUtils.mustForgeValue @Void aavePolicy forgeValue
            Prelude.<> (Prelude.mempty, Constraints.mustPayToPubKey pkh forgeValue)
    void $ awaitTxConfirmed $ Ledger.txId ledgerTx
    pure aaveSymbol
