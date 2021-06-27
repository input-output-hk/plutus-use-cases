{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}

module Fixtures.Aave where

import           Control.Monad              (void)
import           Data.Text                  (Text)
import           Data.Void                  (Void)
import           Fixtures.Symbol            (forgeSymbol, getSymbol)
import qualified Ledger
import qualified Ledger.Constraints         as Constraints
import           Ledger.Typed.Scripts       (MonetaryPolicy)
import           Plutus.Contract
import qualified Plutus.Contracts.Core      as Aave
import qualified Plutus.Contracts.Endpoints as Aave
import qualified Plutus.Contracts.TxUtils   as TxUtils
import           Plutus.V1.Ledger.Value     (CurrencySymbol, TokenName,
                                             assetClass, assetClassValue)
import qualified PlutusTx
import           PlutusTx.Prelude
import qualified Prelude

aaveSymbol :: CurrencySymbol
aaveSymbol = getSymbol Aave.aaveProtocolName

aaveAddress :: Ledger.Address
aaveAddress = Aave.aaveAddress . Aave.aave $ aaveSymbol

aave :: Aave.Aave
aave = Aave.aave aaveSymbol

aaveHash :: Ledger.ValidatorHash
aaveHash = Aave.aaveHash aave

start :: [Aave.CreateParams] -> Contract () Aave.AaveOwnerSchema Text Aave.Aave
start = Aave.start' (forgeSymbol Aave.aaveProtocolName)
