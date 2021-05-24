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
{-# LANGUAGE TypeFamilies #-}
module Plutus.Contract.Wallet.NFT

where
import           Control.Lens
import           PlutusTx.Prelude        hiding (Monoid (..), Semigroup (..))

import           Plutus.Contract         as Contract

import           Ledger                  (CurrencySymbol, PubKeyHash, TxId, TxOutRef (..), pubKeyHash,
                                          scriptCurrencySymbol, txId)
import qualified Ledger.Ada              as Ada
import qualified Ledger.Constraints      as Constraints
import qualified Ledger.Contexts         as V
import           Ledger.Scripts
import qualified PlutusTx

import qualified Ledger.Typed.Scripts    as Scripts
import           Ledger.Value            (AssetClass, TokenName, Value)
import qualified Ledger.Value            as Value

import           Data.Aeson              (FromJSON, ToJSON)
import           Data.Semigroup          (Last (..))
import           GHC.Generics            (Generic)
import qualified PlutusTx.AssocMap       as AssocMap
import           Prelude                 (Semigroup (..))
import qualified Prelude
import           Schema                  (ToSchema)
import           Ledger.Contexts          (ScriptContext (..), TxInfo (..))
import           Plutus.Contract.Blockchain.NFT
 
