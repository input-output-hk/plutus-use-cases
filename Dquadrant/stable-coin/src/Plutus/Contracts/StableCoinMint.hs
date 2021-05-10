{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

{-# LANGUAGE StandaloneDeriving #-}
module Plutus.Contracts.LendingPool
    ( 
    ) where

import           Control.Monad                    hiding (fmap)
import qualified Data.Map                         as Map
import           Data.Monoid                      (Last (..))
import           Data.Proxy                       (Proxy (..))
import           Data.Text                        (Text, pack)
import           Data.Void                        (Void)
import           Ledger                           hiding (singleton)
import           Ledger.Constraints               as Constraints
import           Ledger.Constraints.OnChain       as Constraints
import           Ledger.Constraints.TxConstraints as Constraints
import qualified Ledger.Scripts                   as Scripts
import qualified Ledger.Typed.Scripts             as Scripts
import           Ledger.Value                     as Value
import           Playground.Contract
import           Plutus.Contract                  hiding (when)

import qualified Plutus.Contracts.Currency        as Currency
import qualified PlutusTx
import           PlutusTx.Prelude                 hiding (Semigroup (..), unless)
import           Prelude                          (Semigroup (..))
import qualified Prelude
import           Text.Printf                      (printf)
import Plutus.V1.Ledger.Ada ( lovelaceValueOf )

{-# INLINABLE mkPolicy #-}
mkPolicy :: PubKeyHash -> ScriptContext -> Bool
mkPolicy pkh ctx = txSignedBy (scriptContextTxInfo ctx) pkh

policy :: PubKeyHash -> Scripts.MonetaryPolicy
policy pkh = mkMonetaryPolicyScript $
    $$(PlutusTx.compile [|| Scripts.wrapMonetaryPolicy . mkPolicy ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode pkh

curSymbol :: PubKeyHash -> CurrencySymbol
curSymbol = scriptCurrencySymbol . policy

data MintParams = MintParams
    { mpTokenName :: !TokenName
    , mpAmount    :: !Integer
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

type SignedSchema =
    BlockchainActions
        .\/ Endpoint "mint" MintParams

mint :: MintParams -> Contract w SignedSchema Text ()
mint mp = do
    pkh <- pubKeyHash <$> Contract.ownPubKey
    let val     = Value.singleton (curSymbol pkh) (mpTokenName mp) (mpAmount mp)
        lookups = Constraints.monetaryPolicy $ policy pkh
        tx      = Constraints.mustForgeValue val
    ledgerTx <- submitTxConstraintsWith @Void lookups tx
    void $ awaitTxConfirmed $ txId ledgerTx
    Contract.logInfo @String $ printf "forged %s" (show val)

endpoints :: Contract () SignedSchema Text ()
endpoints = mint' >> endpoints
  where
    mint' = endpoint @"mint" >>= mint

mkSchemaDefinitions ''SignedSchema

mkKnownCurrencies []



