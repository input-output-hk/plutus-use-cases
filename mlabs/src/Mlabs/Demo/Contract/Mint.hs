{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}

module Mlabs.Demo.Contract.Mint
  ( curPolicy
  , curSymbol
  , mintContract
  , mintEndpoints
  , MintParams (..)
  , MintSchema
  ) where
      
import PlutusTx.Prelude hiding (Monoid(..), Semigroup(..), null)

import Plutus.Contract as Contract
import qualified Ledger as Ledger
import qualified Ledger.Ada as Ada
import qualified Ledger.Constraints as Constraints
import Ledger.Contexts
import Ledger.Scripts
import qualified Ledger.Typed.Scripts as Scripts
import Ledger.Value (CurrencySymbol, TokenName)
import qualified Ledger.Value as Value
import qualified PlutusTx as PlutusTx

import Control.Monad
import Data.Aeson (FromJSON, ToJSON)
import Data.Text hiding (all, filter, foldr)
import Data.Void
import GHC.Generics (Generic)
import Prelude (Semigroup(..))
import Schema (ToSchema)

import Mlabs.Demo.Contract.Burn

------------------------------------------------------------------------------
-- On-chain code.

{-# INLINABLE mkPolicy #-}
-- | A monetary policy that mints arbitrary tokens for an equal amount of Ada.
-- For simplicity, the Ada are sent to a burn address.
mkPolicy :: Ledger.Address -> ScriptContext -> Bool
mkPolicy burnAddr ctx =
  traceIfFalse "Insufficient Ada paid" isPaid
    && traceIfFalse "Forged amount is invalid" isForgeValid
 where
  txInfo :: TxInfo
  txInfo = scriptContextTxInfo ctx

  outputs :: [TxOut]
  outputs = txInfoOutputs txInfo

  forged :: [(CurrencySymbol, TokenName, Integer)]
  forged = Value.flattenValue $ txInfoForge txInfo

  forgedQty :: Integer
  forgedQty = foldr (\(_, _, amt) acc -> acc + amt) 0 forged

  isToBurnAddr :: TxOut -> Bool
  isToBurnAddr o = txOutAddress o == burnAddr

  isPaid :: Bool
  isPaid =
    let
      adaVal =
        Ada.fromValue $ mconcat $ txOutValue <$> filter isToBurnAddr outputs
    in Ada.getLovelace adaVal >= forgedQty * tokenToLovelaceXR

  isForgeValid :: Bool
  isForgeValid = all isValid forged
    where isValid (_, _, amt) = amt > 0


curPolicy :: MonetaryPolicy
curPolicy = mkMonetaryPolicyScript $
  $$(PlutusTx.compile [|| Scripts.wrapMonetaryPolicy . mkPolicy ||])
    `PlutusTx.applyCode` PlutusTx.liftCode burnScrAddress

curSymbol :: CurrencySymbol
curSymbol = Ledger.scriptCurrencySymbol curPolicy

-- For demo purposes, all tokens will be minted for a price of 1 Ada.
tokenToLovelaceXR :: Integer
tokenToLovelaceXR = 1_000_000

------------------------------------------------------------------------------
-- Off-chain code.

data MintParams = MintParams
  { mpTokenName :: !TokenName
  , mpAmount    :: !Integer
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

type MintSchema = 
  BlockchainActions
    .\/ Endpoint "mint" MintParams

-- | Generates tokens with the specified name/amount and burns an equal amount of Ada.
mintContract :: MintParams -> Contract w MintSchema Text ()
mintContract mp = do
  let
    tn       = mp.mpTokenName
    amt      = mp.mpAmount
    payVal   = Ada.lovelaceValueOf $ amt * tokenToLovelaceXR
    forgeVal = Value.singleton curSymbol tn amt
    lookups  = Constraints.monetaryPolicy curPolicy
    tx =
      Constraints.mustPayToOtherScript
          burnValHash
          (Datum $ PlutusTx.toData ())
          payVal
        <> Constraints.mustForgeValue forgeVal
  ledgerTx <- submitTxConstraintsWith @Void lookups tx
  void $ awaitTxConfirmed $ Ledger.txId ledgerTx

mintEndpoints :: Contract () MintSchema Text ()
mintEndpoints = mint >> mintEndpoints where mint = endpoint @"mint" >>= mintContract
