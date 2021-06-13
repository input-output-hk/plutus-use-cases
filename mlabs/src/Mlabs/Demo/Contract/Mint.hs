{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ViewPatterns          #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}

module Mlabs.Demo.Contract.Mint
  ( curPolicy
  , getCurrencySymbol
  , MintParams (..)
  , MintSchema
  , mintContract
  , mintEndpoints
  , swapEndpoints
  ) where
      
import PlutusTx.Prelude hiding (Monoid(..), Semigroup(..))

import Plutus.Contract as Contract
import qualified Ledger as Ledger
import qualified Ledger.Ada as Ada
import qualified Ledger.Constraints as Constraints
import qualified Ledger.Contexts as V
import Ledger.Scripts
import qualified Ledger.Typed.Scripts as Scripts
import Ledger.Value (TokenName, Value)
import qualified Ledger.Value as Value
import qualified PlutusTx as PlutusTx

import Control.Monad
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Map as Map
import Data.Semigroup (Last(..))
import Data.Text
import Data.Void
import GHC.Generics (Generic)
import qualified PlutusTx.AssocMap as AssocMap
import Prelude (Semigroup(..))
import qualified Prelude as Haskell
import Schema (ToSchema)
import Text.Printf


-------------------------------------------------------------------------------
-- Swap script
-------------------------------------------------------------------------------

data SwapRedeemer = Deposit TokenName | Swap TokenName

PlutusTx.unstableMakeIsData ''SwapRedeemer

{-# INLINABLE mkSwapValidator #-}
mkSwapValidator :: () -> SwapRedeemer -> V.ScriptContext -> Bool
mkSwapValidator _ _ ctx = True
--  where
--   txInfo :: V.TxInfo
--   txInfo = V.scriptContextTxInfo ctx

--   ownSymbol :: Value.CurrencySymbol
--   ownSymbol = V.ownCurrencySymbol ctx

--   expectedForged :: Value
--   expectedForged = Value.singleton ownSymbol (tokenName md) (amount md + 10)

--   forged :: Value
--   forged = V.txInfoForge txInfo

--  where
--   ownInput :: V.TxOut
--   ownInput = case V.findOwnInput ctx of
--     Nothing -> traceError "input is missing"
--     Just i  -> V.txInInfoResolved i

--     feesPaid :: Bool
--     feesPaid = V.txOutValue ownInput == forged

data Swapping
instance Scripts.ScriptType Swapping where
  type DatumType Swapping = ()
  type RedeemerType Swapping = SwapRedeemer


swapInst :: Scripts.ScriptInstance Swapping
swapInst = Scripts.validator @Swapping
    $$(PlutusTx.compile [|| mkSwapValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @() @SwapRedeemer

swapValidator :: Validator
swapValidator = Scripts.validatorScript swapInst

swapValHash :: Ledger.ValidatorHash
swapValHash = validatorHash swapValidator

swapScrAddress :: Ledger.Address
swapScrAddress = Scripts.scriptAddress swapInst

data SwapParams = SwapParams
  { spBuyTokenName  :: !TokenName
  , spSellTokenName :: !TokenName
  , spAmount        :: !Integer
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)


type SwapSchema = 
  BlockchainActions
    .\/ Endpoint "swap" SwapParams

-- | Exchanges the specified amount of tokens at a 1 to 1 exchange rate.
swapContract :: SwapParams -> Contract w SwapSchema Text ()
swapContract sp = do
  pkh <- V.pubKeyHash <$> ownPubKey
  let
    buyTn   = spBuyTokenName sp
    sellTn  = spSellTokenName sp
    amt     = spAmount sp
    buyCs   = getCurrencySymbol buyTn amt
    sellCs  = getCurrencySymbol sellTn amt
    buyVal  = getVal buyCs buyTn amt
    sellVal = getVal sellCs sellTn amt
    tx =
      Constraints.mustPayToTheScript () sellVal
        <> Constraints.mustPayToPubKey pkh buyVal
  ledgerTx <- submitTxConstraints swapInst tx
  void $ awaitTxConfirmed $ Ledger.txId ledgerTx
 where
  getVal cs tn amt
    | cs == Ada.adaSymbol = Ada.lovelaceValueOf amt
    | otherwise           = Value.singleton cs tn amt


swapEndpoints :: Contract () SwapSchema Text ()
swapEndpoints = mint >> swapEndpoints where mint = endpoint @"swap" >>= swapContract


-------------------------------------------------------------------------------
-- Minting script
-------------------------------------------------------------------------------

{-# INLINABLE mkCurPolicy #-}
mkCurPolicy :: TokenName -> Integer -> V.ScriptContext -> Bool
mkCurPolicy tn amt ctx = traceIfFalse
  "Value forged different from expected"
  (expectedForged == forged)
 where
  txInfo :: V.TxInfo
  txInfo = V.scriptContextTxInfo ctx

  ownSymbol :: Value.CurrencySymbol
  ownSymbol = V.ownCurrencySymbol ctx

  expectedForged :: Value
  expectedForged = Value.singleton ownSymbol tn amt

  forged :: Value
  forged = V.txInfoForge txInfo


curPolicy :: TokenName -> Integer -> MonetaryPolicy
curPolicy tn amt = mkMonetaryPolicyScript $
  $$(PlutusTx.compile [|| \tn amt -> Scripts.wrapMonetaryPolicy $ mkCurPolicy tn amt ||])
    `PlutusTx.applyCode` PlutusTx.liftCode tn
    `PlutusTx.applyCode` PlutusTx.liftCode amt

getCurrencySymbol :: TokenName -> Integer -> Ledger.CurrencySymbol
getCurrencySymbol tn amt = case tn of
  "" -> Ada.adaSymbol
  _  -> Ledger.scriptCurrencySymbol $ curPolicy tn amt

data MintParams = MintParams
  { mpTokenName :: !TokenName
  , mpAmount    :: !Integer
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)


type MintSchema = 
  BlockchainActions
    .\/ Endpoint "mint" MintParams

-- | Generates tokens with the specified name/amount and pays an equal amount 
--   of Ada to the swap script.
mintContract :: MintParams -> Contract w MintSchema Text ()
mintContract mp = do
  let
    tn       = mpTokenName mp
    amt      = mpAmount mp
    cs       = getCurrencySymbol tn amt
    payVal   = Ada.lovelaceValueOf amt
    forgeVal = Value.singleton cs tn amt
    lookups  = Constraints.monetaryPolicy $ curPolicy tn amt
    tx =
      Constraints.mustPayToOtherScript
          swapValHash
          (Datum $ PlutusTx.toData ())
          payVal
        <> Constraints.mustForgeValue forgeVal
  ledgerTx <- submitTxConstraintsWith @Void lookups tx
  void $ awaitTxConfirmed $ Ledger.txId ledgerTx

mintEndpoints :: Contract () MintSchema Text ()
mintEndpoints = mint >> mintEndpoints where mint = endpoint @"mint" >>= mintContract


