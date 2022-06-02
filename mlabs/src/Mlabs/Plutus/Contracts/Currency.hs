{-# LANGUAGE NamedFieldPuns #-}

module Mlabs.Plutus.Contracts.Currency (
  OneShotCurrency (..),
  CurrencySchema,
  CurrencyError (..),
  AsCurrencyError (..),
  curPolicy,

  -- * Actions etc
  mintContract,
  mintedValue,
  currencySymbol,

  -- * Simple minting policy currency
  SimpleMPS (..),
  mintCurrency,
) where

import Control.Lens
import PlutusTx.Prelude hiding (Monoid (..), Semigroup (..))

import Plutus.Contract as Contract

import Ledger (CurrencySymbol, PaymentPubKeyHash, TxId, TxOutRef (..), getCardanoTxId, scriptCurrencySymbol)
import Ledger.Constraints qualified as Constraints
import Ledger.Contexts qualified as V
import Ledger.Scripts
import PlutusTx qualified

import Ledger.Typed.Scripts qualified as Scripts
import Ledger.Value (TokenName, Value)
import Ledger.Value qualified as Value
import Plutus.V1.Ledger.Ada qualified as Ada

import Data.Aeson (FromJSON, ToJSON)
import Data.Map qualified as Map
import Data.Semigroup (Last (..))
import GHC.Generics (Generic)
import Plutus.Contracts.PubKey qualified as PubKey
import PlutusTx.AssocMap qualified as AssocMap
import Schema (ToSchema)
import Prelude (Semigroup (..))
import Prelude qualified as Haskell

{- HLINT ignore "Use uncurry" -}

-- | A currency that can be created exactly once
data OneShotCurrency = OneShotCurrency
  { -- | Transaction input that must be spent when
    --   the currency is minted.
    curRefTransactionOutput :: (TxId, Integer)
  , -- | How many units of each 'TokenName' are to
    --   be minted.
    curAmounts :: AssocMap.Map TokenName Integer
  }
  deriving stock (Generic, Haskell.Show, Haskell.Eq)
  deriving anyclass (ToJSON, FromJSON)

PlutusTx.makeLift ''OneShotCurrency

currencyValue :: CurrencySymbol -> OneShotCurrency -> Value
currencyValue s OneShotCurrency {curAmounts = amts} =
  let values = map (\(tn, i) -> Value.singleton s tn i) (AssocMap.toList amts)
   in fold values

mkCurrency :: TxOutRef -> [(TokenName, Integer)] -> OneShotCurrency
mkCurrency (TxOutRef h i) amts =
  OneShotCurrency
    { curRefTransactionOutput = (h, i)
    , curAmounts = AssocMap.fromList amts
    }

checkPolicy :: OneShotCurrency -> () -> V.ScriptContext -> Bool
checkPolicy c@(OneShotCurrency (refHash, refIdx) _) _ ctx@V.ScriptContext {V.scriptContextTxInfo = txinfo} =
  let -- see note [Obtaining the currency symbol]
      ownSymbol = V.ownCurrencySymbol ctx

      minted = V.txInfoMint txinfo
      expected = currencyValue ownSymbol c

      -- True if the pending transaction mints the amount of
      -- currency that we expect
      mintOK =
        let v = expected == minted
         in traceIfFalse "C0" {-"Value minted different from expected"-} v

      -- True if the pending transaction spends the output
      -- identified by @(refHash, refIdx)@
      txOutputSpent =
        let v = V.spendsOutput txinfo refHash refIdx
         in traceIfFalse "C1" {-"Pending transaction does not spend the designated transaction output"-} v
   in mintOK && txOutputSpent

curPolicy :: OneShotCurrency -> MintingPolicy
curPolicy cur =
  mkMintingPolicyScript $
    $$(PlutusTx.compile [||Scripts.wrapMintingPolicy . checkPolicy||])
      `PlutusTx.applyCode` PlutusTx.liftCode cur

{- note [Obtaining the currency symbol]
The currency symbol is the address (hash) of the validator. That is why
we can use 'Ledger.scriptAddress' here to get the symbol  in off-chain code,
for example in 'mintedValue'.
Inside the validator script (on-chain) we can't use 'Ledger.scriptAddress',
because at that point we don't know the hash of the script yet. That
is why we use 'V.ownCurrencySymbol', which obtains the hash from the
'PolicyCtx' value.
-}

-- | The 'Value' minted by the 'OneShotCurrency' contract
mintedValue :: OneShotCurrency -> Value
mintedValue cur = currencyValue (currencySymbol cur) cur

currencySymbol :: OneShotCurrency -> CurrencySymbol
currencySymbol = scriptCurrencySymbol . curPolicy

data CurrencyError
  = PKError PubKey.PubKeyError
  | CurContractError ContractError
  deriving stock (Haskell.Eq, Haskell.Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

makeClassyPrisms ''CurrencyError

instance AsContractError CurrencyError where
  _ContractError = _CurContractError

{- | @mint [(n1, c1), ..., (n_k, c_k)]@ creates a new currency with
   @k@ token names, minting @c_i@ units of each token @n_i@.
   If @k == 0@ then no value is minted. A one-shot minting policy
   script is used to ensure that no more units of the currency can
   be minted afterwards.
-}

{- As `Plutus.Contract.Wallet.export` doesn't support Public Key Inputs at the moment,
   original plutus-use-cases contract was tweaked to make UTxO at script address first,
   then use that UTxO to mint one-shot currencies.
   This is the only change.
-}
mintContract ::
  forall w s e.
  ( AsCurrencyError e
  ) =>
  PaymentPubKeyHash ->
  [(TokenName, Integer)] ->
  Contract w s e OneShotCurrency
mintContract pkh amounts = mapError (review _CurrencyError) $ do
  (txOutRef, ciTxOut, pkInst) <- mapError PKError (PubKey.pubKeyContract pkh (Ada.adaValueOf 3))
  let theCurrency = mkCurrency txOutRef amounts
      curVali = curPolicy theCurrency
      lookups =
        Constraints.mintingPolicy curVali
          <> Constraints.otherData (Datum $ getRedeemer unitRedeemer)
          <> Constraints.unspentOutputs (maybe Map.empty (Map.singleton txOutRef) ciTxOut)
          <> Constraints.otherScript (Scripts.validatorScript pkInst)
      mintTx =
        Constraints.mustSpendScriptOutput txOutRef unitRedeemer
          <> Constraints.mustMintValue (mintedValue theCurrency)
  tx <- submitTxConstraintsWith @Scripts.Any lookups mintTx
  _ <- awaitTxConfirmed (getCardanoTxId tx)
  pure theCurrency

{- | Minting policy for a currency that has a fixed amount of tokens issued
   in one transaction
-}
data SimpleMPS = SimpleMPS
  { tokenName :: TokenName
  , amount :: Integer
  }
  deriving stock (Haskell.Eq, Haskell.Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

type CurrencySchema =
  Endpoint "Create native token" SimpleMPS

-- | Use 'mintContract' to create the currency specified by a 'SimpleMPS'
mintCurrency ::
  Promise (Maybe (Last OneShotCurrency)) CurrencySchema CurrencyError OneShotCurrency
mintCurrency = endpoint @"Create native token" $ \SimpleMPS {tokenName, amount} -> do
  ownPK <- ownPaymentPubKeyHash
  cur <- mintContract ownPK [(tokenName, amount)]
  tell (Just (Last cur))
  pure cur
