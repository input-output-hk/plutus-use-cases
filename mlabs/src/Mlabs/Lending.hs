{-# OPTIONS_GHC -fno-specialize #-}
{-# OPTIONS_GHC -fno-strictness #-}
module Mlabs.Lending where

import           PlutusTx.Prelude     hiding (Semigroup(..), unless)
import qualified PlutusTx.Prelude     as Plutus

import Control.Monad (forever)

import           Data.Aeson (FromJSON, ToJSON)
import           Data.Text (Text)
import           GHC.Generics (Generic)

import           Ledger                           hiding (singleton)
-- import           Ledger.Constraints               as Constraints
import           Ledger.Constraints.OnChain       as Constraints
import           Ledger.Constraints.TxConstraints as Constraints
import           Plutus.Contract
import qualified PlutusTx
import qualified Ledger.Typed.Scripts             as Scripts

import           Playground.Contract (ToSchema)
import qualified Prelude

import Mlabs.Lending.Coin
import Mlabs.Lending.Utils

lendexTokenName, poolStateTokenName :: TokenName
lendexTokenName = "Lendex"
poolStateTokenName = "Pool State"

newtype Lendex = Lendex
     { lxCoin :: Coin
     } deriving stock    (Show, Generic)
       deriving anyclass (ToJSON, FromJSON, ToSchema)
       deriving newtype  (Prelude.Eq, Prelude.Ord)
PlutusTx.makeLift ''Lendex

-- | Available actions
data Action
  = Create Coin
  | Close
  deriving (Show)

PlutusTx.unstableMakeIsData ''Action
PlutusTx.makeLift ''Action

type LendingPool = [Coin]

-- | Lending datum
data LendingDatum
  = Factory [Coin]
  | Pool Coin

PlutusTx.unstableMakeIsData ''LendingDatum
PlutusTx.makeLift ''LendingDatum

-- | Parameters for create endpoint
data CreateParams = CreateParams
  { cpCoin :: Coin
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

{-# INLINABLE mkValidator #-}
-- | On-chain script validator
mkValidator :: Lendex -> Coin -> LendingDatum -> Action -> ScriptContext -> Bool
mkValidator lx c dat act ctx = case (dat, act) of
  (Factory cs, Create pool) -> validateCreate lx c cs pool ctx
  (_,          Close      ) -> validateClose lx c dat ctx
  _                         -> False

{-# INLINABLE validateCreate #-}
-- | Validate create-case
validateCreate :: Lendex -> Coin -> [Coin] -> Coin -> ScriptContext -> Bool
validateCreate Lendex{..} poolCoin coins newCoin ctx =
       lendexCoinPresent
    && newCoinIsAdded
    && poolStateCoinForged
    && keepsLedexCoin
    && keepsPoolStateCoin
  where
    lendexCoinPresent =
      Plutus.traceIfFalse "Lendex coin not present"
        (coinValueOf (valueWithin $ findOwnInput' ctx) lxCoin == 1)

    newCoinIsAdded =
      Plutus.traceIfFalse "New coin is added to pool" $
        all (/= newCoin) coins

    poolStateCoinForged =
      Plutus.traceIfFalse "Pool state coin not forged" $
       (coinValueOf forged poolCoin == 1)

    keepsLedexCoin =
      Constraints.checkOwnOutputConstraint ctx (OutputConstraint (Factory $ newCoin : coins) $
        coin lxCoin 1)

    keepsPoolStateCoin =
      Constraints.checkOwnOutputConstraint ctx (OutputConstraint (Pool newCoin) $
        coin poolCoin 1)

    forged :: Value
    forged = txInfoForge $ scriptContextTxInfo ctx

{-# INLINABLE validateClose #-}
validateClose :: Lendex -> Coin -> LendingDatum -> ScriptContext -> Bool
validateClose _ _ _ _ = True

{-# INLINABLE validateLiquidityForging #-}
validateLiquidityForging :: Lendex -> TokenName -> ScriptContext -> Bool
validateLiquidityForging us tn ctx = case [ i
                                          | i <- txInfoInputs $ scriptContextTxInfo ctx
                                          , let v = valueWithin i
                                          , (coinValueOf v usC == 1) ||
                                            (coinValueOf v lpC == 1)
                                          ] of
    [_]    -> True
    [_, _] -> True
    _      -> Plutus.traceError "pool state forging without Lendex input"
  where
    usC, lpC :: Coin
    usC = lxCoin us
    lpC = mkCoin (ownCurrencySymbol ctx) tn

lendexInstance :: Lendex -> Scripts.ScriptInstance Lending
lendexInstance lx = Scripts.validator @Lending
    ($$(PlutusTx.compile [|| mkValidator ||])
        `PlutusTx.applyCode` PlutusTx.liftCode lx
        `PlutusTx.applyCode` PlutusTx.liftCode c)
     $$(PlutusTx.compile [|| wrap ||])
  where
    c :: Coin
    c = poolStateCoin lx

    wrap = Scripts.wrapValidator @LendingDatum @Action

lendexScript :: Lendex -> Validator
lendexScript = Scripts.validatorScript . lendexInstance

lendexAddress :: Lendex -> Ledger.Address
lendexAddress = Ledger.scriptAddress . lendexScript

lendex :: CurrencySymbol -> Lendex
lendex cs = Lendex $ mkCoin cs lendexTokenName

poolStateCoin :: Lendex -> Coin
poolStateCoin = flip mkCoin poolStateTokenName . liquidityCurrency

liquidityPolicy :: Lendex -> MonetaryPolicy
liquidityPolicy lx = mkMonetaryPolicyScript $
    $$(PlutusTx.compile [|| \u t -> Scripts.wrapMonetaryPolicy (validateLiquidityForging u t) ||])
        `PlutusTx.applyCode` PlutusTx.liftCode lx
        `PlutusTx.applyCode` PlutusTx.liftCode poolStateTokenName

liquidityCurrency :: Lendex -> CurrencySymbol
liquidityCurrency = scriptCurrencySymbol . liquidityPolicy

findLendingFactory :: App (TxOutRef, TxOut, LendingPool)
findLendingFactory = undefined

-- | TODO
create :: CreateParams -> App ()
create CreateParams{..} = do
  (_oref, _outp, _lps) <- findLendingFactory
  let _usDat = cpCoin
  return ()

data Lending
instance Scripts.ScriptType Lending where
  type RedeemerType Lending = Action
  type DatumType    Lending = LendingDatum

type LendingSchema =
     BlockchainActions
         .\/ Endpoint "create" CreateParams

type App a = Contract () LendingSchema Text a

endpoints :: App ()
endpoints = create' >> forever endpoints
  where
    create' = endpoint @"create" >>= create

