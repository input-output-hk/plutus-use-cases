{-# OPTIONS_GHC -fno-specialize #-}
{-# OPTIONS_GHC -fno-strictness #-}
module Mlabs.Lending where

import           PlutusTx.Prelude     hiding (Semigroup(..), unless)
import qualified PlutusTx.Prelude     as Plutus

import Control.Monad (forever, void)

import           Data.Aeson (FromJSON, ToJSON)
import           Data.Text (Text)
import           GHC.Generics (Generic)

import           Ledger                           hiding (singleton)
import           Ledger.Constraints               as Constraints
import           Ledger.Constraints.OnChain       as Constraints
import           Ledger.Constraints.TxConstraints as Constraints
import           Plutus.Contract
import qualified PlutusTx
import qualified Ledger.Typed.Scripts             as Scripts

import           Playground.Contract (ToSchema)
import qualified Prelude
import           Prelude (Semigroup(..))
import qualified Data.Map as Map
import Text.Printf (printf)

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
  deriving stock Show

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
      Plutus.traceIfFalse "Lendex coin not present" $
        hasCoinValue (valueWithin $ findOwnInput' ctx) lxCoin

    newCoinIsAdded =
      Plutus.traceIfFalse "New coin is added to pool" $
        all (/= newCoin) coins

    poolStateCoinForged =
      Plutus.traceIfFalse "Pool state coin not forged" $
       hasCoinValue forged poolCoin

    keepsLedexCoin     = keepsCoin (Factory $ newCoin : coins) lxCoin
    keepsPoolStateCoin = keepsCoin (Pool newCoin) poolCoin

    keepsCoin st c = Constraints.checkOwnOutputConstraint ctx (OutputConstraint st $ coin c 1)

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
                                          , hasCoinValue v usC ||
                                            hasCoinValue v lpC
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

findLendexInstance :: Lendex -> Coin -> (LendingDatum -> Maybe a) -> App (TxOutRef, TxOutTx, a)
findLendexInstance us c f = do
    let addr = lendexAddress us
    logInfo @String $ printf "looking for Lendex instance at address %s containing coin %s " (show addr) (show c)
    utxos <- utxoAt addr
    go  [x | x@(_, o) <- Map.toList utxos, coinValueOf (txOutValue $ txOutTxOut o) c == 1]
  where
    go [] = throwError "Lendex instance not found"
    go ((oref, o) : xs) = do
        d <- getLendexDatum o
        case f d of
            Nothing -> go xs
            Just a  -> do
                logInfo @String $ printf "found Lendex instance with datum: %s" (show d)
                return (oref, o, a)

findLendexFactory :: Lendex -> App (TxOutRef, TxOutTx, [Coin])
findLendexFactory lx@Lendex{..} = findLendexInstance lx lxCoin $ \case
     Factory lps -> Just lps
     Pool _      -> Nothing

getLendexDatum :: TxOutTx -> App LendingDatum
getLendexDatum o = case txOutDatumHash $ txOutTxOut o of
  Nothing -> throwError "datumHash not found"
  Just h -> case Map.lookup h $ txData $ txOutTxTx o of
    Nothing -> throwError "datum not found"
    Just (Datum e) -> case PlutusTx.fromData e of
      Nothing -> throwError "datum has wrong type"
      Just d  -> return d

-- | Creates a liquidity pool for a given coin.
create :: Lendex -> CreateParams -> App ()
create lx CreateParams{..} = do
    (oref, o, lps) <- findLendexFactory lx
    let lp        =  cpCoin
        usInst   = lendexInstance lx
        usScript = lendexScript lx
        usDat1   = Factory $ lp : lps
        usDat2   = Pool lp
        psC      = poolStateCoin lx
        usVal    = coin (lxCoin lx) 1
        lpVal    = coin cpCoin 0

        lookups  = Constraints.scriptInstanceLookups usInst
                <> Constraints.otherScript usScript
                <> Constraints.monetaryPolicy (liquidityPolicy lx)
                <> Constraints.unspentOutputs (Map.singleton oref o)

        tx       = Constraints.mustPayToTheScript usDat1 usVal
                <> Constraints.mustPayToTheScript usDat2 lpVal
                <> Constraints.mustForgeValue (coin psC 1)
                <> Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toData $ Create lp)

    ledgerTx <- submitTxConstraintsWith lookups tx
    void $ awaitTxConfirmed $ txId ledgerTx

    logInfo $ "created liquidity pool: " ++ show lp

data Lending
instance Scripts.ScriptType Lending where
  type RedeemerType Lending = Action
  type DatumType    Lending = LendingDatum

type LendingSchema =
     BlockchainActions
         .\/ Endpoint "create" CreateParams

type App a = Contract () LendingSchema Text a

userEndpoints :: Lendex -> App ()
userEndpoints lx = forever create'
  where
    create' = endpoint @"create" >>= create lx

