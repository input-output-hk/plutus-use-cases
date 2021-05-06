{-# OPTIONS_GHC -fno-specialize #-}
{-# OPTIONS_GHC -fno-strictness #-}
-- | Lending exchange platform (Lendex for short) is a tool for
-- user to provide lending funds.
--
-- There are three roles of users:
--
-- * **admin** - can initialise whole platform and close it.
--
-- * **lender user**  can create new tokens on the platform and provide funds with it.
--
-- * **borrower user** can borrow funds.
module Mlabs.Lending where

import           PlutusTx.Prelude     hiding (Semigroup(..), unless)
import qualified PlutusTx.Prelude     as Plutus

import Control.Monad (forever, void)

import           Data.Monoid (Last(..))

import           Data.Aeson (FromJSON, ToJSON)
import           Data.Text (Text)
import           GHC.Generics (Generic)

import           Ledger                           hiding (singleton)
import           Ledger.Constraints               as Constraints
import           Ledger.Constraints.OnChain       as Constraints
import           Ledger.Constraints.TxConstraints as Constraints
import           Plutus.Contract
import qualified Plutus.Contracts.Currency        as Currency
import qualified PlutusTx
import qualified Ledger.Typed.Scripts             as Scripts

import           Playground.Contract (ToSchema)
import qualified Prelude
import           Prelude (Semigroup(..))
import qualified Data.Map as Map
import Text.Printf (printf)
import qualified Plutus.Trace as Trace
import           Plutus.Contract.Trace (Wallet)
import           Plutus.Trace (EmulatorTrace)
import Mlabs.Lending.Coin
import Mlabs.Lending.Utils

import qualified Data.Text as T

-- | Constants for thread of lendex state and pool state.
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
  -- ^ Create new coin for lending
  | Close
  -- $ close the exchange
  deriving (Show)

PlutusTx.unstableMakeIsData ''Action
PlutusTx.makeLift ''Action

type LendingPool = [Coin]

-- | Lending datum
data LendingDatum
  = Factory [Coin]
  -- ^ Global state to watch for coins that were created.
  -- For every new coin we check against this state
  -- weather it is new and have not been already created.
  | Pool Coin
  -- ^ single coint to lend funds.
  deriving stock Show

PlutusTx.unstableMakeIsData ''LendingDatum
PlutusTx.makeLift ''LendingDatum

-- | Parameters for create endpoint
data CreateParams = CreateParams
  { cpCoin :: Coin
  -- ^ coin for which we create lending capabilities.
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
-- | It validates create-case
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
-- | It validates the closing of the whole lending system
validateClose :: Lendex -> Coin -> LendingDatum -> ScriptContext -> Bool
validateClose _ _ _ _ = True

{-# INLINABLE validateLiquidityForging #-}
-- | It validates the forging of new coin for lending purposes
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

-- | Instance of validation script for lending exchange
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

-- | Validator
lendexScript :: Lendex -> Validator
lendexScript = Scripts.validatorScript . lendexInstance

-- | Validator script address
lendexAddress :: Lendex -> Ledger.Address
lendexAddress = Ledger.scriptAddress . lendexScript

-- | Wrapper to create lendex state coin out of @CurrencySymbol@.
lendex :: CurrencySymbol -> Lendex
lendex cs = Lendex $ mkCoin cs lendexTokenName

-- | Constructor for pool state coin.
-- It relies on script for new coin forgery validation.
poolStateCoin :: Lendex -> Coin
poolStateCoin = flip mkCoin poolStateTokenName . liquidityCurrency

-- | pool state forgery validator
liquidityPolicy :: Lendex -> MonetaryPolicy
liquidityPolicy lx = mkMonetaryPolicyScript $
    $$(PlutusTx.compile [|| \u t -> Scripts.wrapMonetaryPolicy (validateLiquidityForging u t) ||])
        `PlutusTx.applyCode` PlutusTx.liftCode lx
        `PlutusTx.applyCode` PlutusTx.liftCode poolStateTokenName

-- | @CurrencySumbol@ for the lendex. We use it for pool state.
-- They share common @CurrencySymbol@
liquidityCurrency :: Lendex -> CurrencySymbol
liquidityCurrency = scriptCurrencySymbol . liquidityPolicy

-- | Provides TxOut that contains lendex script.
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

-- | Provides TXOut that contains global state of lendex.
-- It provides the list of coins that are part of the exchange so far.
findLendexFactory :: Lendex -> App (TxOutRef, TxOutTx, [Coin])
findLendexFactory lx@Lendex{..} = findLendexInstance lx lxCoin $ \case
     Factory lps -> Just lps
     Pool _      -> Nothing

-- | Reads lendex datum for the @TxOut@.
getLendexDatum :: TxOutTx -> App LendingDatum
getLendexDatum o = case txOutDatumHash $ txOutTxOut o of
  Nothing -> throwError "datumHash not found"
  Just h -> case Map.lookup h $ txData $ txOutTxTx o of
    Nothing -> throwError "datum not found"
    Just (Datum e) -> case PlutusTx.fromData e of
      Nothing -> throwError "datum has wrong type"
      Just d  -> return d

-- | Creates a Lendex "factory". This factory will keep track of the existing
-- liquidity pools and enforce that there will be at most one liquidity pool
-- for any pair of tokens at any given time.
start :: HasBlockchainActions s => Contract w s Text Lendex
start = do
  pkh <- pubKeyHash <$> ownPubKey
  cs  <- fmap Currency.currencySymbol $
        mapError (T.pack . show @Currency.CurrencyError) $
        Currency.forgeContract pkh [(lendexTokenName, 1)]
  let c    = mkCoin cs lendexTokenName
      us   = lendex cs
      inst = lendexInstance us
      tx   = mustPayToTheScript (Factory []) $ coin c 1
  ledgerTx <- submitTxConstraints inst tx
  void $ awaitTxConfirmed $ txId ledgerTx

  logInfo @String $ printf "started Uniswap %s at address %s" (show us) (show $ lendexAddress us)
  return us

-- | Creates a liquidity pool for a given coin.
-- We have no coins at the start
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

-- Type to tag Redeemer and Datum for our lending platform
data Lending
instance Scripts.ScriptType Lending where
  type RedeemerType Lending = Action
  type DatumType    Lending = LendingDatum

-- | Schema for the super user who can initiate the whole lendex platform.
type LendingOwnerSchema =
     BlockchainActions
         .\/ Endpoint "start" ()

-- | Schema for lender.
type LendingSchema =
     BlockchainActions
         .\/ Endpoint "create" CreateParams  -- create new coin to lend funds

type App a = Contract () LendingSchema Text a
type OwnerApp a = Contract () LendingOwnerSchema Text a

-- | Endpoints for admin of the platform. Admin can initialise the lending platform.
ownerEndpoint :: Contract (Last Lendex) LendingOwnerSchema Text ()
ownerEndpoint = forever start'
  where
  start' =
    endpoint @"start" >>= \() -> do
      lx <- start
      tell $ Last $ Just lx

-- | Endpoints for lender
userEndpoints :: Lendex -> App ()
userEndpoints lx = forever create'
  where
    create' = endpoint @"create" >>= create lx

-----------------------------------------------
-- call endpoints (for testing)

-- | Calls init lendex platform for a given wallet.
-- Produces tag of the platform that contains coin by which we track
-- state of the platform.
callStart :: Wallet -> EmulatorTrace (Maybe Lendex)
callStart w = do
  hdl <- Trace.activateContractWallet w ownerEndpoint
  void $ Trace.callEndpoint @"start" hdl ()
  void $ Trace.waitNSlots 10
  Last res <- Trace.observableState hdl
  return res

-- | Lendeer calls create coin endpoint. Coin for @CreateParams@ is used for lending purposes.
callCreate :: Lendex -> Wallet -> CreateParams -> EmulatorTrace ()
callCreate lx w cp = do
  hdl <- Trace.activateContractWallet w (userEndpoints lx)
  void $ Trace.callEndpoint @"create" hdl cp

