{-# OPTIONS_GHC -fno-specialize #-}
{-# OPTIONS_GHC -fno-strictness #-}
module Mlabs.Lending.Contract.Lendex(
    lendexAddress
  , mkValidator
  , scriptInstance
  -- * Endpoints
  , UserLendexSchema, UserApp
  , userEndpoints
  , PriceOracleLendexSchema, PriceOracleApp
  , priceOracleEndpoints
  , GovernLendexSchema, GovernApp
  , governEndpoints
  , StartParams(..)
  -- * Test endpoints
  , callUserAct
  , callPriceOracleAct
  , callGovernAct
  , callStartLendex
  , userAction
  , startLendex
) where

import qualified Prelude as P

import Control.Monad (forever)
import Control.Monad.State.Strict (runStateT)
import Data.List.Extra (firstJust)

import Data.Aeson (FromJSON, ToJSON)
import Data.Functor (void)

import GHC.Generics

import           Plutus.Contract
import qualified Plutus.Contract.StateMachine as SM
import           Ledger                       hiding (singleton)
import qualified Ledger.Typed.Scripts         as Scripts
import           Ledger.Value                 (assetClassValue)
import           Ledger.Constraints
import qualified PlutusTx                     as PlutusTx
import           PlutusTx.Prelude             hiding (Applicative (..), check, Semigroup(..), Monoid(..))
import qualified PlutusTx.Prelude             as PlutusTx


import Mlabs.Lending.Logic.Emulator
import Mlabs.Lending.Logic.React
import Mlabs.Lending.Logic.Types
import qualified Mlabs.Lending.Contract.Forge as Forge
import Mlabs.Lending.Contract.Utils

import Plutus.Trace.Emulator (EmulatorTrace, callEndpoint, activateContractWallet)
import qualified Wallet.Emulator as Emulator

import qualified Data.Map as M
-- import Data.Text.Prettyprint.Doc.Extras


type Lendex = SM.StateMachine LendingPool Act

{-# INLINABLE machine #-}
machine :: Lendex
machine = SM.mkStateMachine Nothing transition isFinal
  where
    isFinal = const False

{-# INLINABLE mkValidator #-}
mkValidator :: Scripts.ValidatorType Lendex
mkValidator = SM.mkValidator machine

client :: SM.StateMachineClient LendingPool Act
client = SM.mkStateMachineClient $ SM.StateMachineInstance machine scriptInstance

lendexValidatorHash :: ValidatorHash
lendexValidatorHash = Scripts.scriptHash scriptInstance

lendexAddress :: Address
lendexAddress = scriptHashAddress lendexValidatorHash

scriptInstance :: Scripts.ScriptInstance Lendex
scriptInstance = Scripts.validator @Lendex
  $$(PlutusTx.compile [|| mkValidator ||])
  $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator

{-# INLINABLE transition #-}
transition ::
     SM.State LendingPool
  -> Act
  -> Maybe (SM.TxConstraints SM.Void SM.Void, SM.State LendingPool)
transition SM.State{stateData=oldData, stateValue=oldValue} input = case runStateT (react input) oldData of
  Left _err              -> Nothing
  Right (resps, newData) -> Just ( foldMap toConstraints resps
                                 , SM.State { stateData=newData
                                            , stateValue= updateLendexValue resps oldValue })

-----------------------------------------------------------------------
-- endpoints and schemas

type LendexError = SM.SMContractError

type UserLendexSchema =
  BlockchainActions
    .\/ Endpoint "user-action" UserAct

type UserApp a = Contract () UserLendexSchema LendexError a

findInputStateDatum :: UserApp Datum
findInputStateDatum = do
  utxos <- utxoAt lendexAddress
  maybe err P.pure $ firstJust (readDatum . snd) $ M.toList utxos
  where
    err = throwError $ SM.SMCContractError "Can not find Lending app instance"

userAction :: UserAct -> UserApp ()
userAction act = do
  pkh <- fmap pubKeyHash ownPubKey
  inputDatum <- findInputStateDatum
  let lookups = monetaryPolicy Forge.currencyPolicy P.<>
                ownPubKeyHash  pkh
      constraints = mustIncludeDatum inputDatum
  t <- SM.mkStep client (UserAct (UserId pkh) act)
  logInfo @String $ "Executes action " P.<> show act
  case t of
    Left _err -> logError ("Action failed" :: String)
    Right SM.StateMachineTransition{smtConstraints=constraints', smtLookups=lookups'} -> do
        tx <- submitTxConstraintsWith (lookups P.<> lookups') (constraints P.<> constraints')
        -- mapM_ (logInfo @String) (lines $ show $ pretty tx)
        awaitTxConfirmed (txId tx)

-- | Endpoints for user
userEndpoints :: UserApp ()
userEndpoints = forever userAction'
  where
    userAction' = endpoint @"user-action" >>= userAction

type PriceOracleLendexSchema =
  BlockchainActions
    .\/ Endpoint "price-oracle-action" PriceAct

type PriceOracleApp a = Contract () PriceOracleLendexSchema LendexError a

priceOracleAction :: PriceAct -> PriceOracleApp ()
priceOracleAction act = do
  void $ SM.runStep client (PriceAct act)

-- | Endpoints for price oracle
priceOracleEndpoints :: PriceOracleApp ()
priceOracleEndpoints = forever priceOracleAction'
  where
    priceOracleAction' = endpoint @"price-oracle-action" >>= priceOracleAction

type GovernLendexSchema =
  BlockchainActions
    .\/ Endpoint "govern-action" GovernAct
    .\/ Endpoint "start-lendex"  StartParams

data StartParams = StartParams
  { sp'coins  :: [CoinCfg]  -- ^ supported coins with ratios to ADA
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

type GovernApp a = Contract () GovernLendexSchema LendexError a

governAction :: GovernAct -> GovernApp ()
governAction act = do
  void $ SM.runStep client (GovernAct act)

startLendex :: StartParams -> GovernApp ()
startLendex StartParams{..} = do
  void $ SM.runInitialise client (initLendingPool Forge.currencySymbol sp'coins) initValue
  where
    initValue = PlutusTx.mempty

-- | Endpoints for admin user
governEndpoints :: GovernApp ()
governEndpoints = startLendex' >> forever governAction'
  where
    governAction' = endpoint @"govern-action" >>= governAction
    startLendex'  = endpoint @"start-lendex"  >>= startLendex

---------------------------------------------------------

{-# INLINABLE toConstraints #-}
toConstraints :: Resp -> TxConstraints SM.Void SM.Void
toConstraints = \case
  Move addr coin amount | amount > 0 -> case addr of
    -- pays to lendex app
    Self       -> PlutusTx.mempty -- we already check this constraint with StateMachine
    -- pays to the user
    UserId pkh -> mustPayToPubKey pkh (assetClassValue coin amount)
  Mint coin amount      -> mustForgeValue (assetClassValue coin amount)
  Burn coin amount      -> mustForgeValue (assetClassValue coin $ negate amount)
  _ -> PlutusTx.mempty

{-# INLINABLE updateLendexValue #-}
updateLendexValue :: [Resp] -> Value -> Value
updateLendexValue rs val = foldMap toLendexValue rs PlutusTx.<> val

{-# INLINABLE toLendexValue #-}
toLendexValue :: Resp -> Value
toLendexValue = \case
  Move Self coin amount -> assetClassValue coin amount
  Mint coin amount      -> assetClassValue coin amount
  Burn coin amount      -> assetClassValue coin (negate amount)
  _                     -> PlutusTx.mempty

---------------------------------------------------------
-- call endpoints (for debug and testing)

-- | Calls user act
callUserAct :: Emulator.Wallet -> UserAct -> EmulatorTrace ()
callUserAct wal act = do
  hdl <- activateContractWallet wal userEndpoints
  void $ callEndpoint @"user-action" hdl act

-- | Calls price oracle act
callPriceOracleAct :: Emulator.Wallet -> PriceAct -> EmulatorTrace ()
callPriceOracleAct wal act = do
  hdl <- activateContractWallet wal priceOracleEndpoints
  void $ callEndpoint @"price-oracle-action" hdl act

-- | Calls govern act
callGovernAct :: Emulator.Wallet -> GovernAct -> EmulatorTrace ()
callGovernAct wal act = do
  hdl <- activateContractWallet wal governEndpoints
  void $ callEndpoint @"govern-action" hdl act

-- | Calls initialisation of state for Lending pool
callStartLendex :: Emulator.Wallet -> StartParams -> EmulatorTrace ()
callStartLendex wal sp = do
  hdl <- activateContractWallet wal governEndpoints
  void $ callEndpoint @"start-lendex" hdl sp

