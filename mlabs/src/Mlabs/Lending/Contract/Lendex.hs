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
import           Ledger.Constraints
import qualified PlutusTx                     as PlutusTx
import           PlutusTx.Prelude             hiding (Applicative (..), check, Semigroup(..), Monoid(..))
import qualified PlutusTx.Prelude             as Plutus

import Mlabs.Emulator.Blockchain
import Mlabs.Emulator.Types
import Mlabs.Lending.Logic.React
import Mlabs.Lending.Logic.Types
import qualified Mlabs.Lending.Contract.Forge as Forge
import Mlabs.Lending.Contract.Utils

import Plutus.Trace.Emulator (EmulatorTrace, callEndpoint, activateContractWallet)
import qualified Wallet.Emulator as Emulator

import qualified Data.Map as M
-- import Data.Text.Prettyprint.Doc.Extras

type Lendex = SM.StateMachine (LendexId, LendingPool) Act

{-# INLINABLE machine #-}
machine :: LendexId -> Lendex
machine lid = (SM.mkStateMachine Nothing (transition lid) isFinal)
  { SM.smCheck = checkTimestamp }
  where
    isFinal = const False

    checkTimestamp _ input ctx = maybe True check $ getInputTime input
      where
        check t = member (Slot t) range
        range = txInfoValidRange $ scriptContextTxInfo ctx

    getInputTime = \case
      UserAct time _ _  -> Just time
      PriceAct time _ _ -> Just time
      _                 -> Nothing

{-# INLINABLE mkValidator #-}
mkValidator :: LendexId -> Scripts.ValidatorType Lendex
mkValidator lid = SM.mkValidator (machine lid)

client :: LendexId -> SM.StateMachineClient (LendexId, LendingPool) Act
client lid = SM.mkStateMachineClient $ SM.StateMachineInstance (machine lid) (scriptInstance lid)

lendexValidatorHash :: LendexId -> ValidatorHash
lendexValidatorHash lid = Scripts.scriptHash (scriptInstance lid)

lendexAddress :: LendexId -> Address
lendexAddress lid = scriptHashAddress (lendexValidatorHash lid)

scriptInstance :: LendexId -> Scripts.ScriptInstance Lendex
scriptInstance lid = Scripts.validator @Lendex
  ($$(PlutusTx.compile [|| mkValidator ||])
      `PlutusTx.applyCode` (PlutusTx.liftCode lid)
  )
  $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator

{-# INLINABLE transition #-}
transition ::
     LendexId
  -> SM.State (LendexId, LendingPool)
  -> Act
  -> Maybe (SM.TxConstraints SM.Void SM.Void, SM.State (LendexId, LendingPool))
transition lid SM.State{stateData=oldData, stateValue=oldValue} input
  | lid == inputLid = case runStateT (react input) (snd oldData) of
      Left _err              -> Nothing
      Right (resps, newData) -> Just ( foldMap toConstraints resps Plutus.<> ctxConstraints
                                    , SM.State { stateData  = (lid, newData)
                                                , stateValue = updateRespValue resps oldValue })
  | otherwise = Nothing
  where
    inputLid = fst oldData

    -- we check that user indeed signed the transaction with his own key
    ctxConstraints = maybe Plutus.mempty mustBeSignedBy userId

    userId = case input of
      UserAct _ (UserId uid) _ -> Just uid
      _                        -> Nothing

-----------------------------------------------------------------------
-- endpoints and schemas

type LendexError = SM.SMContractError

type UserLendexSchema =
  BlockchainActions
    .\/ Endpoint "user-action" UserAct

type UserApp a = Contract () UserLendexSchema LendexError a

findInputStateDatum :: LendexId -> UserApp Datum
findInputStateDatum lid = do
  utxos <- utxoAt (lendexAddress lid)
  maybe err P.pure $ firstJust (readDatum . snd) $ M.toList utxos
  where
    err = throwError $ SM.SMCContractError "Can not find Lending app instance"

userAction :: LendexId -> UserAct -> UserApp ()
userAction lid act = do
  currentTimestamp <- getSlot <$> currentSlot
  pkh <- fmap pubKeyHash ownPubKey
  inputDatum <- findInputStateDatum lid
  let lookups = monetaryPolicy (Forge.currencyPolicy lid) P.<>
                ownPubKeyHash  pkh
      constraints = mustIncludeDatum inputDatum
  t <- SM.mkStep (client lid) (UserAct currentTimestamp (UserId pkh) act)
  logInfo @String $ "Executes action " P.<> show act
  case t of
    Left _err -> logError ("Action failed" :: String)
    Right SM.StateMachineTransition{smtConstraints=constraints', smtLookups=lookups'} -> do
        tx <- submitTxConstraintsWith (lookups P.<> lookups') (constraints P.<> constraints')
        -- mapM_ (logInfo @String) (lines $ show $ pretty tx)
        awaitTxConfirmed (txId tx)

-- | Endpoints for user
userEndpoints :: LendexId -> UserApp ()
userEndpoints lid = forever userAction'
  where
    userAction' = endpoint @"user-action" >>= userAction lid

type PriceOracleLendexSchema =
  BlockchainActions
    .\/ Endpoint "price-oracle-action" PriceAct

type PriceOracleApp a = Contract () PriceOracleLendexSchema LendexError a

priceOracleAction :: LendexId -> PriceAct -> PriceOracleApp ()
priceOracleAction lid act = do
  pkh <- fmap pubKeyHash ownPubKey
  currentTimestamp <- getSlot <$> currentSlot
  void $ SM.runStep (client lid) (PriceAct currentTimestamp (UserId pkh) act)

-- | Endpoints for price oracle
priceOracleEndpoints :: LendexId -> PriceOracleApp ()
priceOracleEndpoints lid = forever priceOracleAction'
  where
    priceOracleAction' = endpoint @"price-oracle-action" >>= priceOracleAction lid

type GovernLendexSchema =
  BlockchainActions
    .\/ Endpoint "govern-action" GovernAct
    .\/ Endpoint "start-lendex"  StartParams

data StartParams = StartParams
  { sp'coins     :: [CoinCfg]  -- ^ supported coins with ratios to ADA
  , sp'initValue :: Value      -- ^ init value deposited to the lending app
  , sp'admins    :: [UserId]   -- ^ admins
  , sp'oracles   :: [UserId]   -- ^ trusted oracles
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

type GovernApp a = Contract () GovernLendexSchema LendexError a

governAction :: LendexId -> GovernAct -> GovernApp ()
governAction lid act = do
  uid <- ownUserId
  void $ SM.runStep (client lid) (GovernAct uid act)

startLendex :: LendexId -> StartParams -> GovernApp ()
startLendex lid StartParams{..} = do
  void $ SM.runInitialise (client lid) (lid, initLendingPool (Forge.currencySymbol lid) sp'coins sp'admins sp'oracles) sp'initValue

-- | Endpoints for admin user
governEndpoints :: LendexId -> GovernApp ()
governEndpoints lid = startLendex' >> forever governAction'
  where
    governAction' = endpoint @"govern-action" >>= (governAction lid)
    startLendex'  = endpoint @"start-lendex"  >>= (startLendex lid)

---------------------------------------------------------
-- call endpoints (for debug and testing)

-- | Calls user act
callUserAct :: LendexId -> Emulator.Wallet -> UserAct -> EmulatorTrace ()
callUserAct lid wal act = do
  hdl <- activateContractWallet wal (userEndpoints lid)
  void $ callEndpoint @"user-action" hdl act

-- | Calls price oracle act
callPriceOracleAct :: LendexId -> Emulator.Wallet -> PriceAct -> EmulatorTrace ()
callPriceOracleAct lid wal act = do
  hdl <- activateContractWallet wal (priceOracleEndpoints lid)
  void $ callEndpoint @"price-oracle-action" hdl act

-- | Calls govern act
callGovernAct :: LendexId -> Emulator.Wallet -> GovernAct -> EmulatorTrace ()
callGovernAct lid wal act = do
  hdl <- activateContractWallet wal (governEndpoints lid)
  void $ callEndpoint @"govern-action" hdl act

-- | Calls initialisation of state for Lending pool
callStartLendex :: LendexId -> Emulator.Wallet -> StartParams -> EmulatorTrace ()
callStartLendex lid wal sp = do
  hdl <- activateContractWallet wal (governEndpoints lid)
  void $ callEndpoint @"start-lendex" hdl sp

