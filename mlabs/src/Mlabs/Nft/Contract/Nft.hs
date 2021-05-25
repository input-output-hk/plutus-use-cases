{-# OPTIONS_GHC -fno-specialize #-}
{-# OPTIONS_GHC -fno-strictness #-}
-- | Plutus bindings for NFT contract
module Mlabs.Nft.Contract.Nft(
    machine
  , nftAddress
  , callUserAct
  , callStartNft
  , StartParams(..)
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
import qualified PlutusTx.Prelude             as PlutusTx


import Mlabs.Emulator.Blockchain
import Mlabs.Emulator.Types
import Mlabs.Nft.Logic.React
import Mlabs.Nft.Logic.Types
import qualified Mlabs.Nft.Contract.Forge as Forge
import Mlabs.Lending.Contract.Utils

import Plutus.Trace.Emulator (EmulatorTrace, callEndpoint, activateContractWallet)
import qualified Wallet.Emulator as Emulator

import qualified Data.Map as M

type NftMachine = SM.StateMachine Nft Act
type NftMachineClient = SM.StateMachineClient Nft Act

{-# INLINABLE machine #-}
machine :: NftId -> NftMachine
machine nftId = (SM.mkStateMachine Nothing (transition nftId) isFinal)
  where
    isFinal = const False

{-# INLINABLE mkValidator #-}
mkValidator :: NftId -> Scripts.ValidatorType NftMachine
mkValidator nftId = SM.mkValidator (machine nftId)

client :: NftId -> NftMachineClient
client nftId = SM.mkStateMachineClient $ SM.StateMachineInstance (machine nftId) (scriptInstance nftId)

nftValidatorHash :: NftId -> ValidatorHash
nftValidatorHash nftId = Scripts.scriptHash (scriptInstance nftId)

nftAddress :: NftId -> Address
nftAddress nftId = scriptHashAddress (nftValidatorHash nftId)

scriptInstance :: NftId -> Scripts.ScriptInstance NftMachine
scriptInstance nftId = Scripts.validator @NftMachine
  ($$(PlutusTx.compile [|| mkValidator ||])
    `PlutusTx.applyCode` (PlutusTx.liftCode nftId)
  )
  $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator

{-# INLINABLE transition #-}
transition ::
     NftId
  -> SM.State Nft
  -> Act
  -> Maybe (SM.TxConstraints SM.Void SM.Void, SM.State Nft)
transition nftId SM.State{stateData=oldData, stateValue=oldValue} input
  | idIsValid =
      case runStateT (react input) oldData of
        Left _err              -> Nothing
        Right (resps, newData) -> Just ( foldMap toConstraints resps
                                      , SM.State { stateData  = newData
                                                  , stateValue = updateRespValue resps oldValue })
  | otherwise = Nothing
  where
    idIsValid = nftId == nft'id oldData

-----------------------------------------------------------------------
-- endpoints and schemas

type NftError = SM.SMContractError

type NftSchema =
  BlockchainActions
    .\/ Endpoint "user-action" UserAct

type NftContract a = Contract () NftSchema NftError a

findInputStateDatum :: NftId -> NftContract Datum
findInputStateDatum nid = do
  utxos <- utxoAt (nftAddress nid)
  maybe err P.pure $ firstJust (readDatum . snd) $ M.toList utxos
  where
    err = throwError $ SM.SMCContractError "Can not find NFT app instance"

getUserId :: HasBlockchainActions s => Contract () s NftError UserId
getUserId = fmap (UserId . pubKeyHash) ownPubKey

userAction :: NftId -> UserAct -> NftContract ()
userAction nid act = do
  pkh <- fmap pubKeyHash ownPubKey
  inputDatum <- findInputStateDatum nid
  let lookups = monetaryPolicy (Forge.currencyPolicy nid) P.<>
                ownPubKeyHash  pkh
      constraints = mustIncludeDatum inputDatum
  t <- SM.mkStep (client nid) (UserAct (UserId pkh) act)
  logInfo @String $ "Executes action " P.<> show act
  case t of
    Left _err -> logError ("Action failed" :: String)
    Right SM.StateMachineTransition{smtConstraints=constraints', smtLookups=lookups'} -> do
        tx <- submitTxConstraintsWith (lookups P.<> lookups') (constraints P.<> constraints')
        -- mapM_ (logInfo @String) (lines $ show $ pretty tx)
        awaitTxConfirmed (txId tx)

-- | Endpoints for user
userEndpoints :: NftId -> NftContract ()
userEndpoints nid = forever userAction'
  where
    userAction' = endpoint @"user-action" >>= (userAction nid)

data StartParams = StartParams
  { sp'content :: ByteString
  , sp'share   :: Rational
  , sp'price   :: Maybe Integer
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

type AuthorContract a = Contract () AuthorScheme NftError a

type AuthorScheme =
  BlockchainActions
    .\/ Endpoint "start-nft"  StartParams

startNft :: StartParams -> AuthorContract ()
startNft StartParams{..} = do
  authorId <- getUserId
  void $ SM.runInitialise (client nid) (initNft authorId sp'content sp'share sp'price) PlutusTx.mempty
  where
    nid = toNftId sp'content

startParamsToNftId :: StartParams -> NftId
startParamsToNftId = toNftId . sp'content

-- | Endpoints for admin user
authorEndpoints :: AuthorContract ()
authorEndpoints = forever startNft'
  where
    startNft'  = endpoint @"start-nft"  >>= startNft

---------------------------------------------------------
-- call endpoints (for debug and testing)

-- | Calls user act
callUserAct :: NftId -> Emulator.Wallet -> UserAct -> EmulatorTrace ()
callUserAct nid wal act = do
  hdl <- activateContractWallet wal (userEndpoints nid)
  void $ callEndpoint @"user-action" hdl act

-- | Calls initialisation of state for Lending pool
callStartNft :: Emulator.Wallet -> StartParams -> EmulatorTrace NftId
callStartNft wal sp = do
  hdl <- activateContractWallet wal authorEndpoints
  void $ callEndpoint @"start-nft" hdl sp
  return nid
  where
    nid = startParamsToNftId sp

