{-# OPTIONS_GHC -fno-specialize #-}
{-# OPTIONS_GHC -fno-strictness #-}
-- | Plutus bindings for NFT contract
module Mlabs.Nft.Contract.Nft(
    machine
  , nftAddress
  , callUserAct
  , callStartNft
  , StartParams(..)
  , UserSchema
  , AuthorSchema
  , startNft
  , userEndpoints
  , BuyAct(..)
  , SetPriceAct(..)
) where

import qualified Prelude as P

import Control.Monad (forever)
import Control.Monad.State.Strict (runStateT)
import Data.List.Extra (firstJust)

import Data.Aeson (FromJSON, ToJSON)
import Data.Monoid (Last(..))
import Data.Functor (void)

import GHC.Generics
import qualified PlutusTx.Prelude as Plutus

import           Plutus.Contract
import qualified Plutus.Contract.StateMachine as SM
import           Ledger                       hiding (singleton)
import qualified Ledger.Typed.Scripts         as Scripts
import           Ledger.Constraints
import qualified PlutusTx                     as PlutusTx
import           PlutusTx.Prelude             hiding (Applicative (..), check, Semigroup(..), Monoid(..))
import qualified Control.Monad.Freer.Error    as F
import Playground.Contract (ToSchema)

import Mlabs.Emulator.Blockchain
import Mlabs.Emulator.Types
import Mlabs.Nft.Logic.React
import Mlabs.Nft.Logic.Types
import qualified Mlabs.Nft.Contract.Forge as Forge
import qualified Mlabs.Plutus.Contract.StateMachine as SM
import Mlabs.Lending.Contract.Utils

import Plutus.Trace.Emulator (EmulatorTrace)
import qualified Plutus.Trace.Emulator as Trace
import qualified Wallet.Emulator as Emulator

import qualified Data.Map as M
import Plutus.V1.Ledger.Value

import Mlabs.Data.Ray (Ray)

--------------------------------------

type NftMachine = SM.StateMachine Nft Act
type NftMachineClient = SM.StateMachineClient Nft Act

{-# INLINABLE machine #-}
-- | State machine definition
machine :: NftId -> NftMachine
machine nftId = (SM.mkStateMachine Nothing (transition nftId) isFinal)
  where
    isFinal = const False

{-# INLINABLE mkValidator #-}
-- | State machine validator
mkValidator :: NftId -> Scripts.ValidatorType NftMachine
mkValidator nftId = SM.mkValidator (machine nftId)

-- | State machine client
client :: NftId -> NftMachineClient
client nftId = SM.mkStateMachineClient $ SM.StateMachineInstance (machine nftId) (scriptInstance nftId)

-- | NFT validator hash
nftValidatorHash :: NftId -> ValidatorHash
nftValidatorHash nftId = Scripts.scriptHash (scriptInstance nftId)

-- | NFT script address
nftAddress :: NftId -> Address
nftAddress nftId = scriptHashAddress (nftValidatorHash nftId)

-- | NFT script instance
scriptInstance :: NftId -> Scripts.ScriptInstance NftMachine
scriptInstance nftId = Scripts.validator @NftMachine
  ($$(PlutusTx.compile [|| mkValidator ||])
    `PlutusTx.applyCode` (PlutusTx.liftCode nftId)
  )
  $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator

{-# INLINABLE transition #-}
-- | State transitions for NFT
transition ::
     NftId
  -> SM.State Nft
  -> Act
  -> Maybe (SM.TxConstraints SM.Void SM.Void, SM.State Nft)
transition nftId SM.State{stateData=oldData, stateValue=oldValue} input
  | idIsValid =
      case runStateT (react input) oldData of
        Left _err              -> Nothing
        Right (resps, newData) -> Just ( foldMap toConstraints resps Plutus.<> ctxConstraints
                                      , SM.State { stateData  = newData
                                                  , stateValue = updateRespValue resps oldValue })
  | otherwise = Nothing
  where
    idIsValid = nftId == nft'id oldData

    -- we check that user indeed signed the transaction with his own key
    ctxConstraints = maybe Plutus.mempty mustBeSignedBy userId

    userId = case input of
      UserAct (UserId uid) _ -> Just uid
      _                      -> Nothing

-----------------------------------------------------------------------
-- NFT forge policy

-- | NFT monetary policy
nftPolicy :: NftId -> MonetaryPolicy
nftPolicy nid = Forge.currencyPolicy (nftAddress nid)  nid

-- | NFT currency symbol
nftSymbol :: NftId -> CurrencySymbol
nftSymbol nid = Forge.currencySymbol (nftAddress nid) nid

-- | NFT coin (AssetClass)
nftCoin :: NftId -> AssetClass
nftCoin nid = AssetClass (nftSymbol nid, nftId'token nid)

-- | Single value of NFT coin. We check that there is only one NFT-coin can be minted.
nftValue :: NftId -> Value
nftValue nid = assetClassValue (nftCoin nid) 1

-----------------------------------------------------------------------
-- endpoints and schemas

-- | NFT errors
type NftError = SM.SMContractError

-- | User schema. Owner can set the price and the buyer can try to buy.
type UserSchema =
  BlockchainActions
    .\/ Endpoint "buy-act"       BuyAct
    .\/ Endpoint "set-price-act" SetPriceAct

-- | User buys NFT
data BuyAct = BuyAct
  { buy'price     :: Integer
  , buy'newPrice  :: Maybe Integer
  }
  deriving stock (Show, Generic, P.Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

-- | User sets new price for NFT
data SetPriceAct = SetPriceAct
  { setPrice'newPrice :: Maybe Integer
  }
  deriving stock (Show, Generic, P.Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

-- | NFT contract for the user
type NftContract a = Contract () UserSchema NftError a

-- | Finds Datum for NFT state machine script.
findInputStateDatum :: NftId -> NftContract Datum
findInputStateDatum nid = do
  utxos <- utxoAt (nftAddress nid)
  maybe err P.pure $ firstJust (readDatum . snd) $ M.toList utxos
  where
    err = throwError $ SM.SMCContractError "Can not find NFT app instance"

-- | Get user id of the wallet owner.
getUserId :: HasBlockchainActions s => Contract w s NftError UserId
getUserId = fmap (UserId . pubKeyHash) ownPubKey

-- | User action endpoint
userAction :: NftId -> UserAct -> NftContract ()
userAction nid act = do
  pkh <- fmap pubKeyHash ownPubKey
  inputDatum <- findInputStateDatum nid
  let lookups = monetaryPolicy (nftPolicy nid) P.<>
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
    userAction' = buy `select` setPrice

    buy      = endpoint @"buy-act"       >>= (\BuyAct{..}      -> userAction nid (Buy buy'price buy'newPrice))
    setPrice = endpoint @"set-price-act" >>= (\SetPriceAct{..} -> userAction nid (SetPrice setPrice'newPrice))


-- | Parameters to init NFT
data StartParams = StartParams
  { sp'content :: ByteString      -- ^ NFT content
  , sp'share   :: Ray             -- ^ author share [0, 1] on reselling of the NFT
  , sp'price   :: Maybe Integer   -- ^ current price of NFT, if it's nothing then nobody can buy it.
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

-- | Contract for the author of NFT
type AuthorContract a = Contract (Last NftId) AuthorSchema NftError a

-- | Schema for the author of NFT
type AuthorSchema =
  BlockchainActions
    .\/ Endpoint "start-nft"  StartParams

-- | Initialise NFt endpoint.
-- We save NftId to the contract writer.
startNft :: StartParams -> AuthorContract ()
startNft StartParams{..} = do
  orefs <- M.keys <$> (utxoAt =<< pubKeyAddress <$> ownPubKey)
  case orefs of
    []        -> logError @String "No UTXO found"
    oref : _ -> do
      let nftId   = toNftId oref sp'content
          val     = nftValue nftId
          lookups = monetaryPolicy $ nftPolicy nftId
          tx      = mustForgeValue val
      authorId <- getUserId
      void $ SM.runInitialiseWith (client nftId) (initNft oref authorId sp'content sp'share sp'price) val lookups tx
      tell $ Last $ Just nftId

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
  hdl <- Trace.activateContractWallet wal (userEndpoints nid)
  case act of
    Buy{..}      -> void $ Trace.callEndpoint @"buy-act"       hdl (BuyAct act'price act'newPrice)
    SetPrice{..} -> void $ Trace.callEndpoint @"set-price-act" hdl (SetPriceAct act'newPrice)

-- | Calls initialisation of state for Lending pool
callStartNft :: Emulator.Wallet -> StartParams -> EmulatorTrace NftId
callStartNft wal sp = do
  hdl <- Trace.activateContractWallet wal authorEndpoints
  void $ Trace.callEndpoint @"start-nft" hdl sp
  void $ Trace.waitNSlots 10
  Last nid <- Trace.observableState hdl
  maybe err P.pure nid
  where
    err = F.throwError $ Trace.GenericError "No NFT started in emulator"


