module Mlabs.Nft.Contract.Server(
  -- * Contracts
    UserContract
  , AuthorContract
  -- * Endpoints
  , userEndpoints
  , authorEndpoints
  , startNft
) where

import Prelude
import Control.Monad

import qualified Data.Map as M
import Data.List.Extra (firstJust)
import Data.Monoid (Last(..))

import Playground.Contract
import Plutus.V1.Ledger.Crypto
import Plutus.V1.Ledger.Api
import Plutus.Contract
import Ledger.Constraints
import Plutus.V1.Ledger.Address

import Mlabs.Emulator.Types
import Mlabs.Nft.Logic.Types

import Mlabs.Plutus.Contract
import Mlabs.Nft.Contract.Api
import Mlabs.Nft.Contract.StateMachine

-- | NFT contract for the user
type UserContract a = Contract () UserSchema NftError a

-- | Contract for the author of NFT
type AuthorContract a = Contract (Last NftId) AuthorSchema NftError a

----------------------------------------------------------------
-- endpoints

-- | Endpoints for user
userEndpoints :: NftId -> UserContract ()
userEndpoints nid = forever $ selects
  [ act $ getEndpoint @Buy
  , act $ getEndpoint @SetPrice
  ]
  where
    act :: IsUserAct a => UserContract a -> UserContract ()
    act readInput = readInput >>= userAction nid

-- | Endpoints for admin user
authorEndpoints :: AuthorContract ()
authorEndpoints = forever startNft'
  where
    startNft'  = getEndpoint @StartParams >>= startNft

userAction :: IsUserAct a => NftId -> a -> UserContract ()
userAction nid input = do
  pkh <- pubKeyHash <$> ownPubKey
  act <- getUserAct input
  inputDatum <- findInputStateDatum nid
  let lookups = monetaryPolicy (nftPolicy nid) <>
                ownPubKeyHash  pkh
      constraints = mustIncludeDatum inputDatum
  runStepWith nid act lookups constraints

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
      authorId <- ownUserId
      runInitialiseWith nftId (initNft oref authorId sp'content sp'share sp'price) val lookups tx
      tell $ Last $ Just nftId


----------------------------------------------------------------

-- | Converts endpoint inputs to logic actions
getUserAct :: IsUserAct a => a -> UserContract Act
getUserAct act = do
  uid <- ownUserId
  pure $ UserAct uid $ toUserAct act

----------------------------------------------------------------
-- utils

-- | Finds Datum for NFT state machine script.
findInputStateDatum :: NftId -> UserContract Datum
findInputStateDatum nid = do
  utxos <- utxoAt (nftAddress nid)
  maybe err pure $ firstJust (readDatum . snd) $ M.toList utxos
  where
    err = throwError $ toNftError "Can not find NFT app instance"
