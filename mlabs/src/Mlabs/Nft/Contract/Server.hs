module Mlabs.Nft.Contract.Server (
  -- * Contracts
  UserContract,
  AuthorContract,

  -- * Endpoints
  userEndpoints,
  authorEndpoints,
  startNft,
) where

import Prelude

import Control.Monad (forever)
import Data.List.Extra (firstJust)
import Data.Map qualified as M
import Data.Monoid (Last (..))
import Ledger.Address (pubKeyAddress)
import Ledger.Constraints (mintingPolicy, mustIncludeDatum, mustMintValue, mustSpendPubKeyOutput, ownPubKeyHash)
import Ledger.Crypto (pubKeyHash)
import Plutus.Contract (Contract, logError, ownPubKey, tell, throwError, toContract, utxoAt)
import Plutus.V1.Ledger.Api (Datum)

import Mlabs.Emulator.Types (ownUserId)
import Mlabs.Nft.Contract.Api (AuthorSchema, Buy, IsUserAct, SetPrice, StartParams (..), UserSchema, toUserAct)
import Mlabs.Nft.Contract.StateMachine qualified as SM
import Mlabs.Nft.Logic.Types (Act (UserAct), NftId, initNft, toNftId)
import Mlabs.Plutus.Contract (getEndpoint, readDatum, selectForever)

-- | NFT contract for the user
type UserContract a = Contract () UserSchema SM.NftError a

-- | Contract for the author of NFT
type AuthorContract a = Contract (Last NftId) AuthorSchema SM.NftError a

----------------------------------------------------------------
-- endpoints

-- | Endpoints for user
userEndpoints :: NftId -> UserContract ()
userEndpoints nid =
  selectForever
    [ getEndpoint @Buy $ userAction nid
    , getEndpoint @SetPrice $ userAction nid
    ]

-- | Endpoints for admin user
authorEndpoints :: AuthorContract ()
authorEndpoints = forever startNft'
  where
    startNft' = toContract $ getEndpoint @StartParams $ startNft

userAction :: IsUserAct a => NftId -> a -> UserContract ()
userAction nid input = do
  pkh <- pubKeyHash <$> ownPubKey
  act <- getUserAct input
  inputDatum <- findInputStateDatum nid
  let lookups =
        mintingPolicy (SM.nftPolicy nid)
          <> ownPubKeyHash pkh
      constraints = mustIncludeDatum inputDatum
  SM.runStepWith nid act lookups constraints

{- | Initialise NFt endpoint.
 We save NftId to the contract writer.
-}
startNft :: StartParams -> AuthorContract ()
startNft StartParams {..} = do
  orefs <- M.keys <$> (utxoAt . pubKeyAddress =<< ownPubKey)
  case orefs of
    [] -> logError @String "No UTXO found"
    oref : _ -> do
      let nftId = toNftId oref sp'content
          val = SM.nftValue nftId
          lookups = mintingPolicy $ SM.nftPolicy nftId
          tx =
            mustMintValue val
              <> mustSpendPubKeyOutput oref
      authorId <- ownUserId
      SM.runInitialiseWith nftId (initNft oref authorId sp'content sp'share sp'price) val lookups tx
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
  utxos <- utxoAt (SM.nftAddress nid)
  maybe err pure $ firstJust (readDatum . snd) $ M.toList utxos
  where
    err = throwError $ SM.toNftError "Can not find NFT app instance"
