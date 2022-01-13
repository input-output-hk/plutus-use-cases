module Mlabs.NftStateMachine.Contract.Server (
  -- * Contracts
  UserContract,
  AuthorContract,

  -- * Endpoints
  userEndpoints,
  authorEndpoints,
  startNft,
) where

import Prelude (String, (<>))

import Control.Lens (preview)
import Control.Monad (forever)
import Data.Map qualified as M
import Data.Monoid (Last (..))
import Ledger (pubKeyHashAddress)
import Ledger.Constraints (mintingPolicy, mustIncludeDatum, mustMintValue, mustSpendPubKeyOutput)
import Ledger.Constraints qualified as Constraints
import Ledger.Tx (ciTxOutDatum)
import Mlabs.Data.List (firstJustRight)
import Mlabs.Emulator.Types (ownUserId)
import Mlabs.NftStateMachine.Contract.Api (AuthorSchema, Buy, IsUserAct, SetPrice, StartParams (..), UserSchema, toUserAct)
import Mlabs.NftStateMachine.Contract.StateMachine qualified as SM
import Mlabs.NftStateMachine.Logic.Types (Act (UserAct), NftId, initNft, toNftId)
import Mlabs.Plutus.Contract (getEndpoint, selectForever)
import Plutus.Contract (Contract, logError, ownPaymentPubKeyHash, tell, throwError, toContract, utxosAt)
import Plutus.V1.Ledger.Api (Datum)
import PlutusTx.Prelude hiding ((<>))

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
  pkh <- ownPaymentPubKeyHash
  act <- getUserAct input
  inputDatum <- findInputStateDatum nid
  let lookups =
        mintingPolicy (SM.nftPolicy nid)
          <> Constraints.ownPaymentPubKeyHash pkh
      constraints = mustIncludeDatum inputDatum
  SM.runStepWith nid act lookups constraints

{- | Initialise NFt endpoint.
 We save NftId to the contract writer.
-}
startNft :: StartParams -> AuthorContract ()
startNft StartParams {..} = do
  orefs <- M.keys <$> (utxosAt . (`pubKeyHashAddress` Nothing) =<< ownPaymentPubKeyHash)
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
  utxos <- utxosAt (SM.nftAddress nid)
  maybe err pure $ firstJustRight (preview ciTxOutDatum . snd) $ M.toList utxos
  where
    err = throwError $ SM.toNftError "Can not find NFT app instance"
