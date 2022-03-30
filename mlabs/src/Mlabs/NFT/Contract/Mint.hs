{-# LANGUAGE UndecidableInstances #-}

module Mlabs.NFT.Contract.Mint (
  mint,
  getDatumsTxsOrdered,
  mintParamsToInfo,
) where

import PlutusTx.Prelude hiding (mconcat)
import Prelude (mconcat)
import Prelude qualified as Hask

import Control.Monad (void)
import Data.Map qualified as Map
import Data.Monoid (Last (..))
import Data.Text (Text)
import Text.Printf (printf)

import Plutus.ChainIndex.Tx (txOutRefMapForAddr)
import Plutus.Contract (Contract)
import Plutus.Contract qualified as Contract
import Plutus.V1.Ledger.Api (ToData (toBuiltinData))

import Ledger (txOutValue)
import Ledger.Constraints qualified as Constraints
import Ledger.Typed.Scripts (validatorScript)
import Ledger.Value as Value (TokenName (..), assetClass, singleton)

import Mlabs.NFT.Contract.Aux (
  getDatumsTxsOrdered,
  getNftAppSymbol,
  getNftHead,
  getUId,
  hashData,
 )
import Mlabs.NFT.Spooky (toSpooky, toSpookyAssetClass, unSpookyAddress, unSpookyCurrencySymbol)
import Mlabs.NFT.Types (
  AuctionState,
  DatumNft (..),
  GenericContract,
  InformationNft (..),
  InsertPoint (InsertPoint),
  MintAct (Mint),
  MintParams (..),
  NftAppInstance,
  NftId (NftId),
  NftListHead (NftListHead),
  NftListNode (..),
  PointInfo (pi'CITx, pi'CITxO, pi'TOR, pi'data),
  Pointer (Pointer),
  UniqueToken,
  UserAct (MintAct),
  UserId,
  UserWriter,
  app'symbol,
  appInstance'Address,
  appInstance'UniqueToken,
  getAppInstance,
  getDatumValue,
  info'id,
  node'appInstance,
  node'information,
 )
import Mlabs.NFT.Validation (asRedeemer, mintPolicy, txPolicy)

--------------------------------------------------------------------------------
-- MINT --

---- | Mints an NFT and sends it to the App Address.
mint :: forall s. UniqueToken -> MintParams -> Contract UserWriter s Text ()
mint uT params = do
  user <- getUId
  head' <- getNftHead uT
  case head' of
    Nothing -> Contract.throwError @Text "Couldn't find head"
    Just headX -> do
      let appInstance = getAppInstance $ pi'data headX
          newNode = createNewNode appInstance params user
          nftPolicy = mintPolicy appInstance
      (InsertPoint lNode rNode) <- findInsertPoint uT newNode
      (lLk, lCx) <- updateNodePointer appInstance lNode newNode
      (nLk, nCx) <- mintNode uT nftPolicy newNode rNode
      let lookups = mconcat [lLk, nLk]
          tx = mconcat [lCx, nCx]
      void $ Contract.submitTxConstraintsWith lookups tx
      Contract.tell . Last . Just . Left . info'id . node'information $ newNode
      Contract.logInfo @Hask.String $ printf "mint successful!"
  where
    createNewNode :: NftAppInstance -> MintParams -> UserId -> NftListNode
    createNewNode appInstance mp author =
      NftListNode
        { node'information' = toSpooky $ mintParamsToInfo mp author
        , node'next' = toSpooky @(Maybe Pointer) Nothing
        , node'appInstance' = toSpooky appInstance
        }

    findInsertPoint :: UniqueToken -> NftListNode -> GenericContract (InsertPoint DatumNft)
    findInsertPoint uT' node = do
      list <- getDatumsTxsOrdered uT'
      case list of
        [] -> Contract.throwError "This Should never happen."
        x : xs -> findPoint x xs
      where
        findPoint :: PointInfo DatumNft -> [PointInfo DatumNft] -> GenericContract (InsertPoint DatumNft)
        findPoint x = \case
          [] -> pure $ InsertPoint x Nothing
          (y : ys) ->
            case compare (pi'data y) (NodeDatum node) of
              LT -> findPoint y ys
              EQ -> Contract.throwError @Text "NFT already minted."
              GT -> pure $ InsertPoint x (Just y)

    -- mintNode ::
    --   UniqueToken ->
    --   MintingPolicy ->
    --   NftListNode ->
    --   Maybe (PointInfo DatumNft) ->
    --   GenericContract (Constraints.ScriptLookups Any, Constraints.TxConstraints i0 DatumNft)
    mintNode uT' mintingP newNode nextNode = do
      appSymbol <- getNftAppSymbol uT'
      let newTokenValue = Value.singleton (unSpookyCurrencySymbol . app'symbol $ appSymbol) (TokenName . getDatumValue . NodeDatum $ newNode) 1
          aSymbol = app'symbol appSymbol
          newTokenDatum =
            NodeDatum $
              newNode
                { node'next' = toSpooky (Pointer . toSpooky . toSpookyAssetClass . assetClass (unSpookyCurrencySymbol aSymbol) . TokenName . getDatumValue . pi'data <$> nextNode)
                }

          mintRedeemer = asRedeemer . Mint . toSpooky . NftId . toSpooky . getDatumValue . NodeDatum $ newNode

          lookups =
            mconcat
              [ Constraints.typedValidatorLookups (txPolicy uT')
              , Constraints.otherScript (validatorScript $ txPolicy uT')
              , Constraints.mintingPolicy mintingP
              ]
          tx =
            mconcat
              [ Constraints.mustPayToTheScript (toBuiltinData newTokenDatum) newTokenValue
              , Constraints.mustMintValueWithRedeemer mintRedeemer newTokenValue
              ]
      pure (lookups, tx)

    -- updateNodePointer ::
    --   NftAppInstance ->
    --   PointInfo DatumNft ->
    --   NftListNode ->
    --   GenericContract (Constraints.ScriptLookups Any, Constraints.TxConstraints i0 DatumNft)
    updateNodePointer appInstance insertPoint newNode = do
      appSymbol <- getNftAppSymbol (appInstance'UniqueToken appInstance)
      let scriptAddr = appInstance'Address . node'appInstance $ newNode
          token =
            Ledger.txOutValue
              . fst
              $ (txOutRefMapForAddr (unSpookyAddress scriptAddr) (pi'CITx insertPoint) Map.! pi'TOR insertPoint)
          newToken = assetClass (unSpookyCurrencySymbol . app'symbol $ appSymbol) (TokenName .getDatumValue . NodeDatum $ newNode)
          newDatum = updatePointer (Pointer . toSpooky . toSpookyAssetClass $ newToken)
          oref = pi'TOR insertPoint
          redeemer = asRedeemer $ MintAct (toSpooky . NftId . toSpooky . getDatumValue . NodeDatum $ newNode) (toSpooky appSymbol)
          oldDatum = pi'data insertPoint

          updatePointer :: Pointer -> DatumNft
          updatePointer newPointer =
            case oldDatum of
              HeadDatum (NftListHead _ a) -> HeadDatum $ NftListHead (toSpooky $ Just newPointer) a
              NodeDatum (NftListNode i _ a) -> NodeDatum $ NftListNode i (toSpooky $ Just newPointer) a

          lookups =
            mconcat
              [ Constraints.typedValidatorLookups (txPolicy uT)
              , Constraints.otherScript (validatorScript $ txPolicy uT)
              , Constraints.unspentOutputs $ Map.singleton (pi'TOR insertPoint) (pi'CITxO insertPoint)
              ]
          tx =
            mconcat
              [ Constraints.mustPayToTheScript (toBuiltinData newDatum) token
              , Constraints.mustSpendScriptOutput oref redeemer
              ]
      pure (lookups, tx)

mintParamsToInfo :: MintParams -> UserId -> InformationNft
mintParamsToInfo MintParams {..} author =
  InformationNft
    { info'id' = toSpooky $ nftIdInit mp'content
    , info'share' = toSpooky mp'share
    , info'price' = toSpooky mp'price
    , info'owner' = toSpooky author
    , info'author' = toSpooky author
    , info'auctionState' = toSpooky @(Maybe AuctionState) Nothing
    }
  where
    nftIdInit = NftId . toSpooky . hashData
