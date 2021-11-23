{-# LANGUAGE UndecidableInstances #-}

module Mlabs.NFT.Contract.Mint (
  mint,
  getDatumsTxsOrdered,
  mintParamsToInfo,
) where

import PlutusTx.Prelude hiding (mconcat, mempty, (<>))
import Prelude (mconcat)
import Prelude qualified as Hask

import Control.Monad (void)
import Data.Map qualified as Map
import Data.Monoid (Last (..), (<>))
import Data.Text (Text)
import Text.Printf (printf)

import Plutus.Contract (Contract)
import Plutus.Contract qualified as Contract

import Ledger (MintingPolicy)

import Ledger.Constraints qualified as Constraints
import Ledger.Typed.Scripts (validatorScript)
import Ledger.Value as Value (TokenName (..), assetClass, assetClassValue, singleton)

import Mlabs.NFT.Contract.Aux
import Mlabs.NFT.Types
import Mlabs.NFT.Validation

--------------------------------------------------------------------------------
-- MINT --

---- | Mints an NFT and sends it to the App Address.
mint :: forall s. NftAppSymbol -> MintParams -> Contract UserWriter s Text ()
mint symbol params = do
  user <- getUId
  head' <- getNftHead symbol
  case head' of
    Nothing -> Contract.throwError @Text "Couldn't find head"
    Just headX -> do
      let appInstance = getAppInstance $ pi'data headX
          newNode = createNewNode appInstance params user
          nftPolicy = mintPolicy appInstance
      (InsertPoint lNode rNode) <- findInsertPoint symbol newNode
      (lLk, lCx) <- updateNodePointer appInstance symbol lNode newNode
      (nLk, nCx) <- mintNode symbol nftPolicy newNode rNode
      let lookups = mconcat [lLk, nLk]
          tx = mconcat [lCx, nCx]
      void $ Contract.submitTxConstraintsWith @NftTrade lookups tx
      Contract.tell . Last . Just . Left . info'id . node'information $ newNode
      Contract.logInfo @Hask.String $ printf "mint successful!"
  where
    createNewNode :: NftAppInstance -> MintParams -> UserId -> NftListNode
    createNewNode appInstance mp author =
      NftListNode
        { node'information = mintParamsToInfo mp author
        , node'next = Nothing
        , node'appInstance = appInstance
        }

    findInsertPoint :: NftAppSymbol -> NftListNode -> GenericContract (InsertPoint DatumNft)
    findInsertPoint aSymbol node = do
      list <- getDatumsTxsOrdered aSymbol
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

    mintNode ::
      NftAppSymbol ->
      MintingPolicy ->
      NftListNode ->
      Maybe (PointInfo DatumNft) ->
      GenericContract (Constraints.ScriptLookups NftTrade, Constraints.TxConstraints i0 DatumNft)
    mintNode appSymbol mintingP newNode nextNode = pure (lookups, tx)
      where
        newTokenValue = Value.singleton (app'symbol appSymbol) (TokenName . getDatumValue . NodeDatum $ newNode) 1
        aSymbol = app'symbol appSymbol
        newTokenDatum =
          NodeDatum $
            newNode
              { node'next = Pointer . assetClass aSymbol . TokenName . getDatumValue . pi'data <$> nextNode
              }

        mintRedeemer = asRedeemer . Mint . NftId . getDatumValue . NodeDatum $ newNode

        lookups =
          mconcat
            [ Constraints.typedValidatorLookups txPolicy
            , Constraints.otherScript (validatorScript txPolicy)
            , Constraints.mintingPolicy mintingP
            ]
        tx =
          mconcat
            [ Constraints.mustPayToTheScript newTokenDatum newTokenValue
            , Constraints.mustMintValueWithRedeemer mintRedeemer newTokenValue
            ]

    updateNodePointer ::
      NftAppInstance ->
      NftAppSymbol ->
      PointInfo DatumNft ->
      NftListNode ->
      GenericContract (Constraints.ScriptLookups NftTrade, Constraints.TxConstraints i0 DatumNft)
    updateNodePointer appInstance appSymbol insertPoint newNode = do
      pure (lookups, tx)
      where
        token = Value.singleton (app'symbol appSymbol) (TokenName . getDatumValue . pi'data $ insertPoint) 1
        newToken = assetClass (app'symbol appSymbol) (TokenName .getDatumValue . NodeDatum $ newNode)
        newDatum = updatePointer (Pointer newToken)
        oref = pi'TOR insertPoint
        redeemer = asRedeemer $ MintAct (NftId . getDatumValue . NodeDatum $ newNode) symbol
        oldDatum = pi'data insertPoint
        uniqueToken = assetClassValue (appInstance'AppAssetClass appInstance) 1

        updatePointer :: Pointer -> DatumNft
        updatePointer newPointer =
          case oldDatum of
            HeadDatum (NftListHead _ a) -> HeadDatum $ NftListHead (Just newPointer) a
            NodeDatum (NftListNode i _ a) -> NodeDatum $ NftListNode i (Just newPointer) a

        lookups =
          mconcat
            [ Constraints.typedValidatorLookups txPolicy
            , Constraints.otherScript (validatorScript txPolicy)
            , Constraints.unspentOutputs $ Map.singleton (pi'TOR insertPoint) (pi'CITxO insertPoint)
            ]
        tx =
          mconcat
            [ case oldDatum of
                NodeDatum _ -> Constraints.mustPayToTheScript newDatum token
                HeadDatum _ -> Constraints.mustPayToTheScript newDatum (token <> uniqueToken)
            , Constraints.mustSpendScriptOutput oref redeemer
            ]

mintParamsToInfo :: MintParams -> UserId -> InformationNft
mintParamsToInfo MintParams {..} author =
  InformationNft
    { info'id = nftIdInit mp'content
    , info'share = mp'share
    , info'price = mp'price
    , info'owner = author
    , info'author = author
    , info'auctionState = Nothing
    }
  where
    nftIdInit = NftId . hashData
