module Mlabs.NFT.Contract.Gov.Query (
  querryCurrentStake,
  queryCurrFeeRate,
  queryFeePkh,
) where

import Prelude qualified as Hask

import Data.Monoid ((<>))
import Data.Text (Text)

import Plutus.Contract (Contract)
import Plutus.Contract qualified as Contract
import PlutusTx.Prelude hiding (mconcat, mempty, (<>))

import Ledger (
  getPubKeyHash,
  scriptCurrencySymbol,
 )
import Ledger qualified
import Ledger.Value as Value (TokenName (..), valueOf)

import Mlabs.Data.LinkedList
import Mlabs.NFT.Contract.Aux
import Mlabs.NFT.Contract.Gov.Aux
import Mlabs.NFT.Governance.Types
import Mlabs.NFT.Governance.Validation
import Mlabs.NFT.Spooky (unSpookyAddress, unSpookyPubKeyHash)
import Mlabs.NFT.Types

-- | Returns current `listGov` stake for user
querryCurrentStake ::
  forall s.
  UniqueToken ->
  () ->
  Contract UserWriter s Text Integer
querryCurrentStake uT _ = do
  user <- getUId
  nftHead' <- getNftHead uT
  nftHead <- case nftHead' of
    Just (PointInfo (HeadDatum x) _ _ _) -> Hask.pure x
    _ -> Contract.throwError "queryCurrentStake: NFT HEAD not found"
  let ownPkh = getUserId user
      listGovTokenName = TokenName . ("listGov" <>) . getPubKeyHash . unSpookyPubKeyHash $ ownPkh
      newGovDatum = GovDatum $ NodeLList user GovLNode Nothing
      appInstance = head'appInstance nftHead
      govAddr = unSpookyAddress . appInstance'Governance $ appInstance
      govCurr = scriptCurrencySymbol govPolicy
      govPolicy = govMintPolicy appInstance
  currGov <- findGov govAddr newGovDatum
  let nodeValue = piValue govAddr currGov
  Hask.pure $ valueOf nodeValue govCurr listGovTokenName
  where
    findGov addr node = do
      list <- getDatumsTxsOrderedFromAddr @GovDatum addr
      findPoint list
      where
        findPoint = \case
          (x1 : xs) ->
            if pi'data x1 == node
              then pure x1
              else findPoint xs
          _ -> Contract.throwError "GOV node not found"

queryGovHeadDatum :: forall w s. UniqueToken -> Contract w s Text GovLHead
queryGovHeadDatum uT = do
  nftHead' <- getNftHead uT
  nftHead <- case pi'data <$> nftHead' of
    Just (HeadDatum x) -> Hask.pure x
    _ -> Contract.throwError "queryCurrFeeRate: NFT HEAD not found"

  let govAddr = unSpookyAddress . appInstance'Governance . head'appInstance $ nftHead
  govHead' <- getGovHead govAddr
  case gov'list . pi'data <$> govHead' of
    Just (HeadLList x _) -> Hask.pure x
    _ -> Contract.throwError "queryCurrFeeRate: GOV HEAD not found"

-- | Get fee rate from GOV HEAD
queryCurrFeeRate :: forall w s. UniqueToken -> Contract w s Text Rational
queryCurrFeeRate uT = do
  govHead <- queryGovHeadDatum uT
  Hask.pure $ govLHead'feeRate govHead

-- | Get fee pkh from GOV HEAD
queryFeePkh :: forall w s. UniqueToken -> Contract w s Text Ledger.PubKeyHash
queryFeePkh uT = do
  govHead <- queryGovHeadDatum uT
  Hask.pure $ govLHead'pkh govHead
