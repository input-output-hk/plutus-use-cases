{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}

module Plutus.Contracts.NftMarketplace.OnChain.Core.StateMachine where

import           Control.Lens                                             ((&),
                                                                           (.~),
                                                                           (?~),
                                                                           (^.))
import qualified Control.Lens                                             as Lens
import qualified Data.Aeson                                               as J
import qualified Data.Text                                                as T
import qualified GHC.Generics                                             as Haskell
import           Ledger
import qualified Ledger.Constraints                                       as Constraints
import qualified Ledger.Typed.Scripts                                     as Scripts
import qualified Ledger.Value                                             as V
import           Plutus.Abstract.Percentage                               (Percentage)
import           Plutus.Contract
import           Plutus.Contract.StateMachine
import           Plutus.Contracts.NftMarketplace.OnChain.Core.ID
import           Plutus.Contracts.NftMarketplace.OnChain.Core.Marketplace
import           Plutus.Contracts.NftMarketplace.OnChain.Core.NFT
import qualified Plutus.Contracts.Services.Auction                        as Auction
import qualified Plutus.Contracts.Services.Sale                           as Sale
import qualified PlutusTx
import qualified PlutusTx.AssocMap                                        as AssocMap
import           PlutusTx.Prelude                                         hiding
                                                                          (Semigroup (..))
import           Prelude                                                  (Semigroup (..))
import qualified Prelude                                                  as Haskell

data RemoveLotRedeemerValue =
  RemoveNftLotRedeemer IpfsCidHash
  | RemoveBundleLotRedeemer BundleId
  deriving  (Haskell.Show)

PlutusTx.unstableMakeIsData ''RemoveLotRedeemerValue

PlutusTx.makeLift ''RemoveLotRedeemerValue

data PutLotRedeemerValue =
  PutNftLotRedeemer InternalNftId LotLink
  | PutBundleLotRedeemer InternalBundleId LotLink
  deriving  (Haskell.Show)

PlutusTx.unstableMakeIsData ''PutLotRedeemerValue

PlutusTx.makeLift ''PutLotRedeemerValue

mkPutLotRedeemer :: InternalId -> LotLink -> MarketplaceRedeemer
mkPutLotRedeemer (NftInternalId nId) lot = PutLotRedeemer $ PutNftLotRedeemer nId lot
mkPutLotRedeemer (BundleInternalId bId) lot = PutLotRedeemer $ PutBundleLotRedeemer bId lot

data MarketplaceRedeemer
  = CreateNftRedeemer IpfsCidHash NftInfo
  | ImportNftRedeemer IpfsCidHash NftInfo
  | PutLotRedeemer PutLotRedeemerValue
  | RemoveLotRedeemer RemoveLotRedeemerValue
  | BundleUpRedeemer [IpfsCidHash] BundleId BundleInfo
  | UnbundleRedeemer BundleId
  deriving  (Haskell.Show)

PlutusTx.unstableMakeIsData ''MarketplaceRedeemer

PlutusTx.makeLift ''MarketplaceRedeemer

mkRemoveLotRedeemer :: InternalId -> MarketplaceRedeemer
mkRemoveLotRedeemer (NftInternalId nId) = RemoveLotRedeemer . RemoveNftLotRedeemer $ iniIpfsCidHash  nId
mkRemoveLotRedeemer (BundleInternalId bId) = RemoveLotRedeemer . RemoveBundleLotRedeemer $ ibiBundleId bId

data MarketplaceDatum =
  MarketplaceDatum
    {
      mdSingletons :: AssocMap.Map IpfsCidHash NFT,
      mdBundles    :: AssocMap.Map BundleId NftBundle
    }
  deriving stock (Haskell.Eq, Haskell.Show, Haskell.Generic)
  deriving anyclass (J.ToJSON, J.FromJSON)

PlutusTx.unstableMakeIsData ''MarketplaceDatum

PlutusTx.makeLift ''MarketplaceDatum

Lens.makeClassy_ ''MarketplaceDatum

{-# INLINABLE insertNft #-}
insertNft :: IpfsCidHash
                      -> NFT -> MarketplaceDatum -> MarketplaceDatum
insertNft ipfsCidHash nftEntry store@MarketplaceDatum{..} =
    store { mdSingletons = AssocMap.insert ipfsCidHash nftEntry mdSingletons }

{-# INLINABLE insertBundle #-}
insertBundle :: BundleId
                      -> NftBundle -> MarketplaceDatum -> MarketplaceDatum
insertBundle bundleId bundle store@MarketplaceDatum{..} =
    store { mdBundles = AssocMap.insert bundleId bundle mdBundles }

{-# INLINABLE nftUnion #-}
nftUnion :: MarketplaceDatum -> AssocMap.Map IpfsCidHash NFT
nftUnion MarketplaceDatum{..} = foldr union mdSingletons $ fmap getNfts $ toList mdBundles
  where
    union = AssocMap.unionWith const
    getNfts NftBundle{..} = case nbTokens of
      NoLot val      -> fmap (\info -> NFT info Nothing) val
      HasLot val lot -> fmap (\(cid, info) -> NFT info (Just (cid, lot))) val

{-# INLINABLE bundleUpDatum #-}
bundleUpDatum :: [IpfsCidHash] -> BundleId -> BundleInfo -> MarketplaceDatum -> MarketplaceDatum
bundleUpDatum nftIds bundleId bundleInfo MarketplaceDatum{..} =
    MarketplaceDatum { mdSingletons = foldr AssocMap.delete mdSingletons nftIds
          , mdBundles = AssocMap.insert bundleId (makeBundle mdSingletons nftIds bundleInfo) mdBundles
          }

{-# INLINABLE unbundleDatum #-}
unbundleDatum :: BundleId -> MarketplaceDatum -> MarketplaceDatum
unbundleDatum bundleId MarketplaceDatum{..} =
    MarketplaceDatum { mdSingletons = foldr insert mdSingletons $ AssocMap.toList tokens
          , mdBundles = AssocMap.delete bundleId mdBundles
          }
  where
    bundle = fromMaybe (traceError "Bundle has not been created.") $
                            AssocMap.lookup bundleId mdBundles
    tokens = case nbTokens bundle of
      NoLot ts   -> ts
      HasLot _ _ -> traceError "Could not unbundle: bundle has lot."
    insert (nftId, record) = AssocMap.insert nftId $ NFT record Nothing

{-# INLINABLE addLotToBundle #-}
addLotToBundle
        :: AssocMap.Map IpfsCidHash IpfsCid -> LotLink -> NftBundle -> NftBundle
addLotToBundle cids lot NftBundle {..} = case nbTokens of
      NoLot tokens      -> NftBundle nbRecord $ HasLot (AssocMap.fromList $ fmap addCid $ AssocMap.toList tokens) lot
      HasLot _ _ -> traceError "Could not add lot: bundle has one."
    where
      addCid :: (IpfsCidHash, NftInfo) -> (IpfsCidHash, (IpfsCid, NftInfo))
      addCid (nftId, entry) = (nftId, (fromMaybe (traceError "NFT IPFS Cid not provided") $ AssocMap.lookup nftId cids, entry))

{-# INLINABLE removeLotFromBundle #-}
removeLotFromBundle :: NftBundle -> NftBundle
removeLotFromBundle NftBundle {..} = NftBundle nbRecord $ NoLot $ snd <$> tokens
  where
    tokens = case nbTokens of
      HasLot tokens _ -> tokens
      NoLot _         -> traceError "Could not remove lot: bundle has none."

{-# INLINABLE transition #-}
transition :: Marketplace -> State MarketplaceDatum -> MarketplaceRedeemer -> Maybe (TxConstraints Void Void, State MarketplaceDatum)
transition marketplace@Marketplace{..} state redeemer = case redeemer of
    CreateNftRedeemer ipfsCidHash nftEntry
        -> Just ( mustBeSignedByIssuer nftEntry <>
                  Constraints.mustPayToPubKey marketplaceOperator marketplaceNFTFee
                , State (insertNft ipfsCidHash (NFT nftEntry Nothing) nftStore) currStateValue
                )
    ImportNftRedeemer ipfsCidHash nftEntry
        -> Just ( mustBeSignedByIssuer nftEntry <>
                  Constraints.mustPayToPubKey marketplaceOperator marketplaceNFTFee
                , State (insertNft ipfsCidHash (NFT nftEntry Nothing) nftStore) currStateValue
                )
    PutLotRedeemer (PutNftLotRedeemer (InternalNftId ipfsCidHash ipfsCid) lot)
        -> let newEntry = maybe (traceError "NFT has not been created.") (_nftLot ?~ (ipfsCid, lot)) $
                            AssocMap.lookup ipfsCidHash $ mdSingletons nftStore
           in  Just ( mempty
                    , State (insertNft ipfsCidHash newEntry nftStore) currStateValue
                    )
    PutLotRedeemer (PutBundleLotRedeemer (InternalBundleId bundleId cids) lot)
        -> let newEntry = maybe (traceError "Bundle has not been created.") (addLotToBundle cids lot) $
                            AssocMap.lookup bundleId $ mdBundles nftStore
           in  Just ( mempty
                    , State (insertBundle bundleId newEntry nftStore) currStateValue
                    )
    RemoveLotRedeemer (RemoveNftLotRedeemer ipfsCidHash)
        -> let newEntry = maybe (traceError "NFT has not been created.") (_nftLot .~ Nothing) $
                            AssocMap.lookup ipfsCidHash $ mdSingletons nftStore
           in  Just ( mempty
                    , State (insertNft ipfsCidHash newEntry nftStore) currStateValue
                    )
    RemoveLotRedeemer (RemoveBundleLotRedeemer bundleId)
        -> let newEntry = maybe (traceError "NFT has not been created.") removeLotFromBundle $
                            AssocMap.lookup bundleId $ mdBundles nftStore
           in  Just ( mempty
                    , State (insertBundle bundleId newEntry nftStore) currStateValue
                    )
    BundleUpRedeemer nftIds bundleId bundleInfo
        -> Just ( Constraints.mustPayToPubKey marketplaceOperator marketplaceNFTFee
                , State (bundleUpDatum nftIds bundleId bundleInfo nftStore) currStateValue
                )
    UnbundleRedeemer bundleId
        -> Just ( mempty
                , State (unbundleDatum bundleId nftStore) currStateValue
                )
    _                                        -> trace "Invalid transition" Nothing
  where
    nftStore :: MarketplaceDatum
    nftStore = stateData state

    currStateValue = stateValue state

    mustBeSignedByIssuer entry = case niIssuer entry of
      Just pkh -> Constraints.mustBeSignedBy pkh
      Nothing  -> mempty

{-# INLINABLE stateTransitionCheck #-}
stateTransitionCheck :: MarketplaceDatum -> MarketplaceRedeemer -> ScriptContext -> Bool
stateTransitionCheck nftStore (CreateNftRedeemer ipfsCidHash nftEntry) ctx =
  traceIfFalse "CreateNftRedeemer: " $
  traceIfFalse "NFT entry already exists" $
    isNothing $ AssocMap.lookup ipfsCidHash $ nftUnion nftStore
stateTransitionCheck MarketplaceDatum {..}  (PutLotRedeemer (PutNftLotRedeemer (InternalNftId ipfsCidHash ipfsCid) lot)) ctx =
  traceIfFalse "PutLotRedeemer: " $
  let nftEntry = fromMaybe (traceError "NFT has not been created") $ AssocMap.lookup ipfsCidHash mdSingletons
      lotValue = getLotValue lot
      hasBeenPutOnSale = lotValue == nftValue ipfsCid nftEntry
      isValidHash = sha2_256 ipfsCid == ipfsCidHash
      hasNoExistingLot = isNothing $ nftLot nftEntry
  in  traceIfFalse "NFT has not been put on sale or auction" hasBeenPutOnSale &&
      traceIfFalse "Invalid IPFS Cid Hash" isValidHash &&
      traceIfFalse "NFT already has a lot" hasNoExistingLot
stateTransitionCheck MarketplaceDatum {..} (PutLotRedeemer (PutBundleLotRedeemer (InternalBundleId bundleId cids) lot)) ctx =
  traceIfFalse "PutLotRedeemer: " $
  let bundle = fromMaybe (traceError "Bundle has not been created") $ AssocMap.lookup bundleId mdBundles
      lotValue = getLotValue lot
      cidHashes = case nbTokens bundle of
          NoLot tokens    -> AssocMap.keys tokens
          HasLot tokens _ -> AssocMap.keys tokens
      allCidsProvided = all (isJust . (`AssocMap.lookup` cids)) cidHashes
      hasBeenPutOnSale = bundleValue cids bundle == lotValue
      isValidHash (ipfsCidHash, ipfsCid) = sha2_256 ipfsCid == ipfsCidHash
      hasValidHashes = all isValidHash $ AssocMap.toList cids
      hasNoExistingLot = not $ hasLotBundle bundle
  in  traceIfFalse "Bundle has not been put on sale or auction" hasBeenPutOnSale &&
      traceIfFalse "Not all IPFS Cids provided" allCidsProvided &&
      traceIfFalse "Invalid IPFS Cid Hash provided" hasValidHashes &&
      traceIfFalse "Bundle already has a lot" hasNoExistingLot
stateTransitionCheck MarketplaceDatum {..} (RemoveLotRedeemer (RemoveNftLotRedeemer ipfsCidHash)) ctx =
  traceIfFalse "RemoveLotRedeemer: " $
  let nftEntry = fromMaybe (traceError "NFT has not been created") $ AssocMap.lookup ipfsCidHash mdSingletons
      hasBeenPutOnSale = isJust $ nftLot nftEntry
  in  traceIfFalse "NFT has not been put on sale or auction" hasBeenPutOnSale
stateTransitionCheck MarketplaceDatum {..} (RemoveLotRedeemer (RemoveBundleLotRedeemer bundleId)) ctx =
  traceIfFalse "RemoveLotRedeemer: " $
  let bundle = fromMaybe (traceError "Bundle has not been created") $ AssocMap.lookup bundleId mdBundles
      hasLot = hasLotBundle bundle
  in  traceIfFalse "Bundle has not been put on sale or auction" hasLot
stateTransitionCheck MarketplaceDatum {..} (BundleUpRedeemer nftIds bundleId bundleInfo) ctx =
  traceIfFalse "BundleUpRedeemer: " $
  let doesNotExist = isNothing $ AssocMap.lookup bundleId mdBundles
      notEmty = not $ null nftIds
      nfts =  fromMaybe (traceError "NFT does not exist or is part of existing bundle") . (`AssocMap.lookup` mdSingletons) <$> nftIds
      doesNotHaveLots = all (isNothing . nftLot) nfts
  in  traceIfFalse "Bundle entry already exists" doesNotExist &&
      traceIfFalse "Bundle is empty" notEmty &&
      traceIfFalse "One of NFTs has a lot" doesNotHaveLots
stateTransitionCheck MarketplaceDatum {..} (UnbundleRedeemer bundleId) ctx =
  traceIfFalse "UnbundleRedeemer: " $
  let bundle = fromMaybe (traceError "Bundle does not exist") $ AssocMap.lookup bundleId mdBundles
      doesNotHaveLot = not $ hasLotBundle bundle
  in  traceIfFalse "Bundle has a lot" doesNotHaveLot
stateTransitionCheck _ _ _ = traceError "Transition disallowed"

{-# INLINABLE marketplaceStateMachine #-}
marketplaceStateMachine :: Marketplace -> StateMachine MarketplaceDatum MarketplaceRedeemer
marketplaceStateMachine marketplace = StateMachine
    { smTransition  = transition marketplace
    , smFinal       = const False
    , smCheck       = stateTransitionCheck
    , smThreadToken = Nothing
    }

{-# INLINABLE mkMarketplaceValidator #-}
mkMarketplaceValidator :: Marketplace -> MarketplaceDatum -> MarketplaceRedeemer -> ScriptContext -> Bool
mkMarketplaceValidator marketplace = mkValidator $ marketplaceStateMachine marketplace

type MarketplaceScript = StateMachine MarketplaceDatum MarketplaceRedeemer

marketplaceInst :: Marketplace -> Scripts.TypedValidator MarketplaceScript
marketplaceInst marketplace = Scripts.mkTypedValidator @MarketplaceScript
    ($$(PlutusTx.compile [|| mkMarketplaceValidator ||])
        `PlutusTx.applyCode` PlutusTx.liftCode marketplace)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @MarketplaceDatum @MarketplaceRedeemer

marketplaceClient :: Marketplace -> StateMachineClient MarketplaceDatum MarketplaceRedeemer
marketplaceClient marketplace = mkStateMachineClient $ StateMachineInstance (marketplaceStateMachine marketplace) (marketplaceInst marketplace)
