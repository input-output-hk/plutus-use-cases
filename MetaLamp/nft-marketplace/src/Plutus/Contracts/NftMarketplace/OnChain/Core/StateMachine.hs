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

import           Control.Lens                                     ((&), (.~),
                                                                   (?~), (^.))
import qualified Control.Lens                                     as Lens
import qualified Data.Aeson                                       as J
import qualified Data.Text                                        as T
import qualified Ext.Plutus.Contracts.Auction                     as Auction
import qualified Ext.PlutusTx.AssocMap                            as AssocMap
import qualified GHC.Generics                                     as Haskell
import           Ledger
import qualified Ledger.Constraints                               as Constraints
import qualified Ledger.Typed.Scripts                             as Scripts
import qualified Ledger.Value                                     as V
import           Plutus.Contract
import           Plutus.Contract.StateMachine
import           Plutus.Contracts.NftMarketplace.OnChain.Core.NFT
import qualified Plutus.Contracts.Services.Sale                   as Sale
import qualified PlutusTx
import qualified PlutusTx.AssocMap                                as AssocMap
import           PlutusTx.Prelude                                 hiding
                                                                  (Semigroup (..))
import           Prelude                                          (Semigroup (..))
import qualified Prelude                                          as Haskell

newtype Marketplace =
  Marketplace
    { marketplaceProtocolToken :: AssetClass
    }
  deriving stock (Haskell.Eq, Haskell.Ord, Haskell.Show, Haskell.Generic)
  deriving anyclass (J.ToJSON, J.FromJSON)

PlutusTx.makeLift ''Marketplace

-- TODO make sum types for eithers (?)
data MarketplaceRedeemer
  = CreateNftRedeemer IpfsCidHash NftInfo
  | PutLotRedeemer (Either (IpfsCidHash, IpfsCid) (AssocMap.Map IpfsCidHash IpfsCid)) LotLink
  | RemoveLotRedeemer (Either IpfsCidHash BundleId)
  deriving  (Haskell.Show)

PlutusTx.unstableMakeIsData ''MarketplaceRedeemer

PlutusTx.makeLift ''MarketplaceRedeemer

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

{-# INLINABLE insertNft #-}
insertNft :: IpfsCidHash
                      -> NFT -> MarketplaceDatum -> MarketplaceDatum
insertNft ipfsCidHash nftEntry store@MarketplaceDatum{..} =
    store { mdSingletons = AssocMap.insert ipfsCidHash nftEntry mdSingletons }

{-# INLINABLE nftUnion #-}
nftUnion :: MarketplaceDatum -> AssocMap.Map IpfsCidHash NFT
nftUnion MarketplaceDatum{..} = foldr union mdSingletons $ fmap getNfts $ toList mdBundles
  where
    union = AssocMap.unionWith const
    getNfts NftBundle{..} = case nbTokens of
      NoLot val      -> fmap (\info -> NFT info Nothing) val
      HasLot val lot -> fmap (\(cid, info) -> NFT info (Just (cid, lot))) val

{-# INLINABLE transition #-}
transition :: Marketplace -> State MarketplaceDatum -> MarketplaceRedeemer -> Maybe (TxConstraints Void Void, State MarketplaceDatum)
transition marketplace state redeemer = case redeemer of
    CreateNftRedeemer ipfsCidHash nftEntry
    -- TODO check that ipfsCidHash is a hash (?)
        -> Just ( mustBeSignedByIssuer nftEntry
                , State (insertNft ipfsCidHash (NFT nftEntry Nothing) nftStore) currStateValue
                )
    PutLotRedeemer (Left (ipfsCidHash, ipfsCid)) lot
        -> let newEntry = maybe (traceError "NFT has not been created.") (_nftLot ?~ (ipfsCid, lot)) $
                            AssocMap.lookup ipfsCidHash $ mdSingletons nftStore
           in  Just ( mempty
                    , State (insertNft ipfsCidHash newEntry nftStore) currStateValue
                    )
    RemoveLotRedeemer (Left ipfsCidHash)
        -> let newEntry = maybe (traceError "NFT has not been created.") (_nftLot .~ Nothing) $
                            AssocMap.lookup ipfsCidHash $ mdSingletons nftStore
           in  Just ( mempty
                    , State (insertNft ipfsCidHash newEntry nftStore) currStateValue
                    )
    _                                        -> trace "Invalid transition" Nothing
  where
    stateToken :: Value
    stateToken = V.assetClassValue (marketplaceProtocolToken marketplace) 1

    nftStore :: MarketplaceDatum
    nftStore = stateData state

    currStateValue = stateValue state - stateToken

    mustBeSignedByIssuer entry = case niIssuer entry of
      Just pkh -> Constraints.mustBeSignedBy pkh
      Nothing  -> mempty

{-# INLINABLE stateTransitionCheck #-}
stateTransitionCheck :: MarketplaceDatum -> MarketplaceRedeemer -> ScriptContext -> Bool
stateTransitionCheck nftStore (CreateNftRedeemer ipfsCidHash nftEntry) ctx =
  traceIfFalse "CreateNftRedeemer: " $
  traceIfFalse "NFT entry already exists" $
    isNothing $ AssocMap.lookup ipfsCidHash $ nftUnion nftStore
stateTransitionCheck MarketplaceDatum {..} (PutLotRedeemer (Left (ipfsCidHash, ipfsCid)) lot) ctx =
  traceIfFalse "PutLotRedeemer: " $
  let nftEntry = fromMaybe (traceError "NFT has not been created") $ AssocMap.lookup ipfsCidHash mdSingletons
      lotValue = either Sale.saleValue (Auction.apAsset . Auction.fromTuple) lot
      nftValue = V.singleton (niCurrency $ nftRecord nftEntry) (V.TokenName ipfsCid) 1
      hasBeenPutOnSale = lotValue == nftValue
      isValidHash = sha2_256 ipfsCid == ipfsCidHash
-- TODO (?) check that there was no previous lot (isNothing $ nftLot nftEntry)
  in  traceIfFalse "NFT has not been put on sale or auction" hasBeenPutOnSale &&
      traceIfFalse "Invalid IPFS Cid Hash" isValidHash
stateTransitionCheck MarketplaceDatum {..} (RemoveLotRedeemer (Left ipfsCidHash)) ctx =
  traceIfFalse "RemoveLotRedeemer: " $
  let nftEntry = fromMaybe (traceError "NFT has not been created") $ AssocMap.lookup ipfsCidHash mdSingletons
      hasBeenPutOnSale = isJust $ nftLot nftEntry
  in  traceIfFalse "NFT has not been put on sale or auction" hasBeenPutOnSale
stateTransitionCheck _ _ _ = traceError "Transition disallowed"

{-# INLINABLE marketplaceStateMachine #-}
marketplaceStateMachine :: Marketplace -> StateMachine MarketplaceDatum MarketplaceRedeemer
marketplaceStateMachine marketplace = StateMachine
    { smTransition  = transition marketplace
    , smFinal       = const False
    , smCheck       = stateTransitionCheck
    , smThreadToken = Just $ marketplaceProtocolToken marketplace
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
