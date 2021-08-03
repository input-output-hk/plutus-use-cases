{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
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

data MarketplaceRedeemer
  = CreateNftRedeemer IpfsCidHash NFT
  | PutLotRedeemer IpfsCidHash Lot
  | RemoveLotRedeemer IpfsCidHash
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

PlutusTx.unstableMakeIsData ''MarketplaceDatum

PlutusTx.makeLift ''MarketplaceDatum

{-# INLINABLE transition #-}
transition :: Marketplace -> State MarketplaceDatum -> MarketplaceRedeemer -> Maybe (TxConstraints Void Void, State MarketplaceDatum)
transition marketplace state redeemer = case redeemer of
    CreateNftRedeemer ipfsCidHash nftEntry
    -- TODO check that ipfsCidHash is a hash (?)
        -> Just ( mustBeSignedByIssuer nftEntry
                , State (MarketplaceDatum $ AssocMap.insert ipfsCidHash nftEntry nftStore) currStateValue
                )
    PutLotRedeemer ipfsCidHash lot
        -> let newEntry = maybe (traceError "NFT has not been created.") (_nftLot ?~ lot) $
                            AssocMap.lookup ipfsCidHash nftStore
           in  Just ( mempty
                    , State (MarketplaceDatum $ AssocMap.insert ipfsCidHash newEntry nftStore) currStateValue
                    )
    RemoveLotRedeemer ipfsCidHash
        -> let newEntry = maybe (traceError "NFT has not been created.") (_nftLot .~ Nothing) $
                            AssocMap.lookup ipfsCidHash nftStore
           in  Just ( mempty
                    , State (MarketplaceDatum $ AssocMap.insert ipfsCidHash newEntry nftStore) currStateValue
                    )
    _                                        -> Nothing
  where
    stateToken :: Value
    stateToken = V.assetClassValue (marketplaceProtocolToken marketplace) 1

    nftStore :: AssocMap.Map IpfsCidHash NFT
    nftStore = getMarketplaceDatum $ stateData state

    currStateValue = stateValue state - stateToken

    mustBeSignedByIssuer entry = case nftIssuer entry of
      Just pkh -> Constraints.mustBeSignedBy pkh
      Nothing  -> mempty

{-# INLINABLE stateTransitionCheck #-}
stateTransitionCheck :: MarketplaceDatum -> MarketplaceRedeemer -> ScriptContext -> Bool
stateTransitionCheck (MarketplaceDatum nftStore) (CreateNftRedeemer ipfsCidHash nftEntry) ctx =
  traceIfFalse "CreateNftRedeemer: " $
  traceIfFalse "NFT entry already exists" $
    isNothing $ AssocMap.lookup ipfsCidHash nftStore
stateTransitionCheck (MarketplaceDatum nftStore) (PutLotRedeemer ipfsCidHash lot) ctx =
  traceIfFalse "PutLotRedeemer: " $
  let nftEntry = fromMaybe (traceError "NFT has not been created") $ AssocMap.lookup ipfsCidHash nftStore
      nftIpfsCid = lotIpfsCid lot
      lotValue = either Sale.saleValue (Auction.apAsset . Auction.fromTuple) $ lotLink lot
      nftValue = V.singleton (nftId nftEntry) (V.TokenName nftIpfsCid) 1
      hasBeenPutOnSale = lotValue == nftValue
      isValidHash = sha2_256 nftIpfsCid == ipfsCidHash
-- TODO (?) check that there was no previous lot (isNothing $ nftLot nftEntry)
  in  traceIfFalse "NFT has not been put on sale or auction" hasBeenPutOnSale &&
      traceIfFalse "Invalid IPFS Cid Hash" isValidHash
stateTransitionCheck (MarketplaceDatum nftStore) (RemoveLotRedeemer ipfsCidHash) ctx =
  traceIfFalse "RemoveLotRedeemer: " $
  let nftEntry = fromMaybe (traceError "NFT has not been created") $ AssocMap.lookup ipfsCidHash nftStore
      hasBeenPutOnSale = isJust $ nftLot nftEntry
  in  traceIfFalse "NFT has not been put on sale or auction" hasBeenPutOnSale

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
