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

import           Control.Lens                   ((&), (.~), (?~), (^.))
import qualified Control.Lens                   as Lens
import qualified Data.Aeson                     as J
import qualified Data.Text                      as T
import qualified GHC.Generics                   as Haskell
import           Ledger
import qualified Ledger.Constraints             as Constraints
import qualified Ledger.Typed.Scripts           as Scripts
import qualified Ledger.Value                   as V
import           Plutus.Contract
import           Plutus.Contract.StateMachine
import qualified Plutus.Contracts.Services.Sale as Sale
import qualified PlutusTx
import qualified PlutusTx.AssocMap              as AssocMap
import           PlutusTx.Prelude               hiding (Semigroup (..))
import           Prelude                        (Semigroup (..))
import qualified Prelude                        as Haskell

newtype Marketplace =
  Marketplace
    { marketplaceProtocolToken :: AssetClass
    }
  deriving stock (Haskell.Eq, Haskell.Ord, Haskell.Show, Haskell.Generic)
  deriving anyclass (J.ToJSON, J.FromJSON)

PlutusTx.makeLift ''Marketplace

type IpfsCidHash = ByteString
type Sale = (AssetClass, Sale.LovelacePrice, Value)

data Lot = Lot
                { lotSale    ::  Sale
                , lotIpfsCid :: ByteString
                }
  deriving stock (Haskell.Eq, Haskell.Show, Haskell.Generic)
  deriving anyclass (J.ToJSON, J.FromJSON)

PlutusTx.unstableMakeIsData ''Lot

PlutusTx.makeLift ''Lot

Lens.makeClassy_ ''Lot

data NFT =
  NFT
    { nftId          :: CurrencySymbol
    , nftName        :: ByteString
    , nftDescription :: ByteString
    , nftIssuer      :: Maybe PubKeyHash
    , nftSale        :: Maybe Lot
    }
  deriving stock (Haskell.Eq, Haskell.Show, Haskell.Generic)
  deriving anyclass (J.ToJSON, J.FromJSON)

PlutusTx.unstableMakeIsData ''NFT

PlutusTx.makeLift ''NFT

Lens.makeClassy_ ''NFT

data MarketplaceRedeemer
  = CreateNftRedeemer IpfsCidHash NFT
  | OpenSaleRedeemer IpfsCidHash Lot
  | BuyNftRedeemer IpfsCidHash
  | CloseSaleRedeemer IpfsCidHash
  deriving  (Haskell.Show)

PlutusTx.unstableMakeIsData ''MarketplaceRedeemer

PlutusTx.makeLift ''MarketplaceRedeemer

newtype MarketplaceDatum =
  MarketplaceDatum
    { getMarketplaceDatum :: AssocMap.Map IpfsCidHash NFT
    }
  deriving  (Haskell.Show)

PlutusTx.unstableMakeIsData ''MarketplaceDatum

PlutusTx.makeLift ''MarketplaceDatum

{-# INLINABLE transition #-}
transition :: Marketplace -> State MarketplaceDatum -> MarketplaceRedeemer -> Maybe (TxConstraints Void Void, State MarketplaceDatum)
transition marketplace state redeemer = case redeemer of
    CreateNftRedeemer ipfsCidHash nftEntry
    -- TODO check that ipfsCidHash is a hash (?)
        -> Just ( mustBeSignedByIssuer nftEntry
                , State (MarketplaceDatum $ AssocMap.insert ipfsCidHash nftEntry nftStore) val
                )
    OpenSaleRedeemer ipfsCidHash lot
        -> let newEntry = maybe (traceError "NFT has not been created.") (_nftSale ?~ lot) $
                            AssocMap.lookup ipfsCidHash nftStore
           in  Just ( mempty
                    , State (MarketplaceDatum $ AssocMap.insert ipfsCidHash newEntry nftStore) val
                    )
    BuyNftRedeemer ipfsCidHash
        -> let newEntry = maybe (traceError "NFT has not been created.") (_nftSale .~ Nothing) $
                            AssocMap.lookup ipfsCidHash nftStore
           in  Just ( mempty
                    , State (MarketplaceDatum $ AssocMap.insert ipfsCidHash newEntry nftStore) val
                    )
    CloseSaleRedeemer ipfsCidHash
        -> let newEntry = maybe (traceError "NFT has not been created.") (_nftSale .~ Nothing) $
                            AssocMap.lookup ipfsCidHash nftStore
           in  Just ( mempty
                    , State (MarketplaceDatum $ AssocMap.insert ipfsCidHash newEntry nftStore) val
                    )
    _                                        -> Nothing
  where
    nftStore :: AssocMap.Map IpfsCidHash NFT
    nftStore = getMarketplaceDatum $ stateData state

    val = stateValue state

    mustBeSignedByIssuer entry = case nftIssuer entry of
      Just pkh -> Constraints.mustBeSignedBy pkh
      Nothing  -> mempty

{-# INLINABLE stateTransitionCheck #-}
stateTransitionCheck :: MarketplaceDatum -> MarketplaceRedeemer -> ScriptContext -> Bool
stateTransitionCheck (MarketplaceDatum nftStore) (CreateNftRedeemer ipfsCidHash nftEntry) ctx =
  traceIfFalse "CreateNftRedeemer: " $
  traceIfFalse "NFT entry already exists" $
    isNothing $ AssocMap.lookup ipfsCidHash nftStore
stateTransitionCheck (MarketplaceDatum nftStore) (OpenSaleRedeemer ipfsCidHash lot) ctx =
  traceIfFalse "OpenSaleRedeemer: " $
  let nftEntry = fromMaybe (traceError "NFT has not been created") $ AssocMap.lookup ipfsCidHash nftStore
      nftIpfsCid = lotIpfsCid lot
      sale = Sale.fromTuple $ lotSale lot
      nftValue = V.singleton (nftId nftEntry) (V.TokenName nftIpfsCid) 1
      hasBeenPutOnSale = Sale.saleValue sale == nftValue
      isValidHash = sha2_256 nftIpfsCid == ipfsCidHash
  in  traceIfFalse "NFT has not been put on sale" hasBeenPutOnSale &&
      traceIfFalse "Invalid IPFS Cid Hash" isValidHash
stateTransitionCheck (MarketplaceDatum nftStore) (BuyNftRedeemer ipfsCidHash) ctx =
  traceIfFalse "BuyNftRedeemer: " $
  let nftEntry = fromMaybe (traceError "NFT has not been created") $ AssocMap.lookup ipfsCidHash nftStore
      hasBeenPutOnSale = isJust $ nftSale nftEntry
  in  traceIfFalse "NFT has not been put on sale" hasBeenPutOnSale
stateTransitionCheck (MarketplaceDatum nftStore) (CloseSaleRedeemer ipfsCidHash) ctx =
  traceIfFalse "CloseSaleRedeemer: " $
  let nftEntry = fromMaybe (traceError "NFT has not been created") $ AssocMap.lookup ipfsCidHash nftStore
      hasBeenPutOnSale = isJust $ nftSale nftEntry
  in  traceIfFalse "NFT has not been put on sale" hasBeenPutOnSale

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
