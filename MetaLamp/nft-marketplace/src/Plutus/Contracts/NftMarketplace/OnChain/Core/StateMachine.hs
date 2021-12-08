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
{-# LANGUAGE NamedFieldPuns #-}
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
import           Ledger.Ada                                               (toValue)
import qualified Ledger.Constraints                                       as Constraints
import qualified Ledger.Typed.Scripts                                     as Scripts
import qualified Ledger.Value                                             as V
import           Plutus.Abstract.Percentage                               (Percentage)
import           Plutus.Contract
import           Plutus.Contract.StateMachine
import           Plutus.Contracts.NftMarketplace.OnChain.Core.ID
import           Plutus.Contracts.NftMarketplace.OnChain.Core.Marketplace
import           Plutus.Contracts.NftMarketplace.OnChain.Core.NFT
import qualified Plutus.Contracts.Services.Sale                           as Sale
import qualified PlutusTx
import qualified PlutusTx.AssocMap                                        as AssocMap
import           PlutusTx.Prelude                                         hiding
                                                                          (Semigroup (..))
import           Prelude                                                  (Semigroup (..))
import qualified Prelude                                                  as Haskell

data RemoveLotRedeemerValue =
  RemoveNftLotRedeemer IpfsCidHash
  deriving  (Haskell.Show)

PlutusTx.unstableMakeIsData ''RemoveLotRedeemerValue

PlutusTx.makeLift ''RemoveLotRedeemerValue

data PutLotRedeemerValue =
  PutNftLotRedeemer InternalNftId LotLink
  deriving  (Haskell.Show)

PlutusTx.unstableMakeIsData ''PutLotRedeemerValue

PlutusTx.makeLift ''PutLotRedeemerValue

mkPutLotRedeemer :: InternalId -> LotLink -> MarketplaceRedeemer
mkPutLotRedeemer (NftInternalId nId) lot = PutLotRedeemer $ PutNftLotRedeemer nId lot

data MarketplaceRedeemer
  = CreateNftRedeemer IpfsCidHash NftInfo
  | PutLotRedeemer PutLotRedeemerValue
  | RemoveLotRedeemer RemoveLotRedeemerValue
  deriving  (Haskell.Show)

PlutusTx.unstableMakeIsData ''MarketplaceRedeemer

PlutusTx.makeLift ''MarketplaceRedeemer

mkRemoveLotRedeemer :: InternalId -> MarketplaceRedeemer
mkRemoveLotRedeemer (NftInternalId nId) = RemoveLotRedeemer . RemoveNftLotRedeemer $ iniIpfsCidHash  nId

data MarketplaceDatum =
  MarketplaceDatum
    {
      mdSingletons :: AssocMap.Map IpfsCidHash NFT
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

{-# INLINABLE transition #-}
transition :: Marketplace -> State MarketplaceDatum -> MarketplaceRedeemer -> Maybe (TxConstraints Void Void, State MarketplaceDatum)
transition marketplace@Marketplace{..} state redeemer = case redeemer of
    CreateNftRedeemer ipfsCidHash nftEntry
        -> Just ( mustBeSignedByIssuer nftEntry <>
                  Constraints.mustPayToPubKey marketplaceOperator (toValue marketplaceNFTFee)
                , State (insertNft ipfsCidHash (NFT nftEntry Nothing) nftStore) currStateValue
                )
    PutLotRedeemer (PutNftLotRedeemer (InternalNftId ipfsCidHash ipfsCid) lot)
        -> let newEntry = maybe (traceError "NFT has not been created.") (_nftLot ?~ (ipfsCid, lot)) $
                            AssocMap.lookup ipfsCidHash $ mdSingletons nftStore
           in  Just ( mempty
                    , State (insertNft ipfsCidHash newEntry nftStore) currStateValue
                    )
    RemoveLotRedeemer (RemoveNftLotRedeemer ipfsCidHash)
        -> let newEntry = maybe (traceError "NFT has not been created.") (_nftLot .~ Nothing) $
                            AssocMap.lookup ipfsCidHash $ mdSingletons nftStore
           in  Just ( mempty
                    , State (insertNft ipfsCidHash newEntry nftStore) currStateValue
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
stateTransitionCheck MarketplaceDatum {..} (CreateNftRedeemer ipfsCidHash nftEntry) ctx =
  traceIfFalse "CreateNftRedeemer: " $
  traceIfFalse "NFT entry already exists" $
    isNothing $ AssocMap.lookup ipfsCidHash $ mdSingletons
stateTransitionCheck MarketplaceDatum {..}  (PutLotRedeemer (PutNftLotRedeemer (InternalNftId ipfsCidHash ipfsCid) lot)) ctx =
  traceIfFalse "PutLotRedeemer: " $
  let nftEntry = fromMaybe (traceError "NFT has not been created") $ AssocMap.lookup ipfsCidHash mdSingletons
      lotValue = getLotValue lot
      hasBeenPutOnSale = lotValue == nftValue ipfsCid nftEntry
      isValidHash = sha2_256 ipfsCid == ipfsCidHash
      hasNoExistingLot = isNothing $ nftLot nftEntry
  in  traceIfFalse "NFT has not been put on sale" hasBeenPutOnSale &&
      traceIfFalse "Invalid IPFS Cid Hash" isValidHash &&
      traceIfFalse "NFT already has a lot" hasNoExistingLot
stateTransitionCheck MarketplaceDatum {..} (RemoveLotRedeemer (RemoveNftLotRedeemer ipfsCidHash)) ctx =
  traceIfFalse "RemoveLotRedeemer: " $
  let nftEntry = fromMaybe (traceError "NFT has not been created") $ AssocMap.lookup ipfsCidHash mdSingletons
      hasBeenPutOnSale = isJust $ nftLot nftEntry
  in  traceIfFalse "NFT has not been put on sale" hasBeenPutOnSale
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
