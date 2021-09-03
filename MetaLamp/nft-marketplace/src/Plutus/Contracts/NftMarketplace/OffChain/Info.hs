{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators         #-}

module Plutus.Contracts.NftMarketplace.OffChain.Info where

import           Control.Lens                                 (_2, _Left,
                                                               _Right, (^.),
                                                               (^?))
import qualified Control.Lens                                 as Lens
import           Control.Monad                                hiding (fmap)
import qualified Data.Aeson                                   as J
import           Data.Proxy                                   (Proxy (..))
import           Data.Text                                    (Text)
import qualified Data.Text                                    as T
import qualified Ext.Plutus.Contracts.Auction                 as Auction
import           Ext.Plutus.Ledger.Value                      (utxoValue)
import qualified GHC.Generics                                 as Haskell
import           Ledger
import qualified Ledger.Typed.Scripts                         as Scripts
import           Ledger.Typed.Tx
import           Ledger.Value
import           Plutus.Abstract.ContractResponse             (ContractResponse,
                                                               withContractResponse)
import           Plutus.Contract
import           Plutus.Contract.StateMachine
import           Plutus.Contracts.Currency                    as Currency
import           Plutus.Contracts.NftMarketplace.OffChain.ID
import qualified Plutus.Contracts.NftMarketplace.OnChain.Core as Core
import qualified PlutusTx
import qualified PlutusTx.AssocMap                            as AssocMap
import           PlutusTx.Prelude                             hiding
                                                              (Semigroup (..))
import           Prelude                                      (Semigroup (..))
import qualified Prelude                                      as Haskell
import           Text.Printf                                  (printf)

-- | Gets current Marketplace store state
marketplaceStore :: Core.Marketplace -> Contract w s Text Core.MarketplaceDatum
marketplaceStore marketplace = do
  let client = Core.marketplaceClient marketplace
  mapError' (getOnChainState client) >>= getStateDatum

getStateDatum ::
    Maybe (OnChainState Core.MarketplaceDatum i, UtxoMap) -> Contract w s Text Core.MarketplaceDatum
getStateDatum = maybe (throwError "Marketplace output not found") (pure . tyTxOutData . ocsTxOut . fst)

getNftEntry :: Core.MarketplaceDatum -> Core.InternalNftId -> Contract w s Text Core.NFT
getNftEntry nftStore (Core.InternalNftId ipfsCidHash ipfsCid) =
        maybe (throwError "NFT has not been created") pure $
          AssocMap.lookup ipfsCidHash $ Core.mdSingletons nftStore

getBundleEntry :: Core.MarketplaceDatum -> Core.InternalBundleId -> Contract w s Text Core.NftBundle
getBundleEntry nftStore (Core.InternalBundleId bundleId cids) =
        maybe (throwError "Bundle has not been created") pure $
          AssocMap.lookup bundleId $ Core.mdBundles nftStore

-- | Gets all UTxOs belonging to a user and concats them into one Value
fundsAt :: PubKeyHash -> Contract w s Text Value
fundsAt pkh = utxoValue <$> utxoAt (pubKeyHashAddress pkh)

-- | Gets all UTxOs belonging to the Marketplace script and concats them into one Value
marketplaceFunds :: Core.Marketplace -> Contract w s Text Value
marketplaceFunds marketplace =  utxoValue <$> utxoAt (Core.marketplaceAddress marketplace)

-- | Gets current auction state for specified NFT
getAuctionState :: Core.Marketplace -> UserItemId -> Contract w s Text Auction.AuctionState
getAuctionState marketplace itemId = do
    let internalId = toInternalId itemId
    nftStore <- marketplaceStore marketplace
    auction <- case internalId of
      Left nftId@(Core.InternalNftId ipfsCidHash ipfsCid) -> do
        nftEntry <- getNftEntry nftStore nftId
        maybe (throwError "NFT has not been put on auction") pure $
            nftEntry ^. Core._nftLot ^? traverse . _2 . _Right
      Right bundleId@(Core.InternalBundleId bundleHash cids) -> do
        bundleEntry <- getBundleEntry nftStore bundleId
        maybe (throwError "Bundle has not been put on auction") pure $
            bundleEntry ^. Core._nbTokens ^? Core._HasLot . _2 . _Right

    let auctionToken = Auction.getStateToken auction
    let auctionParams = Auction.fromTuple auction
    auctionState <- do
        st <- mapError (T.pack . Haskell.show) $ Auction.currentState auctionToken auctionParams
        maybe (throwError "Auction state not found") pure st

    logInfo @Haskell.String $ printf "Returned auction state %s" (Haskell.show auctionState)
    pure auctionState

mapError' :: Contract w s SMContractError a -> Contract w s Text a
mapError' = mapError $ T.pack . Haskell.show

type MarketplaceInfoSchema =
    Endpoint "fundsAt" PubKeyHash
    .\/ Endpoint "marketplaceFunds" ()
    .\/ Endpoint "marketplaceStore" ()
    .\/ Endpoint "getAuctionState" UserItemId

data InfoContractState =
    FundsAt Value
    | MarketplaceFunds Value
    | MarketplaceStore Core.MarketplaceDatum
    | AuctionState Auction.AuctionState
    deriving stock (Haskell.Eq, Haskell.Show, Haskell.Generic)
    deriving anyclass (J.ToJSON, J.FromJSON)

Lens.makeClassyPrisms ''InfoContractState

infoEndpoints :: Core.Marketplace -> Promise (ContractResponse Text InfoContractState) MarketplaceInfoSchema Void ()
infoEndpoints marketplace =
    (withContractResponse (Proxy @"fundsAt") FundsAt fundsAt
    `select` withContractResponse (Proxy @"marketplaceFunds") MarketplaceFunds (const $ marketplaceFunds marketplace)
    `select` withContractResponse (Proxy @"marketplaceStore") MarketplaceStore (const $ marketplaceStore marketplace)
    `select` withContractResponse (Proxy @"getAuctionState") AuctionState (getAuctionState marketplace)) <> infoEndpoints marketplace
