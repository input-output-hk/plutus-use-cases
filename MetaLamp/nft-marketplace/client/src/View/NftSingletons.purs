module View.NftSingletons where

import Prelude
import Business.Datum (Auction)
import Business.Datum as Datum
import Data.Foldable (intercalate)
import Data.Maybe (maybe)
import Data.Newtype (unwrap)
import Halogen.HTML as HH
import Network.RemoteData (RemoteData)
import Plutus.Contracts.NftMarketplace.OnChain.Core.StateMachine (MarketplaceDatum)
import Plutus.Contracts.Services.Sale.Core (Sale)
import Plutus.V1.Ledger.Value (Value)
import View.RemoteDataState (remoteDataState)

renderNftSingletons ::
  forall props act.
  RemoteData String Value -> RemoteData String MarketplaceDatum -> (Datum.NftSingleton -> HH.HTML props act) -> HH.HTML props act
renderNftSingletons val md injRender = remoteDataState render ({ value: _, datum: _ } <$> val <*> md)
  where
  render rd = HH.div_ $ map (\n -> renderNft (injRender n) n) $ Datum.findNftSingletons rd.value rd.datum

renderNftSingletonLots ::
  forall props act.
  RemoteData String MarketplaceDatum -> (Datum.NftSingletonLot -> HH.HTML props act) -> HH.HTML props act
renderNftSingletonLots md injRender = remoteDataState render md
  where
  render rd = HH.div_ $ map (\r -> renderNft (injRender r) r.nft) $ Datum.findNftSingletonLots rd

renderNft ::
  forall props act.
  HH.HTML props act -> Datum.NftSingleton -> HH.HTML props act
renderNft html nft =
  HH.div_
    [ HH.h4_ [ HH.text "IPFS Content ID: " ]
    , HH.p_ [ HH.text nft.ipfsCid ]
    , HH.h4_ [ HH.text "NFT name: " ]
    , HH.p_ [ HH.text nft.name ]
    , HH.h4_ [ HH.text "NFT description: " ]
    , HH.p_ [ HH.text nft.description ]
    , HH.h4_ [ HH.text "NFT category: " ]
    , HH.p_ [ HH.text $ intercalate "." nft.category ]
    , HH.h4_ [ HH.text "NFT issuer: " ]
    , HH.p_ [ HH.text $ maybe "***HIDDEN***" (unwrap >>> _.getPubKeyHash) nft.issuer ]
    , html
    , HH.br_
    ]

renderSale ::
  forall props act.
  Sale -> HH.HTML props act
renderSale sale =
  HH.div_
    [ HH.h4_ [ HH.text "Ongoing sale: " ]
    , HH.p_ [ HH.text $ "price: " <> show (unwrap sale).salePrice <> " Lovelace" ]
    ]

renderAuction ::
  forall props act.
  Auction -> HH.HTML props act
renderAuction auction =
  HH.div_
    [ HH.h4_ [ HH.text "Auction: " ]
    , HH.p_ [ HH.text $ "end time: " <> show auction.endTime ]
    ]
