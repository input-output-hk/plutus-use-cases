module View.NftSingletons where

import Prelude
import Business.Datum as Datum
import Data.Foldable (intercalate)
import Data.Maybe (fromMaybe, maybe)
import Data.Newtype (unwrap)
import Halogen.HTML as HH
import Network.RemoteData (RemoteData)
import Plutus.Contracts.NftMarketplace.OnChain.Core.StateMachine (MarketplaceDatum)
import Plutus.V1.Ledger.Value (Value)
import View.RemoteDataState (remoteDataState)

renderNftSingletons ::
  forall props act.
  RemoteData String Value -> RemoteData String MarketplaceDatum -> HH.HTML props act
renderNftSingletons val md = remoteDataState render ({ value: _, datum: _ } <$> val <*> md)
  where
  render rd = HH.div_ $ map renderNft $ Datum.findNftSingletons rd.value rd.datum

renderNft ::
  forall props act.
  Datum.NftSingleton -> HH.HTML props act
renderNft nft =
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
    , HH.br_
    ]
