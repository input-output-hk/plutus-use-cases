module Mlabs.NFT.PAB.MarketplaceContract (
  MarketplaceContracts (..),
) where

import PlutusTx.Prelude
import Prelude qualified as Hask

import Data.Aeson (
  FromJSON,
  ToJSON,
 )
import Data.OpenApi.Schema qualified as OpenApi

import GHC.Generics (Generic)

import Prettyprinter (Pretty (..), viaShow)

import Language.PureScript.Bridge (argonaut, equal, genericShow, mkSumType)

import Plutus.PAB.Effects.Contract.Builtin (HasDefinitions (..), SomeBuiltin (..))
import Plutus.PAB.Effects.Contract.Builtin qualified as Builtin
import Plutus.PAB.Run.PSGenerator (HasPSTypes (..))

import Mlabs.NFT.Api qualified as Contract.NFT
import Mlabs.NFT.Types (NftAppSymbol (..))

import Plutus.V1.Ledger.Value (CurrencySymbol (..))

{- | Contracts available through PAB.
 For concrete endpoints see `getContract`
-}
data MarketplaceContracts
  = -- | Contract for initialising NFT marketplace.
    NftAdminContract
  | -- | Contracts for NFT marketplace user - contracts for
    -- buying/selling NFT, auctions, and query.
    UserContract NftAppSymbol
  deriving stock (Hask.Eq, Hask.Ord, Hask.Show, Generic)
  deriving anyclass (FromJSON, ToJSON, OpenApi.ToSchema)

instance Pretty MarketplaceContracts where
  pretty = viaShow

instance HasPSTypes MarketplaceContracts where
  psTypes =
    [ equal . genericShow . argonaut $ mkSumType @MarketplaceContracts
    ]

instance HasDefinitions MarketplaceContracts where
  getDefinitions =
    [ NftAdminContract
    , UserContract someAppSymbol
    ]
    where
      someAppSymbol = NftAppSymbol $ CurrencySymbol "ff"

  getContract = \case
    NftAdminContract ->
      SomeBuiltin Contract.NFT.adminEndpoints
    UserContract appSymbol ->
      SomeBuiltin $ Contract.NFT.nftMarketUserEndpoints appSymbol

  getSchema = \case
    NftAdminContract -> Builtin.endpointsToSchemas @Contract.NFT.NFTAppSchema
    UserContract _ -> Builtin.endpointsToSchemas @Contract.NFT.NFTAppSchema
