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
import Mlabs.NFT.Contract.Init (uniqueTokenName)
import Mlabs.NFT.Types (UniqueToken)

import Plutus.Contracts.Currency ()
import Plutus.V1.Ledger.Value (CurrencySymbol (..), TokenName (..), assetClass)

{- | Contracts available through PAB.
 For concrete endpoints see `getContract`
-}
data MarketplaceContracts
  = -- | Contract for initialising NFT marketplace.
    NftAdminContract
  | -- | Contracts for NFT marketplace user - contracts for
    -- buying/selling NFT, auctions, and query.
    UserContract UniqueToken
  deriving stock (Hask.Eq, Hask.Ord, Hask.Show, Generic)
  deriving anyclass (FromJSON, ToJSON, OpenApi.ToSchema)

instance Pretty MarketplaceContracts where
  pretty = viaShow

instance HasPSTypes MarketplaceContracts where
  psTypes =
    [ equal . genericShow . argonaut $ mkSumType @MarketplaceContracts
    ]

-- todo: fix put correct currencySymbol.
instance HasDefinitions MarketplaceContracts where
  getDefinitions =
    [ NftAdminContract
    , UserContract uT
    ]
    where
      uT = assetClass (CurrencySymbol "ff") (TokenName uniqueTokenName)

  getContract = \case
    NftAdminContract -> SomeBuiltin Contract.NFT.adminEndpoints
    UserContract uT -> SomeBuiltin $ Contract.NFT.nftMarketUserEndpoints uT

  getSchema = \case
    NftAdminContract -> Builtin.endpointsToSchemas @Contract.NFT.NFTAppSchema
    UserContract _ -> Builtin.endpointsToSchemas @Contract.NFT.NFTAppSchema
