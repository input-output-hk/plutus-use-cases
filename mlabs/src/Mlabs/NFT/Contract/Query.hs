module Mlabs.NFT.Contract.Query where

import PlutusTx.Prelude hiding (mconcat, (<>))

-- import Prelude (mconcat, (<>))
-- import Prelude qualified as Hask

-- import Control.Lens (filtered, to, traversed, (^.), (^..), _Just, _Right)
-- import Control.Monad (void)
-- import Data.List qualified as L
-- import Data.Map qualified as Map
import Data.Monoid (Last (..))
import Data.Text (Text)
import Plutus.Contract (Contract)

-- import Plutus.Contract qualified as Contract
-- import PlutusTx qualified

-- import Plutus.Contracts.Currency (CurrencyError, mintContract, mintedValue)
-- import Plutus.Contracts.Currency qualified as MC
-- import Plutus.V1.Ledger.Value (TokenName (..), assetClass, currencySymbol, flattenValue, symbols)

--import Ledger (
--  Address,
--  AssetClass,
--  ChainIndexTxOut,
--  Datum (..),
--  Redeemer (..),
--  TxOutRef,
--  Value,
--  ciTxOutDatum,
--  ciTxOutValue,
--  getDatum,
--  pubKeyAddress,
--  pubKeyHash,
--  scriptCurrencySymbol,
--  txId,
-- )

-- import Ledger.Constraints qualified as Constraints
-- import Ledger.Typed.Scripts (validatorScript)
-- import Ledger.Value as Value (singleton, unAssetClass, valueOf)

import Mlabs.NFT.Types (
  GenericContract,
  NftAppSymbol,
  NftId,
  QueryResponse,
 )

-- | A contract used exclusively for query actions.
type QueryContract a = forall s. Contract QueryResponse s Text a

-- | A contract used for all user actions.
type UserContract a = forall s. Contract (Last NftId) s Text a

{- | Query the current price of a given NFTid. Writes it to the Writer instance
 and also returns it, to be used in other contracts.
-}
queryCurrentPrice :: NftId -> NftAppSymbol -> QueryContract QueryResponse
queryCurrentPrice _ _ = error ()

--  price <- wrap <$> getsNftDatum dNft'price nftid
--  Contract.tell price >> log price >> return price
--  where
--    wrap = QueryCurrentPrice . Last . join
--    log price =
--      Contract.logInfo @Hask.String $
--        "Current price of: " <> Hask.show nftid <> " is: " <> Hask.show price

{- | Query the current owner of a given NFTid. Writes it to the Writer instance
 and also returns it, to be used in other contracts.
-}
queryCurrentOwner :: NftId -> NftAppSymbol -> QueryContract QueryResponse
queryCurrentOwner _ _ = error ()

--   ownerResp <- wrap <$> getsNftDatum dNft'owner nftid
--   Contract.tell ownerResp >> log ownerResp >> return ownerResp
--   where
--     wrap = QueryCurrentOwner . Last
--     log owner =
--       Contract.logInfo @Hask.String $
--         "Current owner of: " <> Hask.show nftid <> " is: " <> Hask.show owner
