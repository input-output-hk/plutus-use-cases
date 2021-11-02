module Mlabs.NFT.Contract.Query (
  queryCurrentPrice,
  queryCurrentOwner,
  QueryContract,
) where

import Data.Monoid (mconcat)
import Data.Text (Text)
import GHC.Base (join)
import Mlabs.NFT.Contract (getAppSymbol, getsNftDatum)
import Mlabs.NFT.Types (
  DatumNft (..),
  InformationNft (..),
  NftAppInstance,
  NftId,
  NftListNode (..),
  QueryResponse (..),
 )
import Plutus.Contract (Contract)
import Plutus.Contract qualified as Contract
import PlutusTx.Prelude hiding (mconcat, (<>))
import Prelude (String, show)

-- | A contract used exclusively for query actions.
type QueryContract a = forall s. Contract QueryResponse s Text a

{- | Query the current price of a given NFTid. Writes it to the Writer instance
 and also returns it, to be used in other contracts.
-}
queryCurrentPrice :: NftId -> NftAppInstance -> QueryContract QueryResponse
queryCurrentPrice nftId appInst = do
  price <- wrap <$> getsNftDatum extractPrice nftId (getAppSymbol appInst)
  Contract.tell price >> log price >> return price
  where
    wrap = QueryCurrentPrice . join
    extractPrice = \case
      HeadDatum _ -> Nothing
      NodeDatum d -> info'price . node'information $ d
    log price = Contract.logInfo @String $ mconcat ["Current price of: ", show nftId, " is: ", show price]

{- | Query the current owner of a given NFTid. Writes it to the Writer instance
 and also returns it, to be used in other contracts.
-}
queryCurrentOwner :: NftId -> NftAppInstance -> QueryContract QueryResponse
queryCurrentOwner nftId appInst = do
  owner <- wrap <$> getsNftDatum extractOwner nftId (getAppSymbol appInst)
  Contract.tell owner >> log owner >> return owner
  where
    wrap = QueryCurrentOwner . join

    extractOwner = \case
      HeadDatum _ -> Nothing
      NodeDatum d -> Just . info'owner . node'information $ d

    log owner = Contract.logInfo @String $ mconcat ["Current owner of: ", show nftId, " is: ", show owner]
