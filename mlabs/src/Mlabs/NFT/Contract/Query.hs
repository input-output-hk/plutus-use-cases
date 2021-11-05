module Mlabs.NFT.Contract.Query (
  queryCurrentPrice,
  queryCurrentOwner,
  queryAppSymbol,
  QueryContract,
) where

import Control.Monad ()

import Data.Monoid (Last (..), mconcat)
import Data.Text (Text)
import GHC.Base (join)
import Mlabs.NFT.Contract (getAppSymbol, getsNftDatum)
import Mlabs.NFT.Types (
  DatumNft (..),
  InformationNft (..),
  NftAppInstance,
  NftAppSymbol,
  NftId,
  NftListNode (..),
  QueryResponse (..),
  UserWriter,
 )
import Plutus.Contract (Contract)
import Plutus.Contract qualified as Contract
import PlutusTx.Prelude hiding (mconcat, (<>))
import Prelude (String, show)

-- | A contract used exclusively for query actions.
type QueryContract a = forall s. Contract UserWriter s Text a

{- | Query the current price of a given NFTid. Writes it to the Writer instance
 and also returns it, to be used in other contracts.
-}
queryCurrentPrice :: NftAppSymbol -> NftId -> QueryContract QueryResponse
queryCurrentPrice appSymb nftId = do
  price <- wrap <$> getsNftDatum extractPrice nftId appSymb
  Contract.tell (Last . Just . Right $ price) >> log price >> return price
  where
    wrap = QueryCurrentPrice . join
    extractPrice = \case
      HeadDatum _ -> Nothing
      NodeDatum d -> info'price . node'information $ d
    log price = Contract.logInfo @String $ mconcat ["Current price of: ", show nftId, " is: ", show price]

{-
currentPriceLog :: NftId -> Integer -> String
currentPriceLog nftId price = mconcat ["Current price of: ", show nftId, " is: ", show price]
-}

{- | Query the current owner of a given NFTid. Writes it to the Writer instance
 and also returns it, to be used in other contracts.
-}
queryCurrentOwner :: NftAppSymbol -> NftId -> QueryContract QueryResponse
queryCurrentOwner appSymb nftId = do
  owner <- wrap <$> getsNftDatum extractOwner nftId appSymb
  Contract.tell (Last . Just . Right $ owner) >> log owner >> return owner
  where
    wrap = QueryCurrentOwner . join

    extractOwner = \case
      HeadDatum _ -> Nothing
      NodeDatum d -> Just . info'owner . node'information $ d

    log owner = Contract.logInfo @String $ mconcat ["Current owner of: ", show nftId, " is: ", show owner]

-- | Returns the App symbol.
queryAppSymbol :: NftAppInstance -> NftAppSymbol
queryAppSymbol = getAppSymbol
