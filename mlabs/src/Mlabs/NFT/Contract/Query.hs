module Mlabs.NFT.Contract.Query (
  queryCurrentOwnerLog,
  queryCurrentPriceLog,
  queryListNftsLog,
  queryCurrentPrice,
  queryCurrentOwner,
  queryListNfts,
  QueryContract,
  queryContent,
  queryContentLog,
) where

import PlutusTx.Prelude hiding (mconcat)
import Prelude (String, show)

import Data.Monoid (Last (..), mconcat)
import Data.Text (Text)
import GHC.Base (join)
import Plutus.Contract (Contract)
import Plutus.Contract qualified as Contract

import Mlabs.NFT.Contract.Aux (getDatumsTxsOrdered, getNftDatum, getsNftDatum, hashData)
import Mlabs.NFT.Spooky (toSpooky)
import Mlabs.NFT.Types (
  Content,
  DatumNft (..),
  InformationNft,
  NftId (NftId),
  PointInfo (pi'data),
  QueryResponse (..),
  UniqueToken,
  UserWriter,
  info'owner,
  info'price,
  node'information,
 )

-- | A contract used exclusively for query actions.
type QueryContract a = forall s. Contract UserWriter s Text a

{- | Query the current price of a given NFTid. Writes it to the Writer instance
 and also returns it, to be used in other contracts.
-}
queryCurrentPrice :: UniqueToken -> NftId -> QueryContract QueryResponse
queryCurrentPrice uT nftId = do
  price <- wrap <$> getsNftDatum extractPrice nftId uT
  Contract.tell (Last . Just . Right $ price) >> log price >> return price
  where
    wrap = QueryCurrentPrice . join
    extractPrice = \case
      HeadDatum _ -> Nothing
      NodeDatum d -> info'price . node'information $ d
    log price = Contract.logInfo @String $ queryCurrentPriceLog nftId price

{- | Query the current owner of a given NFTid. Writes it to the Writer instance
 and also returns it, to be used in other contracts.
-}
queryCurrentOwner :: UniqueToken -> NftId -> QueryContract QueryResponse
queryCurrentOwner uT nftId = do
  owner <- wrap <$> getsNftDatum extractOwner nftId uT
  Contract.tell (Last . Just . Right $ owner) >> log owner >> return owner
  where
    wrap = QueryCurrentOwner . join
    extractOwner = \case
      HeadDatum _ -> Nothing
      NodeDatum d -> Just . info'owner . node'information $ d
    log owner = Contract.logInfo @String $ queryCurrentOwnerLog nftId owner

-- | Log of Current Price. Used in testing as well.
queryCurrentPriceLog :: NftId -> QueryResponse -> String
queryCurrentPriceLog nftId price = mconcat ["Current price of: ", show nftId, " is: ", show price]

-- | Log msg of Current Owner. Used in testing as well.
queryCurrentOwnerLog :: NftId -> QueryResponse -> String
queryCurrentOwnerLog nftId owner = mconcat ["Current owner of: ", show nftId, " is: ", show owner]

-- | Query the list of all NFTs in the app
queryListNfts :: UniqueToken -> QueryContract QueryResponse
queryListNfts uT = do
  datums <- fmap pi'data <$> getDatumsTxsOrdered uT
  let nodes = mapMaybe getNode datums
      infos = node'information <$> nodes
  Contract.tell $ wrap infos
  Contract.logInfo @String $ queryListNftsLog infos
  return $ QueryListNfts infos
  where
    getNode (NodeDatum node) = Just node
    getNode _ = Nothing

    wrap = Last . Just . Right . QueryListNfts

-- | Log of list of NFTs available in app. Used in testing as well.
queryListNftsLog :: [InformationNft] -> String
queryListNftsLog infos = mconcat ["Available NFTs: ", show infos]

-- | Given an application instance and a `Content` returns the status of the NFT
queryContent :: UniqueToken -> Content -> QueryContract QueryResponse
queryContent uT content = do
  let nftId = NftId . toSpooky . hashData $ content
  datum <- getNftDatum nftId uT
  status <- wrap $ getStatus datum
  Contract.tell (Last . Just . Right $ status)
  log status
  return status
  where
    wrap = return . QueryContent
    getStatus :: Maybe DatumNft -> Maybe InformationNft
    getStatus = \case
      Just (NodeDatum nftListNode) -> Just $ node'information nftListNode
      _ -> Nothing
    log status = Contract.logInfo @String $ queryContentLog content status

-- | Log of status of a content. Used in testing as well.
queryContentLog :: Content -> QueryResponse -> String
queryContentLog content info = mconcat ["Content status of: ", show content, " is: ", show info]
