module NFT.API where

import Prelude
import Data.Argonaut as A
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, runAff_)
import Effect.Console (log)
import Effect.Class (liftEffect)

import PAB.Api (PABConnectionInfo, callEndpoint)
import PAB.Types (ContractInstanceId)

type Buy = 
  { buy'price     :: String
  , buy'newPrice  :: Maybe String
  }

type SetPrice = 
 { setPrice'newPrice :: Maybe String
}

buyNft :: PABConnectionInfo 
  -> ContractInstanceId 
  -> Buy
  -> Aff Unit
buyNft ci cii buy = do 
    json <- callEndpoint ci cii "buy-nft" buy
    liftEffect $ log $ A.stringify json
    pure unit

setNftPrice :: PABConnectionInfo 
  -> ContractInstanceId 
  -> SetPrice
  -> Aff Unit
setNftPrice ci cii set = do 
    json <- callEndpoint ci cii "set-price-for-nft" set
    liftEffect $ log $ A.stringify json
    pure unit


instanceId :: ContractInstanceId
instanceId = { unContractInstanceId: "67dea86b-e189-43de-bbc5-f946ef24dba4" }

connectionInfo :: PABConnectionInfo
connectionInfo = {
    baseURL: "http://localhost:8080"
}

testBuy :: Buy
testBuy = {
    buy'price: "1000",
    buy'newPrice: Nothing
}

testBuyNft :: Effect Unit
testBuyNft = runAff_ (log <<< show) $ buyNft connectionInfo instanceId testBuy

testSetPrice :: SetPrice
testSetPrice = {
    setPrice'newPrice: Just $ "5000"
}

testSetPrice_ :: Effect Unit
testSetPrice_ = runAff_ (log <<< show) $ setNftPrice connectionInfo instanceId testSetPrice

-- start-nft
