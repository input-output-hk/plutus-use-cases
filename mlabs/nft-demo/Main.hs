-- | Simulator demo for NFTs
module Main(
      main
    , activateStartNft
    , activateUser
    , nftContent
    , startParams
  ) where

import Prelude

import Control.Monad.IO.Class ( MonadIO(liftIO) )
import Data.Functor ( void )
import Playground.Contract ( Wallet(Wallet) )
import Plutus.Contract ( ContractInstanceId )
import Plutus.PAB.Simulator qualified as Simulator
import PlutusTx.Prelude (ByteString)

import Mlabs.Nft.Logic.Types ( NftId )
import Mlabs.Nft.Contract qualified as Nft
import Mlabs.Nft.Contract.Simulator.Handler qualified as Handler
import PlutusTx.Ratio qualified as R
import Mlabs.Plutus.PAB ( call, printBalance, waitForLast )
import Mlabs.System.Console.PrettyLogger ( logNewLine )
import Mlabs.System.Console.Utils ( logAction, logMlabs )

-- | Main function to run simulator
main :: IO ()
main = Handler.runSimulator startParams $ do
  let users = [1, 2, 3]
  logMlabs
  test "Init users" users (pure ())

  test "User 1 creates the Mona lisa (NFT)" users (pure ())

  nid <- activateStartNft user1
  cids <- mapM (activateUser nid) [user1, user2, user3]
  let [u1, u2, u3] = cids

  test "User 1 sets the Mona Lisa's price to 100 Lovelace, User 2 buys The Mona Lisa from User 1 for 100 Lovelace (what a deal!), User 2 has specified that the Mona Lisa is not for sale" [1, 2] $ do
    setPrice u1 (Just 100)
    buy u2 100 Nothing

  test "User 2 sets the sale price to 500 Lovelace, User 3 buys The Mona Lisa from User 2 for 500 Lovelace setting the new sale price to 1000 Lovelace, User 1 receives a royalty from the sale" [1, 2, 3] $ do
    setPrice u2 (Just 500)
    buy u3 500 (Just 1000)

  liftIO $ putStrLn "Fin (Press enter to Exit)"
  where
    test msg wals act = do
      void act
      logAction msg
      mapM_ printBalance wals
      next

    next = do
      logNewLine
      void $ Simulator.waitNSlots 10

------------------------------------------------------------------------
-- handlers

-- | Instanciates start NFT endpoint in the simulator to the given wallet
activateStartNft :: Wallet -> Handler.Sim NftId
activateStartNft wal = do
  wid <- Simulator.activateContract wal Handler.StartNft
  nftId <- waitForLast wid
  void $ Simulator.waitUntilFinished wid
  pure nftId

-- | Instanciates user actions endpoint in the simulator to the given wallet
activateUser :: NftId -> Wallet -> Handler.Sim ContractInstanceId
activateUser nid wal = do
  Simulator.activateContract wal $ Handler.User nid

-------------------------------------------------------------
-- Script helpers

-- | Call buy NFT endpoint
buy :: ContractInstanceId -> Integer -> Maybe Integer -> Handler.Sim ()
buy cid price newPrice = call cid (Nft.Buy price newPrice)

-- | Call set price for NFT endpoint
setPrice :: ContractInstanceId -> Maybe Integer -> Handler.Sim ()
setPrice cid newPrice = call cid (Nft.SetPrice newPrice)

-------------------------------------------------------------
-- constants

-- Users for testing
user1, user2, user3 :: Wallet
user1 = Wallet 1
user2 = Wallet 2
user3 = Wallet 3

-- | Content of NFT
nftContent :: ByteString
nftContent = "Mona Lisa"

-- | NFT initial parameters
startParams :: Nft.StartParams
startParams = Nft.StartParams
  { sp'content = nftContent
  , sp'share   = 1 R.% 10
  , sp'price   = Nothing
  }

