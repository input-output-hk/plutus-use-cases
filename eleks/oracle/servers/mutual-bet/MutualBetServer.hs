{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main(main) where

import Data.Aeson
import Data.Text
import GHC.Generics (Generic)
import Ledger (PubKeyHash)
import Network.Wai.Handler.Warp
import Servant
import Wallet.Emulator (knownWallet, walletPubKeyHash)

type GamesAPI = "wallet" :> Capture "id" Integer :> Get '[JSON] WalletData

data WalletData = WalletData
  { walletDataPubKeyHash :: !PubKeyHash
  , walletId             :: !Text
  } deriving Generic
instance FromJSON WalletData
instance ToJSON WalletData

mutualBetAPI :: Proxy GamesAPI
mutualBetAPI = Proxy

mutualBetServer :: Server GamesAPI
mutualBetServer = wallet
  where
    wallet:: Integer -> Handler WalletData
    wallet wId = do

      let walletInst = knownWallet $ wId
          pubKeyHash = walletPubKeyHash walletInst
      return WalletData { walletDataPubKeyHash   = pubKeyHash
                        , walletId               = toUrlPiece walletInst
                        }

mutualBetApp :: Application
mutualBetApp = serve mutualBetAPI mutualBetServer

main :: IO ()
main = do
  run 8082 mutualBetApp
