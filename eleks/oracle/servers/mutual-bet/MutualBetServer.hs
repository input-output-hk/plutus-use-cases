{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main(main) where

import Data.Aeson
import Data.Text
import GHC.Generics (Generic)
import Ledger.Address (PaymentPubKeyHash)
import Network.Wai.Handler.Warp
import Servant
import Wallet.Emulator (knownWallet)
import Wallet.Emulator.Wallet (mockWalletPaymentPubKeyHash)

type GamesAPI = "wallet" :> Capture "id" Integer :> Get '[JSON] WalletData

data WalletData = WalletData
  { walletDataPubKeyHash :: !PaymentPubKeyHash
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
          pubKeyHash = mockWalletPaymentPubKeyHash walletInst
      return WalletData { walletDataPubKeyHash   = pubKeyHash
                        , walletId               = toUrlPiece walletInst
                        }

mutualBetApp :: Application
mutualBetApp = serve mutualBetAPI mutualBetServer

main :: IO ()
main = do
  run 8082 mutualBetApp
