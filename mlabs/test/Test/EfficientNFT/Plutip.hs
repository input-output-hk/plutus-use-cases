{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Test.EfficientNFT.Plutip (test) where

import Prelude hiding (toEnum)

import PlutusTx.Enum (toEnum)
import System.Environment (setEnv)
import Test.Plutip
import Test.Plutip.Internal.LocalCluster.Types (Outcome (Success), RunResult (RunResult))

import Mlabs.EfficientNFT.Contract.Burn (burn)
import Mlabs.EfficientNFT.Contract.MarketplaceBuy (marketplaceBuy)
import Mlabs.EfficientNFT.Contract.MarketplaceDeposit (marketplaceDeposit)
import Mlabs.EfficientNFT.Contract.MarketplaceRedeem (marketplaceRedeem)
import Mlabs.EfficientNFT.Contract.MarketplaceSetPrice (marketplaceSetPrice)
import Mlabs.EfficientNFT.Contract.Mint (generateNft, mintWithCollection)
import Mlabs.EfficientNFT.Contract.SetPrice (setPrice)
import Mlabs.EfficientNFT.Types

test :: IO ()
test = do
  setEnv "SHELLEY_TEST_DATA" "cluster-data"
  setEnv "NO_POOLS" "1"

  runUsingCluster $ do
    w1 <- addSomeWallet (ada 100)
    w2 <- addSomeWallet (ada 100)
    w3 <- addSomeWallet (ada 100)
    waitSeconds 2

    cnft1 <-
      runContract @() w1 generateNft
        >>= getResult

    nft1_1 <-
      runContract w1 (mintWithCollection (cnft1, MintParams (toEnum 10) (toEnum 10_000_000) 5 5))
        >>= getResult

    nft1_2 <-
      runContract w1 (setPrice (SetPriceParams nft1_1 (toEnum 20_000_000)))
        >>= getResult

    cnft2 <-
      runContract @() w2 generateNft
        >>= getResult

    nft1_3 <-
      runContract w1 (marketplaceDeposit nft1_2)
        >>= getResult

    nft2_1 <-
      runContract w2 (mintWithCollection (cnft2, MintParams (toEnum 10) (toEnum 10_000_000) 5 5))
        >>= getResult

    nft2_2 <-
      runContract w2 (setPrice (SetPriceParams nft2_1 (toEnum 20_000_000)))
        >>= getResult

    nft1_4 <-
      runContract w1 (marketplaceSetPrice (SetPriceParams nft1_3 (toEnum 25_000_000)))
        >>= getResult

    nft2_3 <-
      runContract w2 (marketplaceDeposit nft2_2)
        >>= getResult

    nft1_5 <-
      runContract w3 (marketplaceBuy nft1_4)
        >>= getResult

    nft2_4 <-
      runContract w3 (marketplaceBuy nft2_3)
        >>= getResult

    nft1_6 <-
      runContract w3 (marketplaceSetPrice (SetPriceParams nft1_5 (toEnum 20_000_000)))
        >>= getResult

    nft1_7 <-
      runContract w3 (marketplaceRedeem nft1_6)
        >>= getResult

    nft2_5 <-
      runContract w3 (marketplaceRedeem nft2_4)
        >>= getResult

    runContract w3 (burn nft1_7) >>= getResult

    runContract w3 (burn nft2_5) >>= getResult

    pure ()
  where
    getResult r = case r of
      RunResult _ (Success x _) -> do
        waitSeconds 2
        pure x
      _ -> error . show $ r
