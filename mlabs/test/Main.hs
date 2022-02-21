module Main (main) where

import PlutusTx.Prelude
import Prelude (IO)

import Plutus.Test.Model (readDefaultBchConfig)
import Test.Tasty (defaultMain, testGroup)

import Test.EfficientNFT.Plutip qualified as ENFT.Plutip
import Test.EfficientNFT.Quickcheck qualified as ENFT.Quickcheck
import Test.EfficientNFT.Resources qualified as ENFT.Resources
import Test.EfficientNFT.Script.TokenBurn qualified as ENFT.TokenBurn
import Test.EfficientNFT.Script.TokenChangeOwner qualified as ENFT.TokenChangeOwner
import Test.EfficientNFT.Script.TokenChangePrice qualified as ENFT.TokenChangePrice
import Test.EfficientNFT.Script.TokenMarketplaceBuy qualified as ENFT.TokenMarketplaceBuy
import Test.EfficientNFT.Script.TokenMarketplaceRedeem qualified as ENFT.TokenMarketplaceRedeem
import Test.EfficientNFT.Script.TokenMarketplaceSetPrice qualified as ENFT.TokenMarketplaceSetPrice
import Test.EfficientNFT.Script.TokenMint qualified as ENFT.TokenMint
import Test.EfficientNFT.Script.TokenRestake qualified as ENFT.TokenRestake
import Test.EfficientNFT.Script.TokenUnstake qualified as ENFT.TokenUnstake
import Test.EfficientNFT.Size qualified as ENFT.Size
import Test.EfficientNFT.Trace qualified as ENFT.Trace
import Test.NFT.Size qualified as NFT.Size

main :: IO ()
main = do
  -- To move this below tasty we must write cutom main
  ENFT.Plutip.test
  cfg <- readDefaultBchConfig
  defaultMain $
    testGroup
      "tests"
      [ testGroup
          "NFT"
          [ NFT.Size.test
          ]
      , testGroup
          "Efficient NFT"
          [ ENFT.Size.test
          , ENFT.Resources.test cfg
          , testGroup
              "Token"
              [ ENFT.TokenMint.test
              , ENFT.TokenChangeOwner.test
              , ENFT.TokenChangePrice.test
              , ENFT.TokenBurn.test
              ]
          , testGroup
              "Staking"
              [ ENFT.TokenUnstake.test
              , ENFT.TokenRestake.test
              ]
          , testGroup
              "Marketplace"
              [ ENFT.TokenMarketplaceSetPrice.test
              , ENFT.TokenMarketplaceBuy.test
              , ENFT.TokenMarketplaceRedeem.test
              ]
          , ENFT.Quickcheck.test
          ]
      ]
