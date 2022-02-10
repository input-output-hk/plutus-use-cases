module Main (main) where

import PlutusTx.Prelude
import Prelude (IO, replicate)

import Plutus.Test.Model (readDefaultBchConfig)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.ExpectedFailure (ignoreTest)

-- import Test.Demo.Contract.Mint qualified as Demo.Contract.Mint
-- import Test.Governance.Contract qualified as Governance.Contract
-- import Test.Lending.Contract qualified as Lending.Contract
-- import Test.Lending.Logic qualified as Lending.Logic
-- import Test.Lending.QuickCheck qualified as Lending.QuickCheck
-- import Test.NFT.Contract qualified as NFT.Contract
-- import Test.NFT.QuickCheck qualified as NFT.QuickCheck
-- import Test.NFT.Script.Main qualified as NFT.Script
-- import Test.NftStateMachine.Contract qualified as Nft.Contract
-- import Test.NftStateMachine.Logic qualified as Nft.Logic

import Test.EfficientNFT.Quickcheck qualified as ENFT.Quickcheck
import Test.EfficientNFT.Resources qualified as ENFT.Resources
import Test.EfficientNFT.Script.TokenBurn qualified as ENFT.TokenBurn
import Test.EfficientNFT.Script.TokenChangeOwner qualified as ENFT.TokenChangeOwner
import Test.EfficientNFT.Script.TokenChangePrice qualified as ENFT.TokenChangePrice
import Test.EfficientNFT.Script.TokenMint qualified as ENFT.TokenMint
import Test.EfficientNFT.Script.TokenRestake qualified as ENFT.TokenRestake
import Test.EfficientNFT.Script.TokenUnstake qualified as ENFT.TokenUnstake
import Test.EfficientNFT.Size qualified as ENFT.Size
import Test.EfficientNFT.Trace qualified as ENFT.Trace
import Test.NFT.Size qualified as NFT.Size

main :: IO ()
main = do
  cfg <- readDefaultBchConfig
  defaultMain $
    testGroup
      "tests"
      -- [ testGroup
      --     "NFT - legacy" []
      --     [ Nft.Logic.test
      --     , contract Nft.Contract.test
      --     ]
      [ testGroup
          "NFT"
          [ NFT.Size.test
          -- , NFT.Script.test
          -- , contract NFT.Contract.test
          ]
      , -- HACK
        -- Doing it this way relieves some of the time +
        -- memory usage issues with the QuickCheck tests.
        -- This will run 100 tests
        -- <> replicate 10 (contract NFT.QuickCheck.test)
        testGroup
          "Efficient NFT"
          [ ENFT.Size.test
          , ENFT.Resources.test cfg
          , ENFT.TokenMint.test
          , ENFT.TokenChangeOwner.test
          , ENFT.TokenChangePrice.test
          , ENFT.TokenBurn.test
          , ENFT.TokenUnstake.test
          , ENFT.TokenRestake.test
          , ENFT.Quickcheck.test
          ]
          -- , testGroup
          --     "Lending"
          --     [ Lending.Logic.test
          --     , contract Lending.Contract.test
          --     , Lending.QuickCheck.test
          --     ]
          -- , contract Lending.Contract.test
          -- , testGroup "Demo" [Demo.Contract.Mint.test]
          -- , testGroup "Governance" [Governance.Contract.test]
      ]
  where
    contract
      | ignoreContract = ignoreTest
      | otherwise = id

    ignoreContract = False
