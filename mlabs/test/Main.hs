module Main (main) where

import PlutusTx.Prelude
import Prelude (IO)

import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.ExpectedFailure (ignoreTest)

import Test.Demo.Contract.Mint qualified as Demo.Contract.Mint
import Test.Governance.Contract qualified as Governance.Contract
import Test.Lending.Contract qualified as Lending.Contract
import Test.Lending.Logic qualified as Lending.Logic
import Test.Lending.QuickCheck qualified as Lending.QuickCheck
import Test.NFT.Contract qualified as NFT.Contract
import Test.NFT.QuickCheck qualified as NFT.QuickCheck
import Test.Nft.Contract qualified as Nft.Contract
import Test.Nft.Logic qualified as Nft.Logic

main :: IO ()
main =
  defaultMain $
    testGroup
      "tests"
      [ testGroup
          "Nft"
          [ Nft.Logic.test
          , contract Nft.Contract.test
          ]
      , testGroup
          "NFT"
          [ contract NFT.Contract.test
          , NFT.QuickCheck.test
          ]
      , testGroup
          "Lending"
          [ Lending.Logic.test
          , contract Lending.Contract.test
          , Lending.QuickCheck.test
          ]
      , contract Lending.Contract.test
      , testGroup "Demo" [Demo.Contract.Mint.test]
      , testGroup "Governance" [Governance.Contract.test]
      ]
  where
    contract
      | ignoreContract = ignoreTest
      | otherwise = id

    ignoreContract = False
