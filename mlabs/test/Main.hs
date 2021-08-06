module Main (main) where

import PlutusTx.Prelude
import Prelude (IO)

import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.ExpectedFailure (ignoreTest)

import Test.Demo.Contract.Mint qualified as Demo.Contract.Mint
import Test.Lending.Contract qualified as Lending.Contract
import Test.Lending.Logic qualified as Lending.Logic
import Test.Lending.QuickCheck qualified as Lending.QuickCheck
import Test.Nft.Contract qualified as Nft.Contract
import Test.Nft.Logic qualified as Nft.Logic

main :: IO ()
main =
  defaultMain $
    testGroup
      "tests"
      [ testGroup
          "NFT"
          [ Nft.Logic.test
          , contract Nft.Contract.test
          ]
      , testGroup
          "Lending"
          [ Lending.Logic.test
          , contract Lending.Contract.test
          , Lending.QuickCheck.test
          ]
      , contract Lending.Contract.test
      , testGroup "Demo" [Demo.Contract.Mint.test]
      ]
  where
    contract
      | ignoreContract = ignoreTest
      | otherwise = id

    ignoreContract = False
