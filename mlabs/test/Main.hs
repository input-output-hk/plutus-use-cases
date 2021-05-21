module Main where

import Test.Tasty
import Test.Tasty.ExpectedFailure (ignoreTest)

import qualified Test.Lending.Contract as Lending.Contract
import qualified Test.Lending.Logic    as Lending.Logic
import qualified Test.Nft.Logic        as Nft.Logic

main :: IO ()
main = defaultMain $ testGroup "tests"
  [ testGroup "NFT"     [ Nft.Logic.test]
  , testGroup "Lending" [ Lending.Logic.test
                        , contract Lending.Contract.test ]
  ]
  where
    contract
      | ignoreContract = ignoreTest
      | otherwise      = id

    ignoreContract = False

