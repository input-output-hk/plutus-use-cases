module Main where

import Test.Tasty
import Test.Tasty.ExpectedFailure (ignoreTest)

import qualified Test.Lending.Contract as Lending.Contract
import qualified Test.Lending.Logic    as Lending.Logic
import qualified Test.Lending.QuickCheck as Lending.QuickCheck
import qualified Test.Nft.Logic        as Nft.Logic
import qualified Test.Nft.Contract     as Nft.Contract
import qualified Test.Demo.Contract.Mint as Demo.Contract.Mint

main :: IO ()
main = defaultMain $ testGroup "tests"
  [ testGroup "NFT"     [ Nft.Logic.test
                        , contract Nft.Contract.test ]
  , testGroup "Lending" [ Lending.Logic.test
                        , contract Lending.Contract.test
                        , Lending.QuickCheck.test ]
  , testGroup "Demo"    [ Demo.Contract.Mint.test ]
  ]
  where
    contract
      | ignoreContract = ignoreTest
      | otherwise      = id

    ignoreContract = False

