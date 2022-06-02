{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Test.EfficientNFT.Plutip (test) where

import Prelude hiding (toEnum)

import Control.Monad.Reader (ReaderT)
import Data.List.NonEmpty (NonEmpty)
import Data.Monoid (Last)
import Data.Text (Text)
import Ledger (PaymentPubKeyHash (unPaymentPubKeyHash), Value)
import Plutus.Contract (waitNSlots)
import PlutusTx.Enum (toEnum)
import Test.Plutip.Contract (initAda, shouldFail, shouldSucceed, withContractAs)
import Test.Plutip.Internal.Types (ClusterEnv, ExecutionResult (ExecutionResult))
import Test.Plutip.LocalCluster (BpiWallet, withCluster)
import Test.Tasty (TestTree)

import Control.Monad (void)
import Mlabs.EfficientNFT.Contract.Burn (burn)
import Mlabs.EfficientNFT.Contract.FeeWithdraw (feeWithdraw)
import Mlabs.EfficientNFT.Contract.MarketplaceBuy (marketplaceBuy)
import Mlabs.EfficientNFT.Contract.MarketplaceDeposit (marketplaceDeposit)
import Mlabs.EfficientNFT.Contract.MarketplaceRedeem (marketplaceRedeem)
import Mlabs.EfficientNFT.Contract.MarketplaceSetPrice (marketplaceSetPrice)
import Mlabs.EfficientNFT.Contract.Mint (generateNft, mintWithCollection)
import Mlabs.EfficientNFT.Contract.SetPrice (setPrice)
import Mlabs.EfficientNFT.Types (MintParams (MintParams), NftData, SetPriceParams (SetPriceParams))

-- TODO: Partial value asserts here when added (https://github.com/mlabs-haskell/plutip/issues/42)
test :: TestTree
test =
  withCluster
    "Integration tests"
    [ shouldSucceed "Happy path" (initAda 100 <> initAda 100 <> initAda 100) testValid
    , shouldFail "Fail to change price when not owner" (initAda 100 <> initAda 100) testChangePriceNotOwner
    , shouldFail "Fail to redeem when not owner" (initAda 100 <> initAda 100) testRedeemNotOwner
    , shouldFail "Fail unlocking too early" (initAda 100) testBurnTooEarly
    ]

type TestCase = ReaderT (ClusterEnv, NonEmpty BpiWallet) IO (ExecutionResult (Last NftData) Text ((), NonEmpty Value))

testValid :: TestCase
testValid = do
  (ExecutionResult (Right ((nft3, pkhs), _)) _) <- withContractAs 0 $ \[_, pkh] -> do
    let pkhs = pure $ unPaymentPubKeyHash pkh
    cnft <- generateNft
    waitNSlots 1

    nft1 <- mintWithCollection (cnft, MintParams (toEnum 0) (toEnum 50_00) (toEnum 10_000_000) 5 5 Nothing pkhs)
    waitNSlots 1

    nft2 <- setPrice (SetPriceParams nft1 (toEnum 50_000_000))
    waitNSlots 1

    nft3 <- marketplaceDeposit nft2
    waitNSlots 1

    pure (nft3, pkhs)

  withContractAs 1 $
    const $ do
      nft4 <- marketplaceBuy nft3
      waitNSlots 1

      nft5 <- marketplaceSetPrice (SetPriceParams nft4 (toEnum 25_000_000))
      waitNSlots 1

      nft6 <- marketplaceRedeem nft5
      waitNSlots 1

      nft7 <- setPrice (SetPriceParams nft6 (toEnum 20_000_000))
      waitNSlots 1

      burn nft7
      waitNSlots 1

  withContractAs 2 $
    const $ do
      feeWithdraw pkhs
      void $ waitNSlots 1

testChangePriceNotOwner :: TestCase
testChangePriceNotOwner = do
  (ExecutionResult (Right (nft2, _)) _) <- withContractAs 0 $
    const $ do
      cnft <- generateNft
      waitNSlots 1

      nft1 <- mintWithCollection (cnft, MintParams (toEnum 0) (toEnum 0) (toEnum 10_000_000) 5 5 Nothing [])
      waitNSlots 1

      nft2 <- marketplaceDeposit nft1
      waitNSlots 1

      pure nft2

  withContractAs 1 $
    const $ do
      marketplaceSetPrice (SetPriceParams nft2 (toEnum 20_000_000))
      waitNSlots 1

      pure ()

testRedeemNotOwner :: TestCase
testRedeemNotOwner = do
  (ExecutionResult (Right (nft2, _)) _) <- withContractAs 0 $
    const $ do
      cnft <- generateNft
      waitNSlots 1

      nft1 <- mintWithCollection (cnft, MintParams (toEnum 0) (toEnum 0) (toEnum 10_000_000) 5 5 Nothing [])
      waitNSlots 1

      nft2 <- marketplaceDeposit nft1
      waitNSlots 1

      pure nft2

  withContractAs 1 $
    const $ do
      marketplaceRedeem nft2
      waitNSlots 1

      pure ()

testBurnTooEarly :: TestCase
testBurnTooEarly = do
  withContractAs 0 $
    const $ do
      cnft <- generateNft
      waitNSlots 1

      nft1 <- mintWithCollection (cnft, MintParams (toEnum 0) (toEnum 0) (toEnum 10_000_000) 5_000_000_000 5_000_000_000 Nothing [])
      waitNSlots 1

      burn nft1
