{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Marketplace.Spec.Sale where

-- import           Control.Lens                                 (_2, _Left, (&),
--                                                                (^.), (^?))
-- import           Control.Monad                                (void)
-- import           Data.Foldable                                (find)
-- import           Data.Maybe                                   (isNothing)
-- import           Data.Proxy
-- import           Data.Text                                    (Text)
-- import           Data.Void                                    (Void)
-- import           Ledger.Ada                                   (Ada (..),
--                                                                lovelaceValueOf,
--                                                                toValue)
-- import           Ledger.Index                                 (minAdaTxOut)
-- import qualified Ledger.Value                                 as V
-- import qualified Marketplace.Fixtures                         as Fixtures
-- import qualified Marketplace.Spec.CreateNft                   as CreateNft
-- import qualified Marketplace.Spec.Start                       as Start
-- import           Plutus.Abstract.ContractResponse             (ContractResponse)
-- import           Plutus.Contract.Test
-- import qualified Plutus.Contracts.NftMarketplace.Endpoints    as Marketplace
-- import qualified Plutus.Contracts.NftMarketplace.OnChain.Core as Marketplace
-- import qualified Plutus.Contracts.Services.Sale               as Sale
-- import qualified Plutus.Trace                                 as Trace
-- import qualified PlutusTx.AssocMap                            as AssocMap
-- import           Test.Tasty
-- import qualified Utils
-- import           Wallet.Emulator.Wallet                       (walletAddress)

-- tests :: TestTree
-- tests =
--   testGroup
--     "Sale"
--   [testGroup
--     "NFT singletons"
--     [
--       checkPredicateOptions
--         Fixtures.options
--         "Should put on sale one NFT locking it in sale script & saving link"
--         (openSaleValueCheck .&&. openSaleDatumsCheck)
--         (void openSaleTrace),
--       checkPredicateOptions
--         Fixtures.options
--         "Should not put on sale if NFT does not exist"
--         errorCheckOpen
--         openSaleTrace',
--       checkPredicateOptions
--         Fixtures.options
--         "Should close sale and pay locked NFT back"
--         (closeSaleValueCheck .&&. completeSaleDatumsCheck)
--         closeSaleTrace,
--       checkPredicateOptions
--         Fixtures.options
--         "Should not close sale if it was not started"
--         errorCheckClose
--         closeSaleTrace',
--       checkPredicateOptions
--         Fixtures.options
--         "Should sell NFT and pay the token to buyer"
--         (buyItemValueCheck .&&. completeSaleDatumsCheck)
--         buyItemTrace,
--       checkPredicateOptions
--         Fixtures.options
--         "Should not sell NFT if it has no lot"
--         errorCheckBuyer
--         buyItemTrace',
--       checkPredicateOptions
--         Fixtures.options
--         "Should sell NFT and pay fee to marketplace operator"
--         (marketplaceOperatorFeeCheck .&&. sellersProfitWithPayingFeeCheck)
--         buyItemTrace
--     ]]

-- -- \/\/\/ "NFT singletons"

-- singletonNftPrice :: Integer
-- singletonNftPrice = 60 * Fixtures.oneAdaInLovelace

-- openSaleParams ::        Marketplace.OpenSaleParams
-- openSaleParams =  Marketplace.OpenSaleParams {
--                     Marketplace.ospItemId   = Marketplace.UserNftId Fixtures.catTokenIpfsCid,
--                     Marketplace.ospSalePrice = singletonNftPrice
--                    }

-- closeLotParams ::        Marketplace.CloseLotParams
-- closeLotParams =  Marketplace.CloseLotParams {
--                       Marketplace.clpItemId    = Marketplace.UserNftId Fixtures.catTokenIpfsCid
--                     }

-- openSaleTrace :: Trace.EmulatorTrace (Trace.ContractHandle (ContractResponse String Text Marketplace.UserContractState) Marketplace.MarketplaceUserSchema Void)
-- openSaleTrace = do
--   h <- CreateNft.createNftTrace

--   _ <- Trace.callEndpoint @"openSale" h openSaleParams

--   _ <- Trace.waitNSlots 50
--   pure h

-- openSaleTrace' :: Trace.EmulatorTrace ()
-- openSaleTrace' = do
--   _ <- Start.startTrace
--   h <- Trace.activateContractWallet Fixtures.userWallet $ Marketplace.userEndpoints Fixtures.marketplace

--   _ <- Trace.callEndpoint @"openSale" h openSaleParams

--   _ <- Trace.waitNSlots 50
--   pure ()

-- closeSaleTrace :: Trace.EmulatorTrace ()
-- closeSaleTrace = do
--   h <- openSaleTrace

--   _ <- Trace.callEndpoint @"closeSale" h closeLotParams

--   _ <- Trace.waitNSlots 50
--   pure ()

-- closeSaleTrace' :: Trace.EmulatorTrace ()
-- closeSaleTrace' = do
--   h <- CreateNft.createNftTrace

--   _ <- Trace.callEndpoint @"closeSale" h closeLotParams

--   _ <- Trace.waitNSlots 50
--   pure ()

-- buyItemTrace :: Trace.EmulatorTrace ()
-- buyItemTrace = do
--   _ <- openSaleTrace

--   h <- Trace.activateContractWallet Fixtures.buyerWallet $ Marketplace.userEndpoints Fixtures.marketplace
--   _ <- Trace.callEndpoint @"buyItem" h closeLotParams

--   _ <- Trace.waitNSlots 50
--   pure ()

-- buyItemTrace' :: Trace.EmulatorTrace ()
-- buyItemTrace' = do
--   _ <- CreateNft.createNftTrace

--   h <- Trace.activateContractWallet Fixtures.buyerWallet $ Marketplace.userEndpoints Fixtures.marketplace
--   _ <- Trace.callEndpoint @"buyItem" h closeLotParams

--   _ <- Trace.waitNSlots 50
--   pure ()

-- openSaleDatumsCheck :: TracePredicate
-- openSaleDatumsCheck =
--   dataAtAddress
--     Fixtures.marketplaceAddress
--     (Utils.checkOneDatum (nftIsOnSale . Marketplace.mdSingletons))
--     where
--       nftIsOnSale = maybe False (\t -> Marketplace.getSaleFromNFT t & fmap Sale.saleValue &
--                                 (== Just (Marketplace.nftValue Fixtures.catTokenIpfsCidBs t))) .
--                     AssocMap.lookup Fixtures.catTokenIpfsCidHash

-- completeSaleDatumsCheck :: TracePredicate
-- completeSaleDatumsCheck =
--   dataAtAddress
--     Fixtures.marketplaceAddress
--     (Utils.checkOneDatum (nftNotOnSale . Marketplace.mdSingletons))
--     where
--       nftNotOnSale = maybe False (isNothing . Marketplace.nftLot) .
--                      AssocMap.lookup Fixtures.catTokenIpfsCidHash

-- openSaleValueCheck :: TracePredicate
-- openSaleValueCheck =
--   valueAtAddress
--     (walletAddress Fixtures.userWallet)
--     (isNothing . find hasNft . V.flattenValue)
--     where
--       hasNft v = (v ^. _2 & V.unTokenName) == Fixtures.catTokenIpfsCidBs

-- closeSaleValueCheck :: TracePredicate
-- closeSaleValueCheck =
--   valueAtAddress
--     (walletAddress Fixtures.userWallet)
--     (Utils.one hasNft . V.flattenValue)
--     where
--       hasNft v = (v ^. _2 & V.unTokenName) == Fixtures.catTokenIpfsCidBs

-- buyItemValueCheck :: TracePredicate
-- buyItemValueCheck =
--   valueAtAddress
--     (walletAddress Fixtures.buyerWallet)
--     (Utils.one hasNft . V.flattenValue)
--     where
--       hasNft v = (v ^. _2 & V.unTokenName) == Fixtures.catTokenIpfsCidBs

-- errorCheckOpen :: TracePredicate
-- errorCheckOpen = Utils.assertCrError (Proxy @"openSale") (Marketplace.userEndpoints Fixtures.marketplace) (Trace.walletInstanceTag Fixtures.userWallet)

-- errorCheckClose :: TracePredicate
-- errorCheckClose = Utils.assertCrError (Proxy @"closeSale") (Marketplace.userEndpoints Fixtures.marketplace) (Trace.walletInstanceTag Fixtures.userWallet)

-- errorCheckBuyer :: TracePredicate
-- errorCheckBuyer = Utils.assertCrError (Proxy @"buyItem") (Marketplace.userEndpoints Fixtures.marketplace) (Trace.walletInstanceTag Fixtures.buyerWallet)

-- marketplaceOperatorFeeCheck :: TracePredicate
-- marketplaceOperatorFeeCheck =
--   walletFundsChange Fixtures.ownerWallet $ toValue (Fixtures.marketplaceCreationFee + saleFee - minAdaTxOut)
--   where
--     saleFee = Lovelace $ Fixtures.roundedPercentage singletonNftPrice

-- sellersProfitWithPayingFeeCheck :: TracePredicate
-- sellersProfitWithPayingFeeCheck =
--   walletFundsChange Fixtures.userWallet $ toValue (nftPriceAda - saleFee - Fixtures.marketplaceCreationFee - minAdaTxOut)
--   where
--     nftPriceAda = Lovelace singletonNftPrice
--     saleFee = Lovelace $ Fixtures.roundedPercentage singletonNftPrice

