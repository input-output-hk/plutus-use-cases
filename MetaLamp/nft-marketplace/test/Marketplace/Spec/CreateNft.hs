{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Marketplace.Spec.CreateNft
  ( tests, createNftTrace
  ) where

import           Control.Lens                                 (_2, (&), (.~),
                                                               (^.))
import           Control.Monad                                (void)
import           Data.Maybe                                   (isNothing)
import           Data.Text                                    (Text)
import           Data.Void                                    (Void)
import           Ledger.Ada                                   (lovelaceValueOf)
import qualified Ledger.Value                                 as V
import qualified Marketplace.Fixtures                         as Fixtures
import qualified Marketplace.Spec.Start                       as Start
import           Plutus.Abstract.ContractResponse             (ContractResponse)
import           Plutus.Contract.Test
import qualified Plutus.Contracts.NftMarketplace.Endpoints    as Marketplace
import qualified Plutus.Contracts.NftMarketplace.OnChain.Core as Marketplace
import qualified Plutus.Trace                                 as Trace
import qualified PlutusTx.AssocMap                            as AssocMap
import           Test.Tasty
import qualified Utils
import           Wallet.Emulator.Wallet                       (walletAddress, walletPubKeyHash)

tests :: TestTree
tests =
  testGroup
    "createNft"
    [ checkPredicateOptions
        Fixtures.options
        "Should mint NFT token into the user wallet and create the Marketplace entry hiding issuer"
        (datumsCheck .&&. valueCheck .&&. marketplaceOperatorFeeCheck)
        (void createNftTrace),
      checkPredicateOptions
        Fixtures.options
        "Should mint NFT token into the user wallet and create the Marketplace entry revealing issuer"
        (datumsCheck' .&&. valueCheck .&&. marketplaceOperatorFeeCheck)
        createNftTrace'
    ]

createNftParams :: Marketplace.CreateNftParams
createNftParams = Marketplace.CreateNftParams {
                        Marketplace.cnpIpfsCid        = Fixtures.catTokenIpfsCid,
                        Marketplace.cnpNftName        = Fixtures.catTokenName,
                        Marketplace.cnpNftDescription = Fixtures.catTokenDescription,
                        Marketplace.cnpNftCategory = Fixtures.catTokenCategory,
                        Marketplace.cnpRevealIssuer   = False
                    }

createNftTrace :: Trace.EmulatorTrace (Trace.ContractHandle (ContractResponse String Text Marketplace.UserContractState) Marketplace.MarketplaceUserSchema Void)
createNftTrace = do
  _ <- Start.startTrace
  h <- Trace.activateContractWallet Fixtures.userWallet $ Marketplace.userEndpoints Fixtures.marketplace
  _ <- Trace.callEndpoint @"createNft" h createNftParams
  _ <- Trace.waitNSlots 50
  pure h

createNftTrace' :: Trace.EmulatorTrace ()
createNftTrace' = do
  _ <- Start.startTrace
  h <- Trace.activateContractWallet Fixtures.userWallet $ Marketplace.userEndpoints Fixtures.marketplace
  _ <- Trace.callEndpoint @"createNft" h $ createNftParams & Marketplace._cnpRevealIssuer .~ True
  _ <- Trace.waitNSlots 50
  pure ()

datumsCheck :: TracePredicate
datumsCheck =
  dataAtAddress
    Fixtures.marketplaceAddress
    (Utils.checkOneDatum (containsNft . Marketplace.mdSingletons))
    where
      containsNft = maybe False (\t -> (t ^. Marketplace._nftLot & isNothing) &&
                                (t ^. Marketplace._nftRecord . Marketplace._niIssuer & isNothing) &&
                                (t ^. Marketplace._nftRecord & Fixtures.hasCatTokenRecord)) .
                    AssocMap.lookup Fixtures.catTokenIpfsCidHash

datumsCheck' :: TracePredicate
datumsCheck' =
  dataAtAddress
    Fixtures.marketplaceAddress
    (Utils.checkOneDatum (containsNft . Marketplace.mdSingletons))
    where
      containsNft = maybe False (\t -> (t ^. Marketplace._nftLot & isNothing) &&
                                t ^. Marketplace._nftRecord . Marketplace._niIssuer == Just (walletPubKeyHash Fixtures.userWallet) &&
                                (t ^. Marketplace._nftRecord & Fixtures.hasCatTokenRecord)) .
                    AssocMap.lookup Fixtures.catTokenIpfsCidHash

valueCheck :: TracePredicate
valueCheck =
  valueAtAddress
    (walletAddress Fixtures.userWallet)
    (Utils.one hasNft . V.flattenValue)
    where
      hasNft v = (v ^. _2 & V.unTokenName) == Fixtures.catTokenIpfsCidBs

marketplaceOperatorFeeCheck :: TracePredicate
marketplaceOperatorFeeCheck =
  walletFundsChange Fixtures.ownerWallet $ lovelaceValueOf 100000
