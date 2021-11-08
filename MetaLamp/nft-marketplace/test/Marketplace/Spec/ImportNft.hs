{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Marketplace.Spec.ImportNft
  ( tests, importNftTrace
  ) where

import           Control.Lens                                 (_2, (&), (.~),
                                                               (^.))
import           Control.Monad                                (void)
import           Data.Maybe                                   (isNothing)
import           Data.Text                                    (Text)
import           Data.Void                                    (Void)
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
import           Wallet.Emulator.Wallet                       (walletAddress)

tests :: TestTree
tests =
  testGroup
    "importNft"
    [ checkPredicateOptions
        Fixtures.options
        "1 Should create the Marketplace entry hiding issuer"
        (valueCheck .&&. datumsCheck)
        (void importNftTrace)
    ]

importNftParams :: Marketplace.ImportNftParams
importNftParams = Marketplace.ImportNftParams {
                        Marketplace.inpCurrency       = Fixtures.catCurrencySymbol,
                        Marketplace.inpIpfsCid        = Fixtures.catTokenIpfsCid,
                        Marketplace.inpNftName        = Fixtures.catTokenName,
                        Marketplace.inpNftDescription = Fixtures.catTokenDescription,
                        Marketplace.inpNftCategory = Fixtures.catTokenCategory,
                        Marketplace.inpRevealIssuer   = False,
                        Marketplace.inpMintBeforeImport = True
                    }

importNftTrace :: Trace.EmulatorTrace (Trace.ContractHandle (ContractResponse String Text Marketplace.UserContractState) Marketplace.MarketplaceUserSchema Void)
importNftTrace = do
  _ <- Start.startTrace
  h <- Trace.activateContractWallet Fixtures.userWallet $ Marketplace.userEndpoints Fixtures.marketplace
  _ <- Trace.callEndpoint @"importNft" h importNftParams
  _ <- Trace.waitNSlots 50
  pure h

importNftTrace' :: Trace.EmulatorTrace ()
importNftTrace' = do
  _ <- Start.startTrace
  h <- Trace.activateContractWallet Fixtures.userWallet $ Marketplace.userEndpoints Fixtures.marketplace
  _ <- Trace.callEndpoint @"importNft" h $ importNftParams & Marketplace._inpRevealIssuer .~ True
  _ <- Trace.waitNSlots 50
  pure ()

datumsCheck :: TracePredicate
datumsCheck =
  dataAtAddress
    Fixtures.marketplaceAddress
    (Utils.checkOneDatum (containsNft . Marketplace.mdSingletons))
    where
      containsNft = maybe False (const True) . AssocMap.lookup Fixtures.catTokenIpfsCidHash

datumsCheck' :: TracePredicate
datumsCheck' =
  dataAtAddress
    Fixtures.marketplaceAddress
    (Utils.checkOneDatum (containsNft . Marketplace.mdSingletons))
    where
      containsNft = maybe False (\t -> (t ^. Marketplace._nftLot & isNothing) &&
                                t ^. Marketplace._nftRecord . Marketplace._niIssuer == Nothing &&
                                (t ^. Marketplace._nftRecord & Fixtures.hasCatTokenRecord)) .
                    AssocMap.lookup Fixtures.catTokenIpfsCidHash

valueCheck :: TracePredicate
valueCheck =
  valueAtAddress
    (walletAddress Fixtures.userWallet)
    (Utils.one hasNft . V.flattenValue)
    where
      hasNft v = (v ^. _2 & V.unTokenName) == Fixtures.catTokenIpfsCidBs
