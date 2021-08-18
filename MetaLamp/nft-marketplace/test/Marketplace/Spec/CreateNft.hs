{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeApplications   #-}
module Marketplace.Spec.CreateNft
  ( tests, createNftTrace
  ) where

import           Control.Lens                                 (_2, (&), (.~),
                                                               (^.), (^?))
import qualified Control.Lens                                 as Lens
import           Control.Monad                                (void)
import qualified Data.Map                                     as Map
import           Data.Maybe                                   (isNothing)
import           Data.Text                                    (Text)
import           Data.Void
import           Ledger
import qualified Ledger.Ada                                   as Ada
import qualified Ledger.Value                                 as V
import qualified Marketplace.Fixtures                         as Fixtures
import qualified Marketplace.Spec.Start                       as Start
import           Plutus.Abstract.ContractResponse
import           Plutus.Contract
import           Plutus.Contract.Test
import qualified Plutus.Contracts.NftMarketplace.Endpoints    as Marketplace
import qualified Plutus.Contracts.NftMarketplace.OnChain.Core as Marketplace
import qualified Plutus.Trace                                 as Trace
import qualified PlutusTx.AssocMap                            as AssocMap
import           PlutusTx.Builtins                            (sha2_256)
import           PlutusTx.Prelude                             (ByteString)
import           Test.Tasty
import qualified Utils
import           Wallet.Emulator.Wallet

tests :: TestTree
tests =
  testGroup
    "createNft"
    [ checkPredicateOptions
        Fixtures.options
        "Should mint NFT token into the user wallet and create the Marketplace entry hiding issuer"
        (datumsCheck .&&. valueCheck)
        (void createNftTrace),
      checkPredicateOptions
        Fixtures.options
        "Should mint NFT token into the user wallet and create the Marketplace entry revealing issuer"
        (datumsCheck' .&&. valueCheck)
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

createNftTrace :: Trace.EmulatorTrace (Trace.ContractHandle (ContractResponse Text Marketplace.UserContractState) Marketplace.MarketplaceUserSchema Void)
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
    (containsNft . Marketplace.mdSingletons)
    where
      containsNft = maybe False (\t -> (t ^. Marketplace._nftLot & isNothing) &&
                                (t ^. Marketplace._nftRecord . Marketplace._niIssuer & isNothing) &&
                                (t ^. Marketplace._nftRecord & Fixtures.hasCatTokenRecord)) .
                    (AssocMap.lookup Fixtures.catTokenIpfsCidHash)

datumsCheck' :: TracePredicate
datumsCheck' =
  dataAtAddress
    Fixtures.marketplaceAddress
    (containsNft . Marketplace.mdSingletons)
    where
      containsNft = maybe False (\t -> (t ^. Marketplace._nftLot & isNothing) &&
                                (t ^. Marketplace._nftRecord . Marketplace._niIssuer == Just (Utils.walletPkh Fixtures.userWallet)) &&
                                (t ^. Marketplace._nftRecord & Fixtures.hasCatTokenRecord)) .
                    (AssocMap.lookup Fixtures.catTokenIpfsCidHash)

valueCheck :: TracePredicate
valueCheck =
  valueAtAddress
    (walletAddress Fixtures.userWallet)
    (Utils.one hasNft . V.flattenValue)
    where
      hasNft v = (v ^. _2 & V.unTokenName) == Fixtures.catTokenIpfsCid
