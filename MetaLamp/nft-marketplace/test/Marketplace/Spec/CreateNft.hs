{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeApplications   #-}
module Marketplace.Spec.CreateNft
  ( tests
  ) where

import           Control.Lens                                 (_2, (&), (.~),
                                                               (^.), (^?))
import qualified Control.Lens                                 as Lens
import           Control.Monad                                (void)
import qualified Data.Map                                     as Map
import           Data.Maybe                                   (isNothing)
import           Data.Text                                    (Text)
import           Ledger
import qualified Ledger.Ada                                   as Ada
import qualified Ledger.Value                                 as V
import qualified Marketplace.Fixtures                         as Fixtures
import qualified Marketplace.Spec.Start                       as Start
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

-- TODO add tests on       cnpRevealIssuer
tests :: TestTree
tests =
  testGroup
    "createNft"
    [ checkPredicateOptions
        Fixtures.options
        "Should mint NFT token into the user wallet and create the Marketplace entry"
        (datumsCheck .&&. valueCheck)
        createNftTrace
    ]

createNftTrace :: Trace.EmulatorTrace ()
createNftTrace = do
  _ <- Start.startTrace
  h <- Trace.activateContractWallet Fixtures.userWallet $ Marketplace.userEndpoints Fixtures.marketplace
  _ <- Trace.callEndpoint @"createNft" h Marketplace.CreateNftParams {
                        Marketplace.cnpIpfsCid        = catTokenIpfsCid,
                        Marketplace.cnpNftName        = "Cat token",
                        Marketplace.cnpNftDescription = "A picture of a cat on a pogo stick",
                        Marketplace.cnpNftCategory = ["GIFs"],
                        Marketplace.cnpRevealIssuer   = False
                    }
  _ <- Trace.waitNSlots 50
  pure ()

datumsCheck :: TracePredicate
datumsCheck =
  dataAtAddress
    Fixtures.marketplaceAddress
    (containsNft . Marketplace.mdSingletons)
    where
      containsNft = maybe False (\t -> (t ^. Marketplace._nftLot & isNothing) && (t ^. Marketplace._nftRecord & hasCatTokenRecord)) .
                    (AssocMap.lookup catTokenIpfsCidHash)

valueCheck :: TracePredicate
valueCheck =
  valueAtAddress
    (walletAddress Fixtures.userWallet)
    (Utils.one hasNft . V.flattenValue)
    where
      hasNft v = (v ^. _2 & V.unTokenName) == catTokenIpfsCid

catTokenIpfsCid :: Marketplace.IpfsCid
catTokenIpfsCid = "QmPeoJnaDttpFrSySYBY3reRFCzL3qv4Uiqz376EBv9W16"

catTokenIpfsCidHash :: Marketplace.IpfsCidHash
catTokenIpfsCidHash = sha2_256 catTokenIpfsCid
catTokenName :: ByteString
catTokenName        = "Cat token"
catTokenDescription :: ByteString
catTokenDescription = "A picture of a cat on a pogo stick"
catTokenCategory :: Marketplace.Category
catTokenCategory = ["GIFs"]
hasCatTokenRecord  :: Marketplace.NftInfo -> Bool
hasCatTokenRecord Marketplace.NftInfo {..} = niCategory == catTokenCategory && niName == catTokenName && niDescription == catTokenDescription

photoTokenIpfsCid :: Marketplace.IpfsCid
photoTokenIpfsCid = "QmeSFBsEZ7XtK7yv5CQ79tqFnH9V2jhFhSSq1LV5W3kuiB"
