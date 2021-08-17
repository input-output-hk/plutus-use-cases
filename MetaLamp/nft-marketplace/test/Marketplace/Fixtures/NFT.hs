{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeApplications   #-}

module Marketplace.Fixtures.NFT where

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

catTokenIpfsCid :: Marketplace.IpfsCid
catTokenIpfsCid = "QmPeoJnaDttpFrSySYBY3reRFCzL3qv4Uiqz376EBv9W16"

catTokenIpfsCidHash :: Marketplace.IpfsCidHash
catTokenIpfsCidHash = sha2_256 catTokenIpfsCid

catTokenName :: ByteString
catTokenName = "Cat token"

catTokenDescription :: ByteString
catTokenDescription = "A picture of a cat on a pogo stick"

catTokenCategory :: Marketplace.Category
catTokenCategory = ["GIFs"]

hasCatTokenRecord :: Marketplace.NftInfo -> Bool
hasCatTokenRecord Marketplace.NftInfo {..} =
  niCategory == catTokenCategory &&
  niName == catTokenName &&
  niDescription == catTokenDescription

photoTokenIpfsCid :: Marketplace.IpfsCid
photoTokenIpfsCid = "QmeSFBsEZ7XtK7yv5CQ79tqFnH9V2jhFhSSq1LV5W3kuiB"
