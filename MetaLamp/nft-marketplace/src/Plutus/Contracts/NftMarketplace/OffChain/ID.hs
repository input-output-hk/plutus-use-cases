{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeApplications    #-}

module Plutus.Contracts.NftMarketplace.OffChain.ID where

import           Control.Monad                                hiding (fmap)
import qualified Data.Aeson                                   as J
import           Data.Proxy                                   (Proxy (..))
import           Data.Text                                    (Text)
import qualified Data.Text                                    as T
import qualified GHC.Generics                                 as Haskell
import           Ledger
import qualified Ledger.Typed.Scripts                         as Scripts
import           Ledger.Value
import           Plutus.Abstract.ContractResponse             (ContractResponse,
                                                               withContractResponse)
import           Plutus.Contract
import           Plutus.Contract.StateMachine
import           Plutus.Contracts.Currency                    as Currency
import qualified Plutus.Contracts.NftMarketplace.OnChain.Core as Core
import qualified PlutusTx
import qualified PlutusTx.AssocMap                            as AssocMap
import           PlutusTx.Prelude                             hiding
                                                              (Semigroup (..))
import           Prelude                                      (Semigroup (..))
import qualified Prelude                                      as Haskell
import qualified Schema
import           Text.Printf                                  (printf)

data UserItemId = UserNftId Core.IpfsCid | UserBundleId [Core.IpfsCid]
    deriving stock    (Haskell.Eq, Haskell.Show, Haskell.Generic)
    deriving anyclass (J.ToJSON, J.FromJSON)

toInternalId :: UserItemId -> Either Core.InternalNftId Core.InternalBundleId
toInternalId (UserNftId ipfsCid) = Left
  Core.InternalNftId {
      Core.iniIpfsCidHash = sha2_256 ipfsCid,
      Core.iniIpfsCid = ipfsCid
    }
toInternalId (UserBundleId cids) = Right
  Core.InternalBundleId {
      Core.ibiIpfsCids = AssocMap.fromList $ (\cid -> (sha2_256 cid, cid)) <$> cids,
      Core.ibiBundleId = Core.calcBundleIdHash cids
    }
