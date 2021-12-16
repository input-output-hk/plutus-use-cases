{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE ViewPatterns          #-}

module Plutus.TestnetMVP.OffChain.ID where

import           Control.Monad                                          hiding
                                                                        (fmap)
import qualified Data.Aeson                                             as J
import           Data.Proxy                                             (Proxy (..))
import           Data.Text                                              (Text)
import qualified Data.Text                                              as T
import qualified GHC.Generics                                           as Haskell
import           Ledger
import qualified Ledger.Typed.Scripts                                   as Scripts
import           Ledger.Value
import           Plutus.Contract
import           Plutus.TestnetMVP.OffChain.Serialization (deserializeByteString)
import           Plutus.TestnetMVP.OnChain.ID        (InternalId (..), InternalNftId(..))
import qualified PlutusTx
import qualified PlutusTx.AssocMap                                      as AssocMap
import           PlutusTx.Prelude                                       hiding
                                                                        (Semigroup (..))
import           Prelude                                                (Semigroup (..))
import qualified Prelude                                                as Haskell
import qualified Schema
import           Text.Printf                                            (printf)

data UserItemId = UserNftId Text
    deriving stock    (Haskell.Eq, Haskell.Show, Haskell.Generic)
    deriving anyclass (J.ToJSON, J.FromJSON)

-- TODO remove ToSchema instances when constraint is removed from PAB
instance Schema.ToSchema UserItemId where
  toSchema = Schema.FormSchemaUnsupported "TODO how to make these instances for sum types?"

toInternalId :: UserItemId -> InternalId
toInternalId (UserNftId (deserializeByteString -> ipfsCid)) = NftInternalId
  InternalNftId {
      iniIpfsCidHash = sha2_256 ipfsCid,
      iniIpfsCid = ipfsCid
    }

